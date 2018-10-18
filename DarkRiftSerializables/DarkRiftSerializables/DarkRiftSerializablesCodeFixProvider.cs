using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.Formatting;

namespace DarkRiftSerializables {
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(DarkRiftSerializablesCodeFixProvider)), Shared]
    public class DarkRiftSerializablesCodeFixProvider : CodeFixProvider {
        private const string title = "Autogenerate 'Serialize' and 'Deserialize' methods";

        public sealed override ImmutableArray<string> FixableDiagnosticIds {
            get { return ImmutableArray.Create(DarkRiftSerializablesAnalyzer.DiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider() {
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;
            var declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<ClassDeclarationSyntax>().First();
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: title,
                    createChangedDocument: c => AutogenerateMethodsAsync(context.Document, declaration, c),
                    equivalenceKey: title),
                diagnostic);
        }

        public MethodDeclarationSyntax GetMethodDeclarationSyntax(string returnTypeName, string methodName, string[] parameterTypes, string[] paramterNames) {
            var parameterList = SyntaxFactory.ParameterList(SyntaxFactory.SeparatedList(GetParametersList(parameterTypes, paramterNames)));
            return SyntaxFactory.MethodDeclaration(attributeLists: SyntaxFactory.List<AttributeListSyntax>(),
                          modifiers: SyntaxFactory.TokenList(),
                          returnType: SyntaxFactory.ParseTypeName(returnTypeName),
                          explicitInterfaceSpecifier: null,
                          identifier: SyntaxFactory.Identifier(methodName),
                          typeParameterList: null,
                          parameterList: parameterList,
                          constraintClauses: SyntaxFactory.List<TypeParameterConstraintClauseSyntax>(),
                          body: SyntaxFactory.Block(
                          ),
                          semicolonToken: SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                  // Annotate that this node should be formatted
                  .WithAdditionalAnnotations(Formatter.Annotation)
                  .WithModifiers(
                    SyntaxFactory.TokenList(
                      SyntaxFactory.Token(SyntaxKind.PublicKeyword)
                    )
                  );
        }

        private IEnumerable<ParameterSyntax> GetParametersList(string[] parameterTypes, string[] paramterNames) {
            for (var i = 0; i < parameterTypes.Length; i++) {
                yield return SyntaxFactory.Parameter(attributeLists: SyntaxFactory.List<AttributeListSyntax>(),
                                                     modifiers: SyntaxFactory.TokenList(),
                                                     type: SyntaxFactory.ParseTypeName(parameterTypes[i]),
                                                     identifier: SyntaxFactory.Identifier(paramterNames[i]),
                                                     @default: null);
            }
        }

        private ExpressionStatementSyntax GetInvocationExpression(string member) {
            return SyntaxFactory.ExpressionStatement(
                     SyntaxFactory.InvocationExpression(
                       SyntaxFactory.MemberAccessExpression(
                         SyntaxKind.SimpleMemberAccessExpression,
                         SyntaxFactory.IdentifierName("e.Writer"),
                         SyntaxFactory.IdentifierName("Write")
                       ).WithOperatorToken(SyntaxFactory.Token(SyntaxKind.DotToken)),
                       SyntaxFactory.ArgumentList(
                         SyntaxFactory.SingletonSeparatedList<ArgumentSyntax>(
                           SyntaxFactory.Argument(
                             SyntaxFactory.MemberAccessExpression(
                               SyntaxKind.SimpleMemberAccessExpression,
                               SyntaxFactory.IdentifierName("this"),
                               SyntaxFactory.IdentifierName(member)
                             ).WithOperatorToken(SyntaxFactory.Token(SyntaxKind.DotToken))
                           )
                         )
                       ).WithOpenParenToken(
                         SyntaxFactory.Token(
                           SyntaxKind.OpenParenToken
                         )
                       ).WithCloseParenToken(
                         SyntaxFactory.Token(
                           SyntaxKind.CloseParenToken
                         )
                       )
                     )
                   );
        }

        private ExpressionStatementSyntax GetReadExpression(string member, string readMethod) {
            return SyntaxFactory.ExpressionStatement(
                     SyntaxFactory.AssignmentExpression(
                         SyntaxKind.SimpleAssignmentExpression,
                       SyntaxFactory.MemberAccessExpression(
                         SyntaxKind.SimpleMemberAccessExpression,
                         SyntaxFactory.IdentifierName("this"),
                         SyntaxFactory.IdentifierName(member)
                       ).WithOperatorToken(SyntaxFactory.Token(SyntaxKind.DotToken)),
                       SyntaxFactory.Token(SyntaxKind.EqualsToken),
                       SyntaxFactory.InvocationExpression(
                       SyntaxFactory.MemberAccessExpression(
                         SyntaxKind.SimpleMemberAccessExpression,
                         SyntaxFactory.IdentifierName("e.Reader"),
                         SyntaxFactory.IdentifierName(readMethod)
                       ).WithOperatorToken(SyntaxFactory.Token(SyntaxKind.DotToken)),
                       SyntaxFactory.ArgumentList(
                       ).WithOpenParenToken(
                         SyntaxFactory.Token(
                           SyntaxKind.OpenParenToken
                         )
                       ).WithCloseParenToken(
                         SyntaxFactory.Token(
                           SyntaxKind.CloseParenToken
                         )
                       )
                     )
                     )
                   );
        }

        private string GetReadMethod(TypeSyntax typeSyntax, SemanticModel semanticModel) {
            var type = "";
            if (typeSyntax is PredefinedTypeSyntax predefinedType) {
                type = predefinedType.Keyword.Text;
            } else if (typeSyntax is IdentifierNameSyntax identifierType) {
                if (semanticModel.GetTypeInfo(identifierType).Type.AllInterfaces.Any(i => i.Name == "IDarkRiftSerializable")) {
                    return "ReadSerializable<" + identifierType.Identifier.Text + ">";
                }
                type = identifierType.Identifier.Text;
            } else if (typeSyntax is ArrayTypeSyntax arrayType) {
                return this.GetReadMethod(arrayType.ElementType, semanticModel) + "s";
            }
            switch (type) {
                case "bool":
                    return "ReadBoolean";
                case "bool[]":
                    return "ReadBooleans";
                case "byte":
                    return "ReadByte";
                case "byte[]":
                    return "ReadBytes";
                case "sbyte":
                    return "ReadSByte";
                case "sbyte[]":
                    return "ReadSBytes";
                case "char":
                    return "ReadChar";
                case "char[]":
                    return "ReadChars";
                case "double":
                    return "ReadDouble";
                case "double[]":
                    return "ReadDoubles";
                case "short":
                case "Int16":
                    return "ReadInt16";
                case "short[]":
                case "Int16[]":
                    return "ReadInt16s";
                case "int":
                case "Int32":
                    return "ReadInt32";
                case "int[]":
                case "Int32[]":
                    return "ReadInt32s";
                case "long":
                case "Int64":
                    return "ReadInt64";
                case "long[]":
                case "Int64[]":
                    return "ReadInt64s";
                case "float":
                    return "ReadSingle";
                case "float[]":
                    return "ReadSingles";
                case "string":
                    return "ReadString";
                case "string[]":
                    return "ReadStrings";
                case "ushort":
                case "UInt16":
                    return "ReadUInt16";
                case "ushort[]":
                case "UInt16[]":
                    return "ReadUInt16s";
                case "uint":
                case "UInt32":
                    return "ReadUInt32";
                case "uint[]":
                case "UInt32[]":
                    return "ReadUInt32s";
                case "ulong":
                case "UInt64":
                    return "ReadUInt64";
                case "ulong[]":
                case "UInt64[]":
                    return "ReadUInt64s";
                default:
                    return "ReadUnsupported";
            }
        }

        private async Task<Document> AutogenerateMethodsAsync(Document document, ClassDeclarationSyntax serializableClass, CancellationToken cancellationToken) {
            var root = await document.GetSyntaxRootAsync().ConfigureAwait(false);
            ClassDeclarationSyntax newClass = null;
            if (!serializableClass.Members.Where(m => m is MethodDeclarationSyntax).Any(m => ((MethodDeclarationSyntax)m).Identifier.Text == "Serialize")) {
                var serializeMethod = GetMethodDeclarationSyntax(returnTypeName: "void",
                                                                methodName: "Serialize",
                                                                parameterTypes: new[] { "SerializeEvent" },
                                                                paramterNames: new[] { "e" });
                foreach (var member in serializableClass.Members.Where(m => m is PropertyDeclarationSyntax).Select(m => (PropertyDeclarationSyntax)m)) {
                    serializeMethod = serializeMethod.AddBodyStatements(this.GetInvocationExpression(member.Identifier.Text));
                }
                foreach (var field in serializableClass.Members.Where(m => m is FieldDeclarationSyntax).Select(f => (FieldDeclarationSyntax)f)) {
                    serializeMethod = serializeMethod.AddBodyStatements(this.GetInvocationExpression(field.Declaration.Variables.First().Identifier.Text));
                }
                newClass = serializableClass.AddMembers(serializeMethod);
            }
            if (!serializableClass.Members.Where(m => m is MethodDeclarationSyntax).Any(m => ((MethodDeclarationSyntax)m).Identifier.Text == "Deserialize")) {
                var semanticModel = await document.GetSemanticModelAsync();
                var deserializeMethod = GetMethodDeclarationSyntax(returnTypeName: "void",
                                                                methodName: "Deserialize",
                                                                parameterTypes: new[] { "DeserializeEvent" },
                                                                paramterNames: new[] { "e" });
                foreach (var member in serializableClass.Members) {
                    if (member is FieldDeclarationSyntax field) {
                        var readMethod = this.GetReadMethod(field.Declaration.Type, semanticModel);
                        deserializeMethod = deserializeMethod.AddBodyStatements(this.GetReadExpression(field.Declaration.Variables.First().Identifier.Text, readMethod));
                    } else if (member is PropertyDeclarationSyntax property) {
                        var readMethod = this.GetReadMethod(property.Type, semanticModel);
                        deserializeMethod = deserializeMethod.AddBodyStatements(this.GetReadExpression(property.Identifier.Text, readMethod));
                    }
                }
                if (newClass != null) {
                    newClass = newClass.AddMembers(deserializeMethod);
                } else {
                    newClass = serializableClass.AddMembers(deserializeMethod);
                }
            }
            if (newClass != null) {
                root = root.ReplaceNode(serializableClass, newClass);
                root = Formatter.Format(root, Formatter.Annotation, document.Project.Solution.Workspace);
            }
            return document.WithSyntaxRoot(root);
        }
    }
}
