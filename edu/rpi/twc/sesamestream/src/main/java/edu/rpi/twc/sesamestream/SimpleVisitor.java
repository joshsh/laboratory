package edu.rpi.twc.sesamestream;

import org.openrdf.query.algebra.Add;
import org.openrdf.query.algebra.And;
import org.openrdf.query.algebra.ArbitraryLengthPath;
import org.openrdf.query.algebra.Avg;
import org.openrdf.query.algebra.BNodeGenerator;
import org.openrdf.query.algebra.BindingSetAssignment;
import org.openrdf.query.algebra.Bound;
import org.openrdf.query.algebra.Clear;
import org.openrdf.query.algebra.Coalesce;
import org.openrdf.query.algebra.Compare;
import org.openrdf.query.algebra.CompareAll;
import org.openrdf.query.algebra.CompareAny;
import org.openrdf.query.algebra.Copy;
import org.openrdf.query.algebra.Count;
import org.openrdf.query.algebra.Create;
import org.openrdf.query.algebra.Datatype;
import org.openrdf.query.algebra.DeleteData;
import org.openrdf.query.algebra.Difference;
import org.openrdf.query.algebra.Distinct;
import org.openrdf.query.algebra.EmptySet;
import org.openrdf.query.algebra.Exists;
import org.openrdf.query.algebra.Extension;
import org.openrdf.query.algebra.ExtensionElem;
import org.openrdf.query.algebra.Filter;
import org.openrdf.query.algebra.FunctionCall;
import org.openrdf.query.algebra.Group;
import org.openrdf.query.algebra.GroupConcat;
import org.openrdf.query.algebra.GroupElem;
import org.openrdf.query.algebra.IRIFunction;
import org.openrdf.query.algebra.If;
import org.openrdf.query.algebra.In;
import org.openrdf.query.algebra.InsertData;
import org.openrdf.query.algebra.Intersection;
import org.openrdf.query.algebra.IsBNode;
import org.openrdf.query.algebra.IsLiteral;
import org.openrdf.query.algebra.IsNumeric;
import org.openrdf.query.algebra.IsResource;
import org.openrdf.query.algebra.IsURI;
import org.openrdf.query.algebra.Join;
import org.openrdf.query.algebra.Label;
import org.openrdf.query.algebra.Lang;
import org.openrdf.query.algebra.LangMatches;
import org.openrdf.query.algebra.LeftJoin;
import org.openrdf.query.algebra.Like;
import org.openrdf.query.algebra.Load;
import org.openrdf.query.algebra.LocalName;
import org.openrdf.query.algebra.MathExpr;
import org.openrdf.query.algebra.Max;
import org.openrdf.query.algebra.Min;
import org.openrdf.query.algebra.Modify;
import org.openrdf.query.algebra.Move;
import org.openrdf.query.algebra.MultiProjection;
import org.openrdf.query.algebra.Namespace;
import org.openrdf.query.algebra.Not;
import org.openrdf.query.algebra.Or;
import org.openrdf.query.algebra.Order;
import org.openrdf.query.algebra.OrderElem;
import org.openrdf.query.algebra.Projection;
import org.openrdf.query.algebra.ProjectionElem;
import org.openrdf.query.algebra.ProjectionElemList;
import org.openrdf.query.algebra.QueryModelNode;
import org.openrdf.query.algebra.QueryModelVisitor;
import org.openrdf.query.algebra.QueryRoot;
import org.openrdf.query.algebra.Reduced;
import org.openrdf.query.algebra.Regex;
import org.openrdf.query.algebra.SameTerm;
import org.openrdf.query.algebra.Sample;
import org.openrdf.query.algebra.Service;
import org.openrdf.query.algebra.SingletonSet;
import org.openrdf.query.algebra.Slice;
import org.openrdf.query.algebra.StatementPattern;
import org.openrdf.query.algebra.Str;
import org.openrdf.query.algebra.Sum;
import org.openrdf.query.algebra.Union;
import org.openrdf.query.algebra.ValueConstant;
import org.openrdf.query.algebra.Var;
import org.openrdf.query.algebra.ZeroLengthPath;

import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SimpleVisitor implements QueryModelVisitor {
    private final List<QueryModelNode> visited;

    public SimpleVisitor(List<QueryModelNode> visited) {
        this.visited = visited;
    }

    public void meet(QueryRoot queryRoot) throws SimpleVisitorException {
        visited.add(queryRoot);
    }

    public void meet(Add add) throws SimpleVisitorException {
        visited.add(add);
    }

    public void meet(And and) throws SimpleVisitorException {
        visited.add(and);
    }

    public void meet(ArbitraryLengthPath arbitraryLengthPath) throws SimpleVisitorException {
        visited.add(arbitraryLengthPath);
    }

    public void meet(Avg avg) throws SimpleVisitorException {
        visited.add(avg);
    }

    public void meet(BindingSetAssignment bindingSetAssignment) throws SimpleVisitorException {
        visited.add(bindingSetAssignment);
    }

    public void meet(BNodeGenerator bNodeGenerator) throws SimpleVisitorException {
        visited.add(bNodeGenerator);
    }

    public void meet(Bound bound) throws SimpleVisitorException {
        visited.add(bound);
    }

    public void meet(Clear clear) throws SimpleVisitorException {
        visited.add(clear);
    }

    public void meet(Coalesce coalesce) throws SimpleVisitorException {
        visited.add(coalesce);
    }

    public void meet(Compare compare) throws SimpleVisitorException {
        visited.add(compare);
    }

    public void meet(CompareAll compareAll) throws SimpleVisitorException {
        visited.add(compareAll);
    }

    public void meet(CompareAny compareAny) throws SimpleVisitorException {
        visited.add(compareAny);
    }

    public void meet(Copy copy) throws SimpleVisitorException {
        visited.add(copy);
    }

    public void meet(Count count) throws SimpleVisitorException {
        visited.add(count);
    }

    public void meet(Create create) throws SimpleVisitorException {
        visited.add(create);
    }

    public void meet(Datatype datatype) throws SimpleVisitorException {
        visited.add(datatype);
    }

    public void meet(DeleteData deleteData) throws SimpleVisitorException {
        visited.add(deleteData);
    }

    public void meet(Difference difference) throws SimpleVisitorException {
        visited.add(difference);
    }

    public void meet(Distinct distinct) throws SimpleVisitorException {
        visited.add(distinct);
    }

    public void meet(EmptySet emptySet) throws SimpleVisitorException {
        visited.add(emptySet);
    }

    public void meet(Exists exists) throws SimpleVisitorException {
        visited.add(exists);
    }

    public void meet(Extension extension) throws SimpleVisitorException {
        visited.add(extension);
    }

    public void meet(ExtensionElem extensionElem) throws SimpleVisitorException {
        visited.add(extensionElem);
    }

    public void meet(Filter filter) throws SimpleVisitorException {
        visited.add(filter);
    }

    public void meet(FunctionCall functionCall) throws SimpleVisitorException {
        visited.add(functionCall);
    }

    public void meet(Group group) throws SimpleVisitorException {
        visited.add(group);
    }

    public void meet(GroupConcat groupConcat) throws SimpleVisitorException {
        visited.add(groupConcat);
    }

    public void meet(GroupElem groupElem) throws SimpleVisitorException {
        visited.add(groupElem);
    }

    public void meet(If anIf) throws SimpleVisitorException {
        visited.add(anIf);
    }

    public void meet(In in) throws SimpleVisitorException {
        visited.add(in);
    }

    public void meet(InsertData insertData) throws SimpleVisitorException {
        visited.add(insertData);
    }

    public void meet(Intersection intersection) throws SimpleVisitorException {
        visited.add(intersection);
    }

    public void meet(IRIFunction iriFunction) throws SimpleVisitorException {
        visited.add(iriFunction);
    }

    public void meet(IsBNode isBNode) throws SimpleVisitorException {
        visited.add(isBNode);
    }

    public void meet(IsLiteral isLiteral) throws SimpleVisitorException {
        visited.add(isLiteral);
    }

    public void meet(IsNumeric isNumeric) throws SimpleVisitorException {
        visited.add(isNumeric);
    }

    public void meet(IsResource isResource) throws SimpleVisitorException {
        visited.add(isResource);
    }

    public void meet(IsURI isURI) throws SimpleVisitorException {
        visited.add(isURI);
    }

    public void meet(Join join) throws SimpleVisitorException {
        visited.add(join);
    }

    public void meet(Label label) throws SimpleVisitorException {
        visited.add(label);
    }

    public void meet(Lang lang) throws SimpleVisitorException {
        visited.add(lang);
    }

    public void meet(LangMatches langMatches) throws SimpleVisitorException {
        visited.add(langMatches);
    }

    public void meet(LeftJoin leftJoin) throws SimpleVisitorException {
        visited.add(leftJoin);
    }

    public void meet(Like like) throws SimpleVisitorException {
        visited.add(like);
    }

    public void meet(Load load) throws SimpleVisitorException {
        visited.add(load);
    }

    public void meet(LocalName localName) throws SimpleVisitorException {
        visited.add(localName);
    }

    public void meet(MathExpr mathExpr) throws SimpleVisitorException {
        visited.add(mathExpr);
    }

    public void meet(Max max) throws SimpleVisitorException {
        visited.add(max);
    }

    public void meet(Min min) throws SimpleVisitorException {
        visited.add(min);
    }

    public void meet(Modify modify) throws SimpleVisitorException {
        visited.add(modify);
    }

    public void meet(Move move) throws SimpleVisitorException {
        visited.add(move);
    }

    public void meet(MultiProjection multiProjection) throws SimpleVisitorException {
        visited.add(multiProjection);
    }

    public void meet(Namespace namespace) throws SimpleVisitorException {
        visited.add(namespace);
    }

    public void meet(Not not) throws SimpleVisitorException {
        visited.add(not);
    }

    public void meet(Or or) throws SimpleVisitorException {
        visited.add(or);
    }

    public void meet(Order order) throws SimpleVisitorException {
        visited.add(order);
    }

    public void meet(OrderElem orderElem) throws SimpleVisitorException {
        visited.add(orderElem);
    }

    public void meet(Projection projection) throws SimpleVisitorException {
        visited.add(projection);
    }

    public void meet(ProjectionElem projectionElem) throws SimpleVisitorException {
        visited.add(projectionElem);
    }

    public void meet(ProjectionElemList projectionElemList) throws SimpleVisitorException {
        visited.add(projectionElemList);
    }

    public void meet(Reduced reduced) throws SimpleVisitorException {
        visited.add(reduced);
    }

    public void meet(Regex regex) throws SimpleVisitorException {
        visited.add(regex);
    }

    public void meet(SameTerm sameTerm) throws SimpleVisitorException {
        visited.add(sameTerm);
    }

    public void meet(Sample sample) throws SimpleVisitorException {
        visited.add(sample);
    }

    public void meet(Service service) throws SimpleVisitorException {
        visited.add(service);
    }

    public void meet(SingletonSet singletonSet) throws SimpleVisitorException {
        visited.add(singletonSet);
    }

    public void meet(Slice slice) throws SimpleVisitorException {
        visited.add(slice);
    }

    public void meet(StatementPattern statementPattern) throws SimpleVisitorException {
        visited.add(statementPattern);
    }

    public void meet(Str str) throws SimpleVisitorException {
        visited.add(str);
    }

    public void meet(Sum sum) throws SimpleVisitorException {
        visited.add(sum);
    }

    public void meet(Union union) throws SimpleVisitorException {
        visited.add(union);
    }

    public void meet(ValueConstant valueConstant) throws SimpleVisitorException {
        visited.add(valueConstant);
    }

    public void meet(Var var) throws SimpleVisitorException {
        visited.add(var);
    }

    public void meet(ZeroLengthPath zeroLengthPath) throws SimpleVisitorException {
        visited.add(zeroLengthPath);
    }

    public void meetOther(QueryModelNode queryModelNode) throws SimpleVisitorException {
        visited.add(queryModelNode);
    }

    public static class SimpleVisitorException extends Exception {
    }
}
