package ocl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.eclipse.emf.ecore.EObject;

import tudresden.ocl20.pivot.essentialocl.expressions.CollectionLiteralExp;
import tudresden.ocl20.pivot.essentialocl.expressions.CollectionRange;
import tudresden.ocl20.pivot.essentialocl.expressions.ExpressionInOcl;
import tudresden.ocl20.pivot.essentialocl.expressions.IfExp;
import tudresden.ocl20.pivot.essentialocl.expressions.OclExpression;
import tudresden.ocl20.pivot.essentialocl.expressions.OperationCallExp;
import tudresden.ocl20.pivot.essentialocl.expressions.PropertyCallExp;
import tudresden.ocl20.pivot.essentialocl.expressions.UndefinedLiteralExp;
import tudresden.ocl20.pivot.essentialocl.expressions.Variable;
import tudresden.ocl20.pivot.essentialocl.expressions.VariableExp;
import tudresden.ocl20.pivot.essentialocl.expressions.impl.BooleanLiteralExpImpl;
import tudresden.ocl20.pivot.essentialocl.expressions.impl.CollectionRangeImpl;
import tudresden.ocl20.pivot.essentialocl.expressions.impl.IntegerLiteralExpImpl;
import tudresden.ocl20.pivot.essentialocl.expressions.impl.IterateExpImpl;
import tudresden.ocl20.pivot.essentialocl.expressions.impl.IteratorExpImpl;
import tudresden.ocl20.pivot.essentialocl.expressions.impl.TypeLiteralExpImpl;
import tudresden.ocl20.pivot.pivotmodel.Constraint;
import util.Indenter;


public class OCLGen {

    private static OCLGen instance;

    private static Indenter indenter = Indenter.getInstance();

    private OCLGen() {
        ;
    }

    public static OCLGen getInstance() {
        if(instance == null) {
            instance = new OCLGen();
        }
        return instance;
    }

    public String toStr(Collection<Constraint> constraints,
            boolean showPreconditions, boolean showPostconditions,
            boolean showInvariants) throws Exception {

        StringBuffer sb = new StringBuffer();


        sb.append("[");
        sb.append("\n");
        sb.append(indenter.indent(4));

        if(showPreconditions) {
            sb.append(toStr(constraints, "precondition"));

            if(showPostconditions || showInvariants) {
                sb.append(",");
                sb.append("\n");
                sb.append(indenter.indent());
            }
        }

        if(showPostconditions) {
            sb.append(toStr(constraints, "postcondition"));

            if(showInvariants) {
                sb.append(",");
                sb.append("\n");
                sb.append(indenter.indent());
            }
        }

        if(showInvariants) {
            sb.append(toStr(constraints, "invariant"));
        }

        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]");

        return sb.toString();
    }

    public String toStr(Collection<Constraint> constraints) throws Exception {
        return toStr(constraints, true, true, true);
    }

    private String toStr(Collection<Constraint> constraints, String constraintType)
            throws Exception {

        StringBuffer sb = new StringBuffer();

        sb.append("{");
        sb.append(constraintType + "s");
        sb.append(", [");

        Collection<Constraint> filteredConstraints = new ArrayList<Constraint>();
        for(Constraint constraint: constraints) {
            if(constraintType.equals(constraint.getKind().getName())) {
                filteredConstraints.add(constraint);
            }
        }

        if(!filteredConstraints.isEmpty()) {
            sb.append("\n");
            sb.append(indenter.indent(4));
        }

        for(Iterator<Constraint> itConstraint = filteredConstraints.iterator();
                itConstraint.hasNext();) {

            Constraint constraint = itConstraint.next();

            sb.append(toStr(constraint));

            if(itConstraint.hasNext()) {
                sb.append(",");
                sb.append("\n");
                sb.append(indenter.indent());
            }

        }

        if(!filteredConstraints.isEmpty()) {
            sb.append("\n");
            sb.append(indenter.indent(-4));
        }
        
        sb.append("]}");

        return sb.toString();
    }

    private String toStr(Constraint constraint) throws Exception {
        StringBuffer sb = new StringBuffer();

        ExpressionInOcl specification =
                (ExpressionInOcl) constraint.getSpecification();

        OclExpression bodyExpression =
                specification.getBodyExpression();

        sb.append("{constraint, [");
        sb.append("\n");
        sb.append(indenter.indent(4));

        sb.append("{name, ");
        sb.append(toString(constraint.getName()));
        sb.append("},");
        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{body, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        if(bodyExpression instanceof OperationCallExp) {
            sb.append(toStr((OperationCallExp)bodyExpression));
        } else if(bodyExpression instanceof IfExp) {
            sb.append(toStr((IfExp)bodyExpression));
        } else if(bodyExpression instanceof IteratorExpImpl) {
            sb.append(toStr((IteratorExpImpl)bodyExpression));
        } else {
            throw new Exception(bodyExpression.getClass().getName());
        }
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}");

        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}");

        return sb.toString();
    }

    private String toStr(EObject eObject)
            throws Exception {

        StringBuffer sb = new StringBuffer();

        if(eObject instanceof VariableExp) {
            sb.append(toStr((VariableExp)eObject));
        } else if(eObject instanceof Variable) {
            sb.append(toStr((Variable)eObject));
        } else if(eObject instanceof PropertyCallExp) {
            sb.append(toStr((PropertyCallExp)eObject));
        } else if(eObject instanceof OperationCallExp) {
            sb.append(toStr((OperationCallExp)eObject));
        } else if(eObject instanceof CollectionLiteralExp) {
            sb.append(toStr((CollectionLiteralExp)eObject));
        } else if(eObject instanceof CollectionRangeImpl) {
            sb.append(toStr((CollectionRangeImpl)eObject));
        } else if(eObject instanceof UndefinedLiteralExp) {
            sb.append(toStr((UndefinedLiteralExp)eObject));
        } else if(eObject instanceof BooleanLiteralExpImpl) {
            sb.append(toStr((BooleanLiteralExpImpl)eObject));
        } else if(eObject instanceof IntegerLiteralExpImpl) {
            sb.append(toStr((IntegerLiteralExpImpl)eObject));
        } else if(eObject instanceof TypeLiteralExpImpl) {
            sb.append(toStr((TypeLiteralExpImpl)eObject));
        } else if(eObject instanceof IfExp) {
            sb.append(toStr((IfExp)eObject));
        } else if(eObject instanceof IteratorExpImpl) {
            sb.append(toStr((IteratorExpImpl)eObject));
        } else if(eObject instanceof IterateExpImpl) {
            sb.append(toStr((IterateExpImpl)eObject));
        } else {
            throw new Exception(eObject.getClass().getName());
        }

        return sb.toString();
    }

    private String toStr(OperationCallExp operationCallExp)
            throws Exception {

        StringBuffer sb = new StringBuffer();

        sb.append("{expression, \"OperationCallExp\"},");
        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{name, ");
        sb.append(toString(operationCallExp.getName()));
        sb.append("},");

        sb.append("\n");
        sb.append(indenter.indent());
        
        if(operationCallExp.getReferredOperation() != null) {
            sb.append("{referredOperation, [");
            sb.append("\n");
            sb.append(indenter.indent(4));


            sb.append("{name, ");
            sb.append(toString(operationCallExp.getReferredOperation().getName()));
            sb.append("},");

            sb.append("\n");
            sb.append(indenter.indent());
            
            sb.append("{type, [");
            sb.append("\n");
            sb.append(indenter.indent(4));
            
            sb.append("{name, ");
            sb.append(toString(
                    operationCallExp.getReferredOperation().getType().getName()));
            sb.append("},");
            
            sb.append("\n");
            sb.append(indenter.indent());
            
            sb.append("{qualifiedName, ");
            sb.append(toString(
                    operationCallExp.getReferredOperation().getType().getQualifiedName()));
            sb.append("}");
            
            sb.append("\n");
            sb.append(indenter.indent(-4));
            sb.append("]}");
            
            sb.append("\n");
            sb.append(indenter.indent(-4));
            sb.append("]},");

            sb.append("\n");
            sb.append(indenter.indent());
        } else {
            sb.append("{referredOperation, [");
            sb.append("\n");
            sb.append(indenter.indent(4));


            sb.append("{name, ");
            sb.append(toString(operationCallExp.getName()));
            sb.append("},");

            sb.append("\n");
            sb.append(indenter.indent());
            
            sb.append("{type, [");
            sb.append("\n");
            sb.append(indenter.indent(4));
            
            sb.append("{name, ");
            sb.append(toString(
                    operationCallExp.getType().getName()));
            sb.append("},");
            
            sb.append("\n");
            sb.append(indenter.indent());
            
            sb.append("{qualifiedName, ");
            sb.append(toString(
                    operationCallExp.getType().getQualifiedName()));
            sb.append("}");
            
            sb.append("\n");
            sb.append(indenter.indent(-4));
            sb.append("]}");
            
            sb.append("\n");
            sb.append(indenter.indent(-4));
            sb.append("]},");

            sb.append("\n");
            sb.append(indenter.indent());
        }

        sb.append("{contents, [");
        sb.append("\n");
        sb.append(indenter.indent(4));

        for(Iterator<EObject> itContents = operationCallExp.eContents().iterator();
                itContents.hasNext();) {

            sb.append("{content, [");
            sb.append("\n");
            sb.append(indenter.indent(4));

            sb.append(toStr(itContents.next()));

            sb.append("\n");
            sb.append(indenter.indent(-4));
            sb.append("]}");

            if(itContents.hasNext()) {
                sb.append(",");
                sb.append("\n");
                sb.append(indenter.indent());
            }
        }

        sb.append("\n");
        sb.append(indenter.indent(-4));

        sb.append("]}");

        return sb.toString();
    }

    private String toStr(VariableExp variableExp) throws Exception {
        StringBuffer sb = new StringBuffer();

        sb.append("{expression, \"VariableExp\"},");
        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{referredVariable, [");
        sb.append("\n");
        sb.append(indenter.indent(4));

        sb.append(toStr(variableExp.getReferredVariable()));
        
        sb.append("\n");
        sb.append(indenter.indent(-4));

        sb.append("]}");

        return sb.toString();
    }

    private String toStr(Variable variable) throws Exception {
        StringBuffer sb = new StringBuffer();

        sb.append("{expression, \"Variable\"},");
        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{name, ");
        sb.append(toString(variable.getQualifiedName()));
        sb.append("},");

        sb.append("\n");
        sb.append(indenter.indent());
        
        sb.append("{type, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        
        sb.append("{name, ");
        sb.append(toString(
                variable.getType().getName()));
        sb.append("},");
        
        sb.append("\n");
        sb.append(indenter.indent());
        
        sb.append("{qualifiedName, ");
        sb.append(toString(
                variable.getType().getQualifiedName()));
        sb.append("}");
        
        if(variable.getInitExpression() != null) {
            sb.append(",");
            sb.append("\n");
            sb.append(indenter.indent());
            
            sb.append("{initExpression, [");
            sb.append("\n");
            sb.append(indenter.indent(4));
            sb.append(toStr(variable.getInitExpression()));
            sb.append("\n");
            sb.append(indenter.indent(-4));
            sb.append("]}");
        }
        
        sb.append("\n");
        sb.append(indenter.indent(-4));
        
        sb.append("]}");

        return sb.toString();
    }
    
    private String toStr(PropertyCallExp propertyCallExp)
            throws Exception {

        StringBuffer sb = new StringBuffer();

        sb.append("{expression, \"PropertyCallExp\"},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{source, [");
        sb.append("\n");
        sb.append(indenter.indent(4));

        sb.append(toStr(propertyCallExp.getSource()));

        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}");
        sb.append(",");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{referredProperty, [");
        sb.append("\n");
        sb.append(indenter.indent(4));

        sb.append("{name, ");
        sb.append(toString(propertyCallExp.getReferredProperty().getName()));
        sb.append("},");

        sb.append("\n");
        sb.append(indenter.indent());
        
        sb.append("{type, [");
        sb.append("\n");
        sb.append(indenter.indent(4));

        sb.append("{name, ");
        sb.append(toString(
                propertyCallExp.getReferredProperty().getType().getName()));
        sb.append("},");
        
        sb.append("\n");
        sb.append(indenter.indent());
        
        sb.append("{qualifiedName, ");
        sb.append(toString(
                propertyCallExp.getReferredProperty().getType().getQualifiedName()));
        sb.append("}");
        
        sb.append("\n");
        sb.append(indenter.indent(-4));
        
        sb.append("]}");
        
        sb.append("\n");
        sb.append(indenter.indent(-4));

        sb.append("]}");

        return sb.toString();
    }

    private String toStr(CollectionLiteralExp collectionLiteralExp)
            throws Exception {
        
        StringBuffer sb = new StringBuffer();

        sb.append("{expression, \"CollectionLiteralExp\"},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{kind, ");
        sb.append(toString(collectionLiteralExp.getKind().getName()));
        sb.append("}");
        
        for(Iterator<EObject> itContents = collectionLiteralExp.eContents().iterator();
                itContents.hasNext();) {
            
            sb.append(",");
            sb.append("\n");
            sb.append(indenter.indent());
            
            EObject eObject = itContents.next();
            if(eObject instanceof CollectionRange) {
                sb.append(toStr((CollectionRange)eObject));
            } else {
                throw new Exception(eObject.getClass().getName());
            }

        }
        
        return sb.toString();
    }

    private String toStr(CollectionRange collectionRange)
            throws Exception {
        
        StringBuffer sb = new StringBuffer();

        sb.append("{range, [");
        
        sb.append("\n");
        sb.append(indenter.indent(4));
        
        sb.append("{first, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        sb.append(toStr(collectionRange.getFirst()));
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}, ");
        
        sb.append("\n");
        sb.append(indenter.indent());
        
        sb.append("{last, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        sb.append(toStr(collectionRange.getLast()));
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}");
        
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}");
        
        return sb.toString();
    }
    
    private String toStr(UndefinedLiteralExp undefinedLiteralExp) {
        StringBuffer sb = new StringBuffer();

        sb.append("{expression, \"UndefinedLiteralExp\"},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{value, ");
        sb.append("null");
        sb.append("}");

        return sb.toString();
    }

    private String toStr(BooleanLiteralExpImpl booleanLiteralExp)
            throws Exception {
        StringBuffer sb = new StringBuffer();
        
        sb.append("{expression, \"BooleanLiteralExp\"},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{value, ");
        sb.append(booleanLiteralExp.isBooleanSymbol());
        sb.append("}");
        
        return sb.toString();
    }
    
    private String toStr(IntegerLiteralExpImpl integerLiteralExp)
            throws Exception {
        StringBuffer sb = new StringBuffer();
        
        sb.append("{expression, \"IntegerLiteralExp\"},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{value, ");
        sb.append(integerLiteralExp.getIntegerSymbol());
        sb.append("}");
        
        return sb.toString();
    }
    
    private String toStr(TypeLiteralExpImpl typeLiteralExp)
            throws Exception {
        StringBuffer sb = new StringBuffer();
        
        sb.append("{expression, \"TypeLiteralExp\"},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{value, ");
        sb.append(toString(typeLiteralExp.getReferredType().getName()));
        sb.append("}");
        
        return sb.toString();
    }
    
    private String toStr(IteratorExpImpl iteratorExp) throws Exception {
        StringBuffer sb = new StringBuffer();
        
        sb.append("{expression, \"IteratorExpImpl\"},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{name, ");
        sb.append(toString(iteratorExp.getName()));
        sb.append("},");
        
        sb.append("\n");
        sb.append(indenter.indent());
        
        sb.append("{iterator, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        for(Iterator<Variable> itVariables =iteratorExp.getIterator().iterator();
                itVariables.hasNext();) {
            Variable variable = itVariables.next();
            sb.append("{variable, [");
            sb.append("\n");
            sb.append(indenter.indent(4));
            sb.append(toStr(variable));
            sb.append("\n");
            sb.append(indenter.indent(-4));
            sb.append("]}");
            if(itVariables.hasNext()) {
                sb.append(",");
                sb.append("\n");
                sb.append(indenter.indent());
            }
        }
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]},");
        
        sb.append("{source, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        sb.append(toStr(iteratorExp.getSource()));
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{body, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        sb.append(toStr(iteratorExp.getBody()));
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}");

        return sb.toString();
    }
    
    private String toStr(IterateExpImpl iterateExp) throws Exception {
        StringBuffer sb = new StringBuffer();

        sb.append("{expression, \"IterateExpImpl\"},");
        
        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{name, ");
        sb.append(toString(iterateExp.getName()));
        sb.append("},");
        
        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{iterator, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        for(Iterator<Variable> itVariables =iterateExp.getIterator().iterator();
                itVariables.hasNext();) {
            Variable variable = itVariables.next();
            sb.append("{variable, [");
            sb.append("\n");
            sb.append(indenter.indent(4));
            sb.append(toStr(variable));
            sb.append("\n");
            sb.append(indenter.indent(-4));
            sb.append("]}");
            if(itVariables.hasNext()) {
                sb.append(",");
                sb.append("\n");
                sb.append(indenter.indent());
            }
        }
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]},");
        
        sb.append("\n");
        sb.append(indenter.indent());
        
        sb.append("{result, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        sb.append(toStr(iterateExp.getResult()));
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]},");

        sb.append("\n");
        sb.append(indenter.indent());
        
        sb.append("{source, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        sb.append(toStr(iterateExp.getSource()));
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{body, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        sb.append(toStr(iterateExp.getBody()));
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}");
        
        return sb.toString();
    }
    
    private String toStr(IfExp ifExp) throws Exception {
        StringBuffer sb = new StringBuffer();

        sb.append("{expression, \"IfExp\"},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{condition, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        sb.append(toStr(ifExp.getCondition()));
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{then, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        sb.append(toStr(ifExp.getThenExpression()));
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]},");

        sb.append("\n");
        sb.append(indenter.indent());

        sb.append("{else, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        sb.append(toStr(ifExp.getElseExpression()));
        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}");

        return sb.toString();
    }

    private String toString(String str) {
        StringBuffer sb = new StringBuffer();
        sb.append("\"");
        sb.append(str);
        sb.append("\"");
        return sb.toString();
    }

}
