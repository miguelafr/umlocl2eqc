package uml;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import ocl.OCLGen;
import ocl.OCLUtils;
import tudresden.ocl20.pivot.metamodels.uml2.internal.model.UML2Class;
import tudresden.ocl20.pivot.metamodels.uml2.internal.model.UML2Operation;
import tudresden.ocl20.pivot.model.IModel;
import tudresden.ocl20.pivot.pivotmodel.Constraint;
import tudresden.ocl20.pivot.pivotmodel.Operation;
import tudresden.ocl20.pivot.pivotmodel.Parameter;
import tudresden.ocl20.pivot.pivotmodel.Property;
import util.Indenter;


public class UMLGen {

    private static UMLGen instance;
    
    private static Indenter indenter = Indenter.getInstance();
    private static UMLUtils umlUtils = UMLUtils.getInstance();
    private static OCLGen oclGen = OCLGen.getInstance();
    private static OCLUtils oclUtils = OCLUtils.getInstance();
    
    private UMLGen() {
        ;
    }

    public static UMLGen getInstance() {
        if(instance == null) {
            instance = new UMLGen();
        }
        return instance;
    }

    public String toStr(IModel model, List<Constraint> constraints)
            throws Exception {

        Map<UML2Class, Collection<Constraint>> constraintsByClass =
                oclUtils.getContraintsByClass(model, constraints);
        
        Map<UML2Operation, Collection<Constraint>> constraintsByOp =
                oclUtils.getContraintsByOp(model, constraints);
        
        StringBuffer sb = new StringBuffer();
        sb.append("[");
        sb.append("\n");
        sb.append(indenter.indent(4));

        /*
         * Classes
         */
        for(Iterator<UML2Class> itClass = umlUtils.getClasses(model).iterator();
                itClass.hasNext();) {

            UML2Class uml2Class = itClass.next();

            sb.append("{class, [");

            sb.append("\n");
            sb.append(indenter.indent(4));

            sb.append("{name, ");
            sb.append(toString(uml2Class.getName()));
            sb.append("},");

            sb.append("\n");
            sb.append(indenter.indent());

            sb.append("{constraints,");
            if(constraintsByClass.containsKey(uml2Class)) {
                sb.append(oclGen.toStr(constraintsByClass.get(uml2Class),
                        false, false, true));
            } else {
                sb.append(oclGen.toStr(new ArrayList<Constraint>(),
                        false, false, true));
            }
            sb.append("},");
            sb.append("\n");
            sb.append(indenter.indent());
            
            /*
             * Attributes
             */
            sb.append(toStrProperties(uml2Class.getOwnedProperty()));
            
            /*
             * Operations
             */
            sb.append(toStrOperations(uml2Class.getOwnedOperation(),
                    constraintsByOp));
            
            sb.append("]}");
            
            if(itClass.hasNext()) {
                sb.append(",");
                sb.append("\n");
                sb.append(indenter.indent());
            } else {
                sb.append("\n");
                sb.append(indenter.indent(-4));
            }

        }
        sb.append("].");

        return sb.toString();
    }

    private String toStrProperties(List<Property> properties)
    throws Exception {
        StringBuffer sb = new StringBuffer();
        
        sb.append("{attributes, [");
        
        if(properties.size() > 0) {
            sb.append("\n");
            sb.append(indenter.indent(4));
        }
        
        for(Iterator<Property> itProperty = properties.iterator();
                itProperty.hasNext();) {
            Property property = itProperty.next();
            sb.append(toStrProperty(property));
            
            if(itProperty.hasNext()) {
                sb.append(",");
                sb.append("\n");
                sb.append(indenter.indent());
            } else {
                sb.append("\n");
                sb.append(indenter.indent(-4));
            }
            
        }
        
        sb.append("]},");
        sb.append("\n");
        sb.append(indenter.indent());
        
        return sb.toString();
    }
    
    private String toStrProperty(Property property) {
        StringBuffer sb = new StringBuffer();
        
        sb.append("{attribute, [");

        sb.append("\n");
        sb.append(indenter.indent(4));

        /*
         * Parameter name
         */
        sb.append("{name, ");
        sb.append(toString(property.getName()));
        sb.append("},");

        sb.append("\n");
        sb.append(indenter.indent());

        /*
         * Parameter type
         */
        sb.append("{type, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        
        sb.append("{name, ");
        sb.append(toString(property.getType().getName()));
        sb.append("},");
        
        sb.append("\n");
        sb.append(indenter.indent());
        
        sb.append("{qualifiedName, ");
        sb.append(toString(property.getType().getQualifiedName()));
        sb.append("}");

        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}");

        sb.append("\n");
        sb.append(indenter.indent(-4));

        sb.append("]}");

        return sb.toString();
    }
    
    private String toStrOperations(List<Operation> operations,
            Map<UML2Operation, Collection<Constraint>> constraintsByOp)
    throws Exception {

        StringBuffer sb = new StringBuffer();
        
        sb.append("{operations, [");

        if(operations.size() > 0) {
            sb.append("\n");
            sb.append(indenter.indent(4));
        }

        for(Iterator<Operation> itOperation = operations.iterator();
                itOperation.hasNext();) {

            Operation operation = itOperation.next();
            Collection<Constraint> constraints = new ArrayList<Constraint>();
            
            if(constraintsByOp.containsKey(operation)) {
                constraints = constraintsByOp.get(operation);
            }
            
            sb.append(toStrOperation(operation, constraints));

            sb.append("]}");

            if(itOperation.hasNext()) {
                sb.append(",");
                sb.append("\n");
                sb.append(indenter.indent());
            } else {
                sb.append("\n");
                sb.append(indenter.indent(-4));
            }
            
        }
        sb.append("]}");

        sb.append("\n");
        sb.append(indenter.indent(-4));
        
        return sb.toString();
    }
    
    private String toStrOperation(Operation operation,
            Collection<Constraint> constraints) throws Exception {
        
        StringBuffer sb = new StringBuffer();
        
        sb.append("{operation, [");
        sb.append("\n");
        sb.append(indenter.indent(4));

        /*
         * Operation name
         */
        sb.append("{name, ");
        sb.append(toString(operation.getName()));
        sb.append("},");

        sb.append("\n");
        sb.append(indenter.indent());

        /*
         * Parameters
         */
        sb.append(toStrParameters(operation.getInputParameter()));
        sb.append(",");
        sb.append("\n");
        sb.append(indenter.indent());
        
        /*
         * Constraints
         */
        sb.append("{constraints,");
        sb.append(oclGen.toStr(constraints));
        sb.append("}");

        sb.append("\n");
        sb.append(indenter.indent(-4));
        
        return sb.toString();
    }
    
    private String toStrParameters(List<Parameter> parameters) {
        StringBuffer sb = new StringBuffer();
        
        sb.append("{params, [");

        if(parameters.size() > 0) {
            sb.append("\n");
            sb.append(indenter.indent(4));
        }

        for(Iterator<Parameter> itParameter = parameters.iterator();
                itParameter.hasNext();) {

            sb.append(toStrParameter(itParameter.next()));

            if(itParameter.hasNext()) {
                sb.append(",");
                sb.append("\n");
                sb.append(indenter.indent());
            } else {
                sb.append("\n");
                sb.append(indenter.indent(-4));
            }

        }
        sb.append("]}");
        
        return sb.toString();
    }
    
    private String toStrParameter(Parameter parameter) {
        StringBuffer sb = new StringBuffer();
        
        sb.append("{param, [");

        sb.append("\n");
        sb.append(indenter.indent(4));

        /*
         * Parameter name
         */
        sb.append("{name, ");
        sb.append(toString(parameter.getName()));
        sb.append("},");

        sb.append("\n");
        sb.append(indenter.indent());

        /*
         * Parameter type
         */
        sb.append("{type, [");
        sb.append("\n");
        sb.append(indenter.indent(4));
        
        sb.append("{name, ");
        sb.append(toString(parameter.getType().getName()));
        sb.append("},");
        
        sb.append("{qualifiedName, ");
        sb.append(toString(parameter.getType().getQualifiedName()));
        sb.append("}");

        sb.append("\n");
        sb.append(indenter.indent(-4));
        sb.append("]}");

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
