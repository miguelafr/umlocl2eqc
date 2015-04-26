package ocl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import tudresden.ocl20.pivot.metamodels.uml2.internal.model.UML2Class;
import tudresden.ocl20.pivot.metamodels.uml2.internal.model.UML2Operation;
import tudresden.ocl20.pivot.model.IModel;
import tudresden.ocl20.pivot.model.ModelAccessException;
import tudresden.ocl20.pivot.parser.ParseException;
import tudresden.ocl20.pivot.pivotmodel.ConstrainableElement;
import tudresden.ocl20.pivot.pivotmodel.Constraint;

public class OCLUtils {

    private static OCLUtils instance;

    private OCLUtils() {
        ;
    }

    public static OCLUtils getInstance() {
        if(instance == null) {
            instance = new OCLUtils();
        }
        return instance;
    }
    
    public Map<UML2Operation, Collection<Constraint>> getContraintsByOp(
            IModel model, List<Constraint> constraints)
            throws ModelAccessException, IOException, ParseException {

        Map<UML2Operation, Collection<Constraint>> result =
            new HashMap<UML2Operation, Collection<Constraint>>();

        for (Constraint constraint : constraints) {
            for (ConstrainableElement constrainableElement :
                constraint.getConstrainedElement()) {
                if(constrainableElement instanceof UML2Operation) {
                    UML2Operation uml2Operation =
                        (UML2Operation)constrainableElement;
                    if(!result.containsKey(uml2Operation)) {
                        result.put(uml2Operation,
                                new ArrayList<Constraint>());
                    }
                    result.get(uml2Operation).add(constraint);
                }
            }
        }

        return result;
    }
    
    public Map<UML2Class, Collection<Constraint>> getContraintsByClass(
            IModel model, List<Constraint> constraints)
            throws ModelAccessException, IOException, ParseException {

        Map<UML2Class, Collection<Constraint>> result =
            new HashMap<UML2Class, Collection<Constraint>>();

        for (Constraint constraint : constraints) {
            for (ConstrainableElement constrainableElement :
                constraint.getConstrainedElement()) {
                if(constrainableElement instanceof UML2Class) {
                    UML2Class uml2Class =
                        (UML2Class)constrainableElement;
                    if(!result.containsKey(uml2Class)) {
                        result.put(uml2Class,
                                new ArrayList<Constraint>());
                    }
                    result.get(uml2Class).add(constraint);
                }
            }
        }

        return result;
    }
    
}
