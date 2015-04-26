package uml;

import java.util.ArrayList;
import java.util.List;

import tudresden.ocl20.pivot.metamodels.uml2.internal.model.UML2Class;
import tudresden.ocl20.pivot.model.IModel;
import tudresden.ocl20.pivot.pivotmodel.Namespace;
import tudresden.ocl20.pivot.pivotmodel.Type;

public class UMLUtils {

    private static UMLUtils instance;

    static {
        instance = new UMLUtils();
    }

    private UMLUtils() {
        ;
    }

    public static UMLUtils getInstance() {
        return instance;
    }

    public List<UML2Class> getClasses(IModel model) throws Exception {
        List<UML2Class> result = new ArrayList<UML2Class>();

        /*
         * Root namespace
         */
        for(Type ownedType : model.getRootNamespace().getOwnedType()) {
            if(ownedType instanceof UML2Class) {
                result.add((UML2Class)ownedType);
            }
        }

        /*
         * Nested namespaces
         */
        List<Namespace> namespaces = model.getRootNamespace().getNestedNamespace();
        for(Namespace namespace : namespaces) {
            for(Type ownedType : namespace.getOwnedType()) {
                if(ownedType instanceof UML2Class) {
                    result.add((UML2Class)ownedType);
                }
            }
        }
        return result;
    }

}
