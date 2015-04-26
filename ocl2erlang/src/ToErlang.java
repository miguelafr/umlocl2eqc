
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.net.URL;
import java.util.List;

import tudresden.ocl20.pivot.model.IModel;
import tudresden.ocl20.pivot.pivotmodel.Constraint;
import tudresden.ocl20.pivot.standalone.facade.StandaloneFacade;
import uml.UMLGen;

public class ToErlang {

    private static UMLGen umlGen = UMLGen.getInstance();

    public static void main(String[] args) throws Exception {
        
        String umlModelFile = args[0];
        String oclConstraintsFile = args[1];

        StandaloneFacade.INSTANCE.initialize(new URL("file:"
                + new File("log4j.properties").getAbsolutePath()));

        try {

            IModel model = StandaloneFacade.INSTANCE.loadUMLModel(
                    new File(umlModelFile), getUMLResources());

            List<Constraint> constraints =
                    StandaloneFacade.INSTANCE.parseOclConstraints(model,
                            new File(oclConstraintsFile));

            FileWriter fileWriter = new FileWriter("constraints.erl");
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.write(umlGen.toStr(model, constraints).toString());
            output.close();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    private static File getUMLResources() {
        return new File(
                "lib/org.eclipse.uml2.uml.resources_3.1.0.v201005031530.jar");
    }

}
