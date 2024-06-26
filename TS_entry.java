import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Write a description of class Paciente here.
 * 
 * @author (your name)
 * @version (a version number or a date)
 */
public class TS_entry {
   private String id;
   private ClasseID classe;
   private TS_entry tipo;

   private int nroElementos;
   private TS_entry tipoBase;

   private TabSimb locals;

   // construtor para arrays
   public TS_entry(String umId, TS_entry umTipo, ClasseID umaClasse) {
      this(umId, umTipo, umaClasse, 0, null);
   }

   public TS_entry(String umId, TS_entry umTipo, ClasseID umaClasse, int elems, TS_entry tp) {
      id = umId;
      tipo = umTipo;
      classe = umaClasse;
      nroElementos = elems;
      tipoBase = tp;
   }

   public String getId() {
      return id;
   }

   public TS_entry getTipo() {
      return tipo;
   }

   public TS_entry getTipoBase() {
      return tipoBase;
   }

   public ClasseID getClasse() {
      return classe;
   }

   public TabSimb getLocals() {
      return locals;
   }

   public String toString() {
      StringBuilder aux = new StringBuilder("");

      aux.append("Id: ");
      aux.append(String.format("%-10s", id));

      aux.append("\tClasse: ");
      aux.append(classe);
      aux.append("\tTipo: ");
      aux.append(tipo2str(this.tipo));

      return aux.toString();

   }

   public String getTipoStr() {
      return tipo2str(this);
   }

   public String tipo2str(TS_entry tipo) {
      TS_entry tmp;
      if (tipo == null)
         return "null";
      else if (tipo == Parser.Tp_INT)
         return "int";
      else if (tipo == Parser.Tp_BOOL)
         return "boolean";
      else if (tipo == Parser.Tp_DOUBLE)
         return "double";

      else if (tipo.getTipo() == Parser.Tp_ARRAY)
         return String.format("array(%d, %s)", tipo.nroElementos, tipo2str(tipo.tipoBase));
      else if (tipo.getTipo() == Parser.Tp_Func) {
         List<TS_entry> params = tipo.locals.getParams();
         String paramsString = params.stream().map(p -> tipo2str(p.tipo)).collect(Collectors.joining(", "));
         tipo.getLocals().listar();
         return String.format("func(%s) -> %s", paramsString, tipo2str(tipo.tipoBase));
      }
      else if (tipo == Parser.Tp_ERRO)
         return "_erro_";
      else if (tipo == Parser.Tp_Void)
         return "void";

      return "erro/tp";
   }

   public void setLocals(TabSimb locals) {
      this.locals = locals;
   }

   public void setTipoBase(TS_entry tipoBase) {
      this.tipoBase = tipoBase;
   }
}
