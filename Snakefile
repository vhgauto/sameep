rule targets:
    input:
        "scripts/obtencion_datos_gis.bash",
        "figuras/gg_rf__001.png",
        "figuras/gg_turb__001.png",
        "index.html"

rule descarga_y_extraccion:
    input:
        script = "scripts/obtencion_datos_gis.bash"
    output:
        o1 = "figuras/gg_rf__001.png",
        o2 = "figuras/gg_turb__001.png"
    conda:
        "environment.yml"
    shell:
        """
        {input.script}
        """

rule render_index:
    input:
        rmd = "index.rmd",
        png = "figuras/gg_turb__001.png"
    output:
        "index.html"
    conda:
        "environment.yml"
    shell:
        """
        R -e "library(rmarkdown); render('{input.rmd}')"
        """