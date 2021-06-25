*          DATA SET TAREP6A    AT LEVEL 003 AS OF 10/04/16                      
*PHASE T7036AB,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T7036A - RATE CALCULATOR'                                       
T7036A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7036A,R6                                                      
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+8                                                              
         BRAS  RE,VK               VALIDATE KEY                                 
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   YES                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         J     ERXIT                                                            
                                                                                
ERRMIS   MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         J     ERXIT                                                            
                                                                                
ERXIT    GOTO1 ERREX                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         MVC   TGDATE,TGTODAY1                                                  
         MVI   TGYREQU,0                                                        
         MVI   TGUNEQU,0                                                        
         MVI   FLTUSE,0                                                         
         MVI   FLTATY,0                                                         
                                                                                
         LA    R2,SPLOPTH                                                       
         GOTO1 ANY                                                              
                                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
                                                                                
         ZIC   R0,4(R1)                                                         
                                                                                
VK10     CLC   =C'YEAR',SCDATA1                                                 
         JNE   VK20                                                             
         GOTO1 YRVAL,DMCB,SCDATA2                                               
         JNE   ERRINV                                                           
         J     VK60                                                             
                                                                                
VK20     CLC   =C'DATE',SCDATA1                                                 
         JNE   VK30                                                             
         LA    R2,SCDATA2                                                       
         GOTO1 DTVAL,DMCB,(X'40',TGDATE)                                        
         JNE   ERRINV                                                           
         J     VK60                                                             
                                                                                
VK30     CLC   =C'UNION',SCDATA1                                                
         JNE   VK40                                                             
         GOTO1 UNIVAL,DMCB,SCDATA2                                              
         JNE   ERRINV                                                           
         J     VK60                                                             
                                                                                
VK40     CLC   =C'USE',SCDATA1                                                  
         JNE   VK50                                                             
         GOTO1 USEVAL,DMCB,(X'40',SCDATA2)                                      
         JNE   ERRINV                                                           
         TM    TGUSSTA2,NORATES                                                 
         JO    ERRINV                                                           
         TM    TGUSSTA3,SOAPUSE                                                 
         JO    ERRINV                                                           
         MVC   FLTUSE,TGUSEQU                                                   
         J     VK60                                                             
                                                                                
VK50     CLC   =C'ATYPE',SCDATA1                                                
         JNE   ERRINV                                                           
         GOTO1 CCTYPVAL,DMCB,SCDATA2                                            
         JNE   ERRINV                                                           
         MVC   FLTATY,TGCCTEQU                                                  
                                                                                
VK60     LA    R3,SCANNEXT                                                      
         BCT   R0,VK10                                                          
         DROP  R3                                                               
                                                                                
         CLI   TGYREQU,0           ENSURE CONTRACT YEAR                         
         JE    ERRMIS                                                           
         CLI   TGUNEQU,0           UNION                                        
         JE    ERRMIS                                                           
         CLI   FLTUSE,0            AND USE ARE ALL PROVIDED                     
         JE    ERRMIS                                                           
                                                                                
         LA    RE,TGUNYR                                                        
VK70     CLI   0(RE),0             ENSURE CONTRACT YEAR IS VALID                
         JE    ERRINV              FOR UNION                                    
         CLC   0(1,RE),TGYREQU                                                  
         JE    VK80                                                             
         LA    RE,1(RE)                                                         
         J     VK70                                                             
                                                                                
VK80     MVC   BYTE,TGUSXUNI       ENSURE UNION IS VALID FOR USE                
         NC    BYTE,TGUNEQU                                                     
         JNZ   ERRINV                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
         MVI   RECNUM,61                                                        
                                                                                
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN                                                      
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',HDUSE),L'HDUSE                               
         GOTO1 (RF),(R1),(C'T',HDTYPE),L'HDTYPE                                 
         GOTO1 (RF),(R1),(C'T',HDADS),L'HDADS                                   
         GOTO1 (RF),(R1),(C'T',HDMEDIA),L'HDMEDIA                               
         GOTO1 (RF),(R1),(C'T',HDCATYPE),L'HDCATYPE                             
         GOTO1 (RF),(R1),(C'T',HDAFMR),L'HDAFMR                                 
         GOTO1 (RF),(R1),(C'T',HDUNI),L'HDUNI                                   
         GOTO1 (RF),(R1),(C'T',HDCAT),L'HDCAT                                   
         GOTO1 (RF),(R1),(C'T',HDCATNAM),L'HDCATNAM                             
         GOTO1 (RF),(R1),(C'T',HDCAM),L'HDCAM                                   
         GOTO1 (RF),(R1),(C'T',HDRATE),L'HDRATE                                 
         GOTO1 (RF),(R1),(C'T',HDUNIT),L'HDUNIT                                 
         BAS   RE,EOLDOWN                                                       
                                                                                
         XC    AUSE,AUSE                                                        
                                                                                
PREP10   BRAS  RE,GETUSE                                                        
         JNE   XIT                                                              
                                                                                
         XC    AUTYP,AUTYP                                                      
                                                                                
PREP20   BRAS  RE,GETUTY                                                        
         JNE   PREP10                                                           
                                                                                
         XC    AMAJ,AMAJ                                                        
                                                                                
PREP30   BRAS  RE,GETMAJ                                                        
         JNE   PREP20                                                           
                                                                                
         MVC   ADSTATE,=2X'FF'                                                  
                                                                                
PREP40   BRAS  RE,GETADS                                                        
         JNE   PREP30                                                           
                                                                                
         XC    AMEDIA,AMEDIA                                                    
                                                                                
PREP50   BRAS  RE,GETMED                                                        
         JNE   PREP40                                                           
                                                                                
         XC    ACTY,ACTY                                                        
         XC    AATY,AATY                                                        
                                                                                
PREP60   BRAS  RE,GETTYP                                                        
         JNE   PREP50                                                           
                                                                                
         XC    AART,AART                                                        
                                                                                
PREP70   BAS   RE,GETART                                                        
         JNE   PREP60                                                           
                                                                                
         XC    ACAT,ACAT                                                        
                                                                                
PREP80   BAS   RE,GETCAT                                                        
         JNE   PREP70                                                           
                                                                                
         XC    TGONOF,TGONOF                                                    
                                                                                
PREP90   BAS   RE,GETCAM                                                        
         JNE   PREP80                                                           
                                                                                
         XC    AUNIT,AUNIT                                                      
         XC    SVUNITS,SVUNITS                                                  
                                                                                
PREP100  BAS   RE,CALC                                                          
         JNE   PREP90                                                           
                                                                                
         CLC   =C'NOPRINT',SVDESC                                               
         JE    PREP100                                                          
         GOTO1 OUTPDOWN,DMCB,(C'T',TGUSCDE),L'TGUSCDE                           
         GOTO1 (RF),(R1),(C'T',TGUSNAME),L'TGUSNAME                             
         GOTO1 (RF),(R1),(C'T',ADSTATE),L'ADSTATE                               
         GOTO1 (RF),(R1),(C'T',TGMENAME),L'TGMENAME                             
         GOTO1 (RF),(R1),(C'T',SVTYPNAM),L'SVTYPNAM                             
         GOTO1 (RF),(R1),(C'T',AFMRATE),L'AFMRATE                               
         GOTO1 (RF),(R1),(C'T',TGUNCDE),L'TGUNCDE                               
         GOTO1 (RF),(R1),(C'T',TGCAT),L'TGCAT                                   
         GOTO1 (RF),(R1),(C'T',SVCATNAM),L'SVCATNAM                             
         GOTO1 (RF),(R1),(C'T',TGONOF),L'TGONOF                                 
         GOTO1 (RF),(R1),(C'T',RATE),L'RATE                                     
         GOTO1 (RF),(R1),(C'T',SVDESC),L'SVDESC                                 
         BAS   RE,EOLDOWN                                                       
         J     PREP100                                                          
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS A(NEXT AFM RATE) IS AART                     *         
***********************************************************************         
                                                                                
GETART   NTR1                                                                   
         L     R3,AART                                                          
         LTR   R3,R3                                                            
         JNZ   GART10                                                           
         LA    R3,ARTTAB                                                        
         TM    TGUSXUNI,AFM                                                     
         JZ    GART20                                                           
         LA    R3,NARTAB                                                        
         J     GART20                                                           
                                                                                
GART10   CLI   1(R3),X'FF'                                                      
         JE    NO                                                               
         LA    R3,1(R3)                                                         
                                                                                
GART20   MVC   AFMRATE,0(R3)                                                    
         ST    R3,AART                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        AFM RATE TABLE                                               *         
***********************************************************************         
                                                                                
NARTAB   DC    X'0'                                                             
         DC    X'FF'                                                            
                                                                                
ARTTAB   DC    C'1'                                                             
         DC    C'2'                                                             
         DC    C'5'                                                             
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        ROUTINE RETURNS A(NEXT CATEGORY) IN ACAT                     *         
***********************************************************************         
                                                                                
GETCAT   NTR1                                                                   
         USING CATTABD,R2                                                       
         L     R2,TGACATS                                                       
         OC    ACAT,ACAT                                                        
         JZ    GC20                                                             
         L     R2,ACAT                                                          
GC10     ZIC   RE,CATLEN                                                        
         AR    R2,RE                                                            
         CLI   0(R2),X'FF'                                                      
         JE    NO                                                               
                                                                                
GC20     MVC   TGBYTE,TGUNEQU                                                   
         NC    TGBYTE,CATUNI                                                    
         JZ    GC10                                                             
                                                                                
         GOTO1 CATVAL,DMCB,(X'80',CATEQU)                                       
                                                                                
         TM    TGUSSTAT,SESSION    IF THIS IS A SESSION                         
         JZ    GC30                                                             
         TM    TGUSXCAT,EXTRA      AND EXTRAS NOT EXCLUDED                      
         JO    GC30                                                             
         TM    TGCATYPE,EXTRA      ALWAYS ALLOW EXTRAS (FOR PPF & CNL)          
         JO    GC40                                                             
GC30     MVC   BYTE,TGUSXCAT       TEST THIS CATEGORY EXCLUDED FROM USE         
         NC    BYTE,TGCATYPE                                                    
         JNZ   GC10                YES - GET NEXT                               
                                                                                
GC40     TM    TGUSSTAT,SESSION    IF THIS ISN'T A SESSION                      
         JO    GC50                                                             
         TM    TGUSSTA2,HLDTYPE    OR A HOLDING FEE TYPE                        
         BO    GC50                                                             
         TM    TGCATYPE,NOREUSE    EXCLUDE SOME CATEGORIES                      
         JO    GC10                                                             
                                                                                
GC50     MVC   SVCATNAM,SPACES                                                  
         ZIC   RE,CATLEN                                                        
         SHI   RE,CATNAME-CATTABD+1                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   SVCATNAM(0),CATNAME                                              
         ST    R2,ACAT                                                          
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE RETURNS CAMERA SETTING IN TGONOF                     *         
***********************************************************************         
                                                                                
GETCAM   NTR1                                                                   
         OC    TGONOF,TGONOF                                                    
         JNZ   GCAM30                                                           
         MVC   TGONOF,=C'ON '                                                   
                                                                                
         CLI   TGMEEQU,RADIO                                                    
         JE    GCAM30                                                           
         CLI   TGUSEQU,UIDS                                                     
         JE    GCAM30                                                           
         CLI   TGUSEQU,UIVR                                                     
         JE    GCAM30                                                           
                                                                                
         CLI   TGUSEQU,UINS        IF MAKING AN INS                             
         JE    GCAM10                                                           
         CLI   TGUSEQU,UIDS        OR IDS                                       
         JE    GCAM10                                                           
         TM    TGUSSTA4,INDPHAS2   OR INDUSTRIAL PHASE 2 PAYMENT ...            
         JZ    GCAM20                                                           
GCAM10   TM    TGCATYPE,INDON      IF PERFORMER IS ON CAMERA                    
         JZ    GCAM30              CHECK ELIGIBILITY FOR ON CAMERA              
                                                                                
GCAM20   TM    TGCASTAT,OKON                                                    
         JZ    GCAM30                                                           
                                                                                
         CLI   TGUSEQU,USTR                                                     
         JE    GCAM30                                                           
         CLI   TGUSEQU,UIVR                                                     
         JE    GCAM30                                                           
         CLI   TGUSEQU,UIDS                                                     
         JE    GCAM30                                                           
         CLI   TGUSEQU,UIVR                                                     
         JE    GCAM30                                                           
         CLI   TGUSEQU,UDIO                                                     
         JNE   YES                                                              
                                                                                
***********************************************************************         
                                                                                
GCAM30   CLC   TGONOF,=C'ON '                                                   
         JNE   NO                                                               
         MVC   TGONOF,=C'OFF'                                                   
                                                                                
         CLI   TGUSEQU,UINS        IF MAKING AN INS                             
         JE    GCAM40                                                           
         CLI   TGUSEQU,UIDS        OR IDS                                       
         JE    GCAM40                                                           
         TM    TGUSSTA4,INDPHAS2   OR INDUSTRIAL PHASE 2 PAYMENT ...            
         JZ    GCAM50                                                           
GCAM40   TM    TGCATYPE,INDOFF     CHECK ELIGIBILITY FOR OFF CAMERA             
         JZ    NO                                                               
                                                                                
GCAM50   TM    TGCASTAT,OKOFF                                                   
         JZ    NO                                                               
                                                                                
         CLI   TGUSEQU,UDWN                                                     
         JE    NO                                                               
         TM    TGUSSTA2,HLDTYPE                                                 
         JO    GCAM60                                                           
         CLI   TGUSEQU,USCN                                                     
         JE    GCAM60                                                           
         CLI   TGUSEQU,UREN                                                     
         JE    GCAM60                                                           
         CLI   TGUSEQU,UARN                                                     
         JE    GCAM60                                                           
         CLI   TGUSEQU,USRE                                                     
         JNE   YES                                                              
GCAM60   TM    TGCATYPE,NOHLDOFF                                                
         JO    NO                                                               
         CLI   TGYREQU,CN88                                                     
         JL    YES                                                              
         TM    TGCATYPE,NHLDOF88                                                
         JZ    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE CALCULATES RATE                                      *         
***********************************************************************         
                                                                                
CALC     NTR1                                                                   
         USING UNITTABD,R2                                                      
         L     R2,AUNIT                                                         
         OC    SVUNITS,SVUNITS                                                  
         JNZ   CALC25                                                           
                                                                                
         LA    R2,UNITTAB                                                       
         OC    AUNIT,AUNIT                                                      
         JZ    CALC20                                                           
         L     R2,AUNIT                                                         
CALC10   AHI   R2,UTLNQ                                                         
         OC    SVUNITS,SVUNITS                                                  
         JZ    *+6                                                              
         DC    H'00'                                                            
         CLI   0(R2),X'FF'                                                      
         JE    NO                                                               
CALC20   CLC   SVTLKUP,UTLKUP                                                   
         JNE   CALC10                                                           
         MVC   SVDESC,UTDESC                                                    
                                                                                
CALC25   LA    R0,TCD                                                           
         LHI   R1,TCEND-TCD                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   TCCAONOF,TGONOF     ON/OFF CAMERA                                
         MVC   TCPCYCS,TGDATE                                                   
         MVC   TCADDST,ADSTATE                                                  
                                                                                
         USING TACOD,R3                                                         
         LA    R3,ELTACO                                                        
         XC    ELTACO,ELTACO                                                    
         MVC   TACOADST,ADSTATE                                                 
                                                                                
         MVC   TACOAFM,AFMRATE                                                  
                                                                                
         TM    TGUSSTAT,CANUSE                                                  
         JZ    CALC30                                                           
         OI    TACOSTAT,TACOSCRT                                                
         MVC   TACOCTYP,TGCCTEQU                                                
         J     CALC40                                                           
                                                                                
CALC30   MVC   TACOTYPE,TGCTEQU                                                 
                                                                                
CALC40   ST    R3,TCATACO                                                       
         DROP  R3                                                               
                                                                                
         CLC   SVTLKUP,=AL2(PERUSE)                                             
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCDEMO                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCDEM5                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCDEM2                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCDEMA                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCCLA1                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCCLA2                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCCLA3                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCCLA4                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCCLA14                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCPAX                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCITN1                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCITN2                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCITN3                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCITN4                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCITN14                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCWSM                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCWSM51                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC1                                                         
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC57                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC58                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC59                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC61                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC62                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC63                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC66                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC67                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC70                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC71                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC101                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC151                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC201                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC1001                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCC2501                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCL1                                                         
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCL50K                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCL100K                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCL150K                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCL200K                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCL250K                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCL500K                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCL750K                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCL1M                                                        
         JE    CALC50                                                           
                                                                                
         CLC   SVTLKUP,=AL2(LCBLIKE)                                            
         JE    CALC10                                                           
                                                                                
         BRAS  RE,CLCWMAJ                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCWM1                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCWM36                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCWM101                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCW1                                                         
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCW2                                                         
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCW26                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCW61                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCW126                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCSNW2                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCA25                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCA26                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCA60                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCA61                                                        
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCA125                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCA126                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCVNR1                                                       
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCTAG25                                                      
         JE    CALC50                                                           
                                                                                
         BRAS  RE,CLCTAG50                                                      
         JE    CALC50                                                           
                                                                                
         USING TASDD,R3                                                         
         LA    R3,ELTASD                                                        
         XC    ELTASD,ELTASD                                                    
         MVC   TASDEQU,TGUSEQU                                                  
         ST    R3,TCATASD                                                       
                                                                                
         BRAS  RE,CLCSD                                                         
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCOT                                                         
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCDT                                                         
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCTRVH                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCTRV15                                                      
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCPDWH                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCPDW15                                                      
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCRSD                                                        
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCR90M                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCRTG1                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCRTGA                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCBSMSD                                                      
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCBSM1                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCBSM20                                                      
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCHOUR                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCRTKH                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCRTKO                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCRTK10                                                      
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCRTKD                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCRTKT                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCIVRH                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCIVR30                                                      
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCIVRT                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCTAGS                                                       
         JNE   CALC10                                                           
                                                                                
         BRAS  RE,CLCTAG12                                                      
         JNE   CALC10                                                           
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
CALC50   MVC   TGTHREE,TGYRCDE                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 TASYSCLC,DMCB,(RC),TCD,SYSCOMM                                   
         MVC   TGUSSTA2,SVUSSTA2                                                
         MVC   TGYRCDE,TGTHREE                                                  
         MVC   AIO,AIO1                                                         
                                                                                
         CLC   UTLINE,=AL2(UTLTRV15)                                            
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLPDW15)                                            
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLDEMA)                                             
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLRTGA)                                             
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLWSM)                                              
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLWSM51)                                            
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLBSM20)                                            
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLSNW2)                                             
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLA26)                                              
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLA61)                                              
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLA126)                                             
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLRTKO)                                             
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLRTKD)                                             
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLRTKT)                                             
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLIVR30)                                            
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLIVRT)                                             
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLPDEM5)                                            
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLTAG2)                                             
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLTAG25)                                            
         JE    CALC60                                                           
         CLC   UTLINE,=AL2(UTLTAG50)                                            
         JNE   CALC100                                                          
CALC60   L     R4,TCPAY                                                         
                                                                                
         L     RE,TCPAY                                                         
         L     RF,SVPAY                                                         
*******  CLI   TGUSTYP,UTAGSESS                                                 
*******  JNE   *+6                                                              
*******  DC    H'00'                                                            
         SR    RE,RF                                                            
         ST    RE,TCPAY                                                         
                                                                                
         CLC   UTLINE,=AL2(UTLWSM)                                              
         JNE   CALC90                                                           
         ST    R4,SVPAY                                                         
                                                                                
         OC    TCPAY,TCPAY                                                      
         JZ    CALC70                                                           
         EDIT  TCPAY,RATE,2,MINUS=YES,ZERO=NOBLANK                              
                                                                                
CALC70   CLC   TCUNITS,=H'50'                                                   
         JL    CALC80                                                           
         XC    SVUNITS,SVUNITS                                                  
         OC    TCPAY,TCPAY                                                      
         JZ    CALC10                                                           
         ST    R2,AUNIT                                                         
         J     YES                                                              
                                                                                
CALC80   OC    TCPAY,TCPAY                                                      
         JZ    CALC25                                                           
         ST    R2,AUNIT                                                         
         J     YES                                                              
                                                                                
CALC90   CLC   UTLINE,=AL2(UTLWSM51)                                            
         JNE   CALC100                                                          
         XC    SVPAY,SVPAY                                                      
         J     CACL140                                                          
                                                                                
CALC100  OC    TCPAY,TCPAY                                                      
         JZ    CALC130                                                          
                                                                                
         CLC   UTLINE,=AL2(UTLTAG1)                                             
         JNE   CALC110                                                          
         L     RE,TCPAY                                                         
         MHI   RE,24                                                            
         L     RF,SVPAY                                                         
         AR    RE,RF                                                            
         ST    RE,SVPAY                                                         
         J     CACL140                                                          
                                                                                
CALC110  CLC   UTLINE,=AL2(UTLTAG2)                                             
         JNE   CALC120                                                          
         L     RE,TCPAY                                                         
         MHI   RE,24                                                            
         L     RF,SVPAY                                                         
         AR    RE,RF                                                            
         ST    RE,SVPAY                                                         
         J     CACL140                                                          
                                                                                
CALC120  CLC   UTLINE,=AL2(UTLTAG25)                                            
         JNE   CALC130                                                          
         L     RE,TCPAY                                                         
         MHI   RE,25                                                            
         L     RF,SVPAY                                                         
         AR    RE,RF                                                            
         ST    RE,SVPAY                                                         
         J     CACL140                                                          
                                                                                
CALC130  MVC   SVPAY,TCPAY                                                      
                                                                                
CACL140  CLC   UTLINE,=AL2(UTLPDEMO)                                            
         JNE   CALC150                                                          
         L     RE,SVPAY                                                         
         MHI   RE,4                                                             
         ST    RE,SVPAY                                                         
                                                                                
CALC150  OC    TCPAY,TCPAY                                                      
         JZ    CALC10                                                           
         EDIT  TCPAY,RATE,2,MINUS=YES,ZERO=NOBLANK                              
         ST    R2,AUNIT                                                         
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
INITDOWN NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
HDUSE    DC    C'USE'                                                           
HDTYPE   DC    C'TYPE'                                                          
HDMAJ    DC    C'MAJORS'                                                        
HDAFMR   DC    C'AFM RATE'                                                      
HDADS    DC    C'ADDENDUM STATE'                                                
HDMEDIA  DC    C'MEDIA'                                                         
HDCATYPE DC    C'COMMERCIAL TYPE/ACTRA TYPE'                                    
HDUNI    DC    C'UNION'                                                         
HDCAT    DC    C'CATEGORY CODE'                                                 
HDCATNAM DC    C'CATEGORY NAME'                                                 
HDCAM    DC    C'CAMERA'                                                        
HDRATE   DC    C'RATE'                                                          
HDUNIT   DC    C'UNIT'                                                          
                                                                                
UNITTAB  DC    AL2(BSSLIKE),AL2(UTLSD)                                          
         DC    CL30'PER SPOT/DAY'                                               
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLOT)                                          
         DC    CL30'PER OVERTIME HOUR'                                          
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLDT)                                          
         DC    CL30'PER DOUBLETIME HOUR'                                        
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLTRVH)                                        
         DC    CL30'FOR FIRST TRAVEL HOUR'                                      
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLTRV15)                                       
         DC    CL30'PER TRAVEL 15 MINUTE INCREMENT'                             
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLPDWH)                                        
         DC    CL30'FOR 1ST PDW HOUR'                                           
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLPDW15)                                       
         DC    CL30'PER PDW 15 MINUTE INCREMENT'                                
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLTAGS)                                        
         DC    CL30'FOR 1ST TAG AT SESSION RATE'                                
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLTAG1)                                        
         DC    CL30'PER TAG 1-24/1-25'                                          
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLTAG2)                                        
         DC    CL30'PER TAG 2-25'                                               
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLTAG25)                                       
         DC    CL30'PER TAG 25-49/26-50'                                        
                                                                                
         DC    AL2(BSSLIKE),AL2(UTLTAG50)                                       
         DC    CL30'PER TAG 50+/51+'                                            
                                                                                
         DC    AL2(PERUSE),AL2(UTLPUSE)                                         
         DC    CL30'PER USE'                                                    
                                                                                
         DC    AL2(PERDEMO),AL2(UTLPDEMO)                                       
         DC    CL30'PER DEMO 1-4'                                               
                                                                                
         DC    AL2(PERDEMO),AL2(UTLPDEM5)                                       
         DC    CL30'PER DEMO 5+'                                                
                                                                                
         DC    AL2(PERDEM2),AL2(UTLDEM2)                                        
         DC    CL30'FOR 2ND DEMO'                                               
                                                                                
         DC    AL2(PERDEM2),AL2(UTLDEMA)                                        
         DC    CL30'PER ADDITIONAL DEMO'                                        
                                                                                
         DC    AL2(BSRLIKE),AL2(UTLRSD)                                         
         DC    CL30'PER SPOT/DAY'                                               
                                                                                
         DC    AL2(BSRLIKE),AL2(UTLR90M)                                        
         DC    CL30'PER EACH 90 MINUTES'                                        
                                                                                
         DC    AL2(BSRLIKE),AL2(UTLRTG1)                                        
         DC    CL30'FOR 1ST TAG'                                                
                                                                                
         DC    AL2(BSRLIKE),AL2(UTLRTGA)                                        
         DC    CL30'PER ADDITIONAL TAG'                                         
                                                                                
         DC    AL2(CLALIKE),AL2(UTLCLA1)                                        
         DC    CL30'FOR 1ST USE'                                                
                                                                                
         DC    AL2(CLALIKE),AL2(UTLCLA2)                                        
         DC    CL30'FOR 2ND USE'                                                
                                                                                
         DC    AL2(CLALIKE),AL2(UTLCLA3)                                        
         DC    CL30'FOR 3RD USE'                                                
                                                                                
         DC    AL2(CLALIKE),AL2(UTLCLA4)                                        
         DC    CL30'PER USE 4-13'                                               
                                                                                
         DC    AL2(CLALIKE),AL2(UTLCLA14)                                       
         DC    CL30'PER USE 14+'                                                
                                                                                
         DC    AL2(PAXLIKE),AL2(UTLPAX)                                         
         DC    CL30'PER USE'                                                    
                                                                                
         DC    AL2(ITNLIKE),AL2(UTLITN1)                                        
         DC    CL30'FOR 1ST USE'                                                
                                                                                
         DC    AL2(ITNLIKE),AL2(UTLITN2)                                        
         DC    CL30'FOR 2ND USE'                                                
                                                                                
         DC    AL2(ITNLIKE),AL2(UTLITN3)                                        
         DC    CL30'FOR 3RD USE'                                                
                                                                                
         DC    AL2(ITNLIKE),AL2(UTLITN4)                                        
         DC    CL30'PER USE 4-13'                                               
                                                                                
         DC    AL2(ITNLIKE),AL2(UTLITN14)                                       
         DC    CL30'PER USE 14+'                                                
                                                                                
         DC    AL2(WSMLIKE),AL2(UTLWSM)                                         
UTWSM    DC    CL8'FOR USE '                                                    
UTWSMUSE DC    CL22' '                                                          
                                                                                
         DC    AL2(WSMLIKE),AL2(UTLWSM51)                                       
         DC    CL30'PER USE 51+'                                                
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC1)                                          
         DC    CL30'FOR USE 1-56'                                               
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC57)                                         
         DC    CL30'FOR 57TH USE'                                               
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC58)                                         
         DC    CL30'FOR 58TH USE'                                               
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC59)                                         
         DC    CL30'PER USE 59-60'                                              
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC61)                                         
         DC    CL30'FOR 61ST USE'                                               
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC62)                                         
         DC    CL30'FOR 62ND USE'                                               
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC63)                                         
         DC    CL30'PER USE 63-65'                                              
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC66)                                         
         DC    CL30'FOR 66TH USE'                                               
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC67)                                         
         DC    CL30'PER USE 67-69'                                              
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC70)                                         
         DC    CL30'FOR 70TH USE'                                               
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC71)                                         
         DC    CL30'PER USE 71-100'                                             
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC101)                                        
         DC    CL30'PER USE 101-150'                                            
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC151)                                        
         DC    CL30'PER USE 151-200'                                            
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC201)                                        
         DC    CL30'PER USE 201-1000'                                           
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC1001)                                       
         DC    CL30'PER USE 1001-2500'                                          
                                                                                
         DC    AL2(CBLLIKE),AL2(UTLC2501)                                       
         DC    CL30'PER USE 2501-3000'                                          
                                                                                
         DC    AL2(LCBLIKE),AL2(UTLL1)                                          
         DC    CL30'FOR SUBSCRIBERS 1-50K'                                      
                                                                                
         DC    AL2(LCBLIKE),AL2(UTLL50K)                                        
         DC    CL30'FOR SUBSCRIBERS 50,001-100K'                                
                                                                                
         DC    AL2(LCBLIKE),AL2(UTLL100K)                                       
         DC    CL30'FOR SUBSCRIBERS 100,001-150K'                               
                                                                                
         DC    AL2(LCBLIKE),AL2(UTLL150K)                                       
         DC    CL30'FOR SUBSCRIBERS 150,001-200K'                               
                                                                                
         DC    AL2(LCBLIKE),AL2(UTLL200K)                                       
         DC    CL30'FOR SUBSCRIBERS 200,001-250K'                               
                                                                                
         DC    AL2(LCBLIKE),AL2(UTLL250K)                                       
         DC    CL30'FOR SUBSCRIBERS 250,001-500K'                               
                                                                                
         DC    AL2(LCBLIKE),AL2(UTLL500K)                                       
         DC    CL30'FOR SUBSCRIBERS 500,001-750K'                               
                                                                                
         DC    AL2(LCBLIKE),AL2(UTLL750K)                                       
         DC    CL30'FOR SUBSCRIBERS 750,001-1M'                                 
                                                                                
         DC    AL2(LCBLIKE),AL2(UTLL1M)                                         
         DC    CL30'FOR SUBSCRIBERS OVER 1M'                                    
                                                                                
         DC    AL2(BSMLIKE),AL2(UTLBSMSD)                                       
         DC    CL30'PER SPOT/DAY'                                               
                                                                                
         DC    AL2(BSMLIKE),AL2(UTLBSM1)                                        
         DC    CL30'FOR 1ST HOUR'                                               
                                                                                
         DC    AL2(BSMLIKE),AL2(UTLBSM20)                                       
         DC    CL30'FOR EACH ADDITIONAL 20 MINUTES'                             
                                                                                
         DC    AL2(PERHOUR),AL2(UTLHOUR)                                        
         DC    CL30'PER HOUR'                                                   
                                                                                
         DC    AL2(PERPRHR),AL2(UTLHOUR)                                        
         DC    CL30'PER PROGRAM/HOUR'                                           
                                                                                
         DC    AL2(WSPLIKE),AL2(UTLWMAJ)                                        
UTWM     DC    CL4'FOR '                                                        
UTWMAJ   DC    CL26' '                                                          
                                                                                
         DC    AL2(WSPLIKE),AL2(UTLWM1)                                         
UTWM1    DC    CL18'PER UNIT 1-35 FOR '                                         
UTWMA1   DC    CL12' '                                                          
                                                                                
         DC    AL2(WSPLIKE),AL2(UTLWM36)                                        
UTWM36   DC    CL20'PER UNIT 36-100 FOR '                                       
UTWMA36  DC    CL10' '                                                          
                                                                                
         DC    AL2(WSPLIKE),AL2(UTLWM101)                                       
UTWM101  DC    CL18'PER UNIT 101+ FOR '                                         
UTWMA101 DC    CL12' '                                                          
                                                                                
         DC    AL2(WSPLIKE),AL2(UTLW1)                                          
         DC    CL30'FOR 1ST UNIT'                                               
                                                                                
         DC    AL2(WSPLIKE),AL2(UTLW2)                                          
         DC    CL30'PER UNIT 2-25'                                              
                                                                                
         DC    AL2(WSPLIKE),AL2(UTLW26)                                         
         DC    CL30'PER UNIT 26-60'                                             
                                                                                
         DC    AL2(WSPLIKE),AL2(UTLW61)                                         
         DC    CL30'PER UNIT 61-125'                                            
                                                                                
         DC    AL2(WSPLIKE),AL2(UTLW126)                                        
         DC    CL30'PER UNIT 126+'                                              
                                                                                
         DC    AL2(SNWLIKE),AL2(UTLW1)                                          
         DC    CL30'FOR 1ST UNIT'                                               
                                                                                
         DC    AL2(SNWLIKE),AL2(UTLSNW2)                                        
         DC    CL30'PER UNIT 2-255'                                             
                                                                                
         DC    AL2(RRNLIKE),AL2(UTLWMAJ)                                        
         DC    CL30'FOR'                                                        
                                                                                
         DC    AL2(ADCLIKE),AL2(UTLW1)                                          
         DC    CL30'FOR 1ST UNIT'                                               
                                                                                
         DC    AL2(ADCLIKE),AL2(UTLSNW2)                                        
         DC    CL30'PER UNIT 2-25'                                              
                                                                                
         DC    AL2(ADCLIKE),AL2(UTLA25)                                         
         DC    CL30'NOPRINT'                                                    
                                                                                
         DC    AL2(ADCLIKE),AL2(UTLA26)                                         
         DC    CL30'PER UNIT 26-60'                                             
                                                                                
         DC    AL2(ADCLIKE),AL2(UTLA60)                                         
         DC    CL30'NOPRINT'                                                    
                                                                                
         DC    AL2(ADCLIKE),AL2(UTLA61)                                         
         DC    CL30'PER UNIT 61-125'                                            
                                                                                
         DC    AL2(ADCLIKE),AL2(UTLA125)                                        
         DC    CL30'NOPRINT'                                                    
                                                                                
         DC    AL2(ADCLIKE),AL2(UTLA126)                                        
         DC    CL30'PER UNIT 126+'                                              
                                                                                
         DC    AL2(TAGLIKE),AL2(UTLTAGS)                                        
         DC    CL30'FOR 1ST TAG AT SESSION RATE'                                
                                                                                
         DC    AL2(TAGLIKE),AL2(UTLTAG1)                                        
         DC    CL30'PER TAG 1-24/1-25'                                          
                                                                                
         DC    AL2(TAGLIKE),AL2(UTLTAG2)                                        
         DC    CL30'PER TAG 2-25'                                               
                                                                                
         DC    AL2(TAGLIKE),AL2(UTLTAG25)                                       
         DC    CL30'PER TAG 25-49/26-50'                                        
                                                                                
         DC    AL2(TAGLIKE),AL2(UTLTAG50)                                       
         DC    CL30'PER TAG 50+/51+'                                            
                                                                                
         DC    AL2(VRELIKE),AL2(UTLCLA1)                                        
         DC    CL30'PER VARIATION'                                              
                                                                                
         DC    AL2(VNRLIKE),AL2(UTLVNR1)                                        
         DC    CL30'FOR 1ST VARIATION'                                          
                                                                                
         DC    AL2(VNRLIKE),AL2(UTLCLA1)                                        
         DC    CL30'PER 4 VARIATIONS'                                           
                                                                                
         DC    AL2(RTKLIKE),AL2(UTLRTKH)                                        
         DC    CL30'PER HOUR'                                                   
                                                                                
         DC    AL2(RTKLIKE),AL2(UTLRTKO)                                        
         DC    CL30'PER OVERTIME HOUR'                                          
                                                                                
         DC    AL2(RTKLIKE),AL2(UTLRTK10)                                       
         DC    CL30'NOPRINT'                                                    
                                                                                
         DC    AL2(RTKLIKE),AL2(UTLRTKD)                                        
         DC    CL30'PER DOUBLETIME HOUR'                                        
                                                                                
         DC    AL2(RTKLIKE),AL2(UTLRTKH)                                        
         DC    CL30'NOPRINT'                                                    
                                                                                
         DC    AL2(RTKLIKE),AL2(UTLRTKT)                                        
         DC    CL30'PER TRAVEL HOUR'                                            
                                                                                
         DC    AL2(IVRLIKE),AL2(UTLIVRH)                                        
         DC    CL30'FOR 1ST HOUR'                                               
                                                                                
         DC    AL2(IVRLIKE),AL2(UTLIVR30)                                       
         DC    CL30'PER ADDITIONAL 30 MINUTES'                                  
                                                                                
         DC    AL2(IVRLIKE),AL2(UTLIVRH)                                        
         DC    CL30'NOPRINT'                                                    
                                                                                
         DC    AL2(IVRLIKE),AL2(UTLIVRT)                                        
         DC    CL30'PER 30 MINUTES OF TRAVEL TIME'                              
                                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS A(NEXT USE) IN AUSE                          *         
***********************************************************************         
                                                                                
GETUSE   NTR1  BASE=*,LABEL=*                                                   
         USING USETABD,R2                                                       
         L     R2,TGAUSES                                                       
         OC    AUSE,AUSE                                                        
         JZ    GU20                                                             
         L     R2,AUSE                                                          
GU10     LH    RE,USELEN                                                        
         AR    R2,RE                                                            
         CLI   0(R2),X'FF'                                                      
         JE    NO                                                               
                                                                                
GU20     CLC   USEEQU,FLTUSE                                                    
         JNE   GU10                                                             
                                                                                
         USING USLUTABD,R3                                                      
         LA    R3,USLUTAB                                                       
GU30     CLC   ULTUSE,USEEQU                                                    
         JE    GU40                                                             
         CLI   ULTLNQ(R3),X'FF'                                                 
         JE    GU10                                                             
         LA    R3,ULTLNQ(R3)                                                    
         J     GU30                                                             
GU40     MVC   SVTLKUP,ULTLKUP                                                  
         DROP  R3                                                               
                                                                                
         ST    R2,AUSE                                                          
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
USLUTAB  DS    0A                                                               
         DC    AL1(UBSS),AL2(BSSLIKE)                                           
         DC    AL1(USSS),AL2(BSSLIKE)                                           
         DC    AL1(UBSC),AL2(BSSLIKE)                                           
         DC    AL1(UCSS),AL2(BSSLIKE)                                           
         DC    AL1(UALF),AL2(PERUSE)                                            
         DC    AL1(ULFT),AL2(PERUSE)                                            
         DC    AL1(USLF),AL2(PERUSE)                                            
         DC    AL1(UDEM),AL2(PERDEMO)                                           
         DC    AL1(UADD),AL2(PERDEMO)                                           
         DC    AL1(USNA),AL2(PERDEMO)                                           
         DC    AL1(UCDM),AL2(PERDEM2)                                           
         DC    AL1(UCAU),AL2(PERUSE)                                            
         DC    AL1(UHLD),AL2(PERUSE)                                            
         DC    AL1(USHL),AL2(PERUSE)                                            
         DC    AL1(URRS),AL2(BSSLIKE)                                           
         DC    AL1(UARS),AL2(BSSLIKE)                                           
         DC    AL1(USRS),AL2(BSSLIKE)                                           
         DC    AL1(URRR),AL2(BSRLIKE)                                           
         DC    AL1(UARR),AL2(BSRLIKE)                                           
         DC    AL1(UCLA),AL2(CLALIKE)                                           
         DC    AL1(UPAX),AL2(PAXLIKE)                                           
         DC    AL1(UITN),AL2(ITNLIKE)                                           
         DC    AL1(ULNA),AL2(CLALIKE)                                           
         DC    AL1(ULNN),AL2(CLALIKE)                                           
         DC    AL1(ULNF),AL2(CLALIKE)                                           
         DC    AL1(ULNC),AL2(CLALIKE)                                           
         DC    AL1(USNT),AL2(PERUSE)                                            
         DC    AL1(UNET),AL2(WSMLIKE)                                           
         DC    AL1(ULOC),AL2(PERUSE)                                            
         DC    AL1(UDLR),AL2(PERUSE)                                            
         DC    AL1(UWSP),AL2(WSPLIKE)                                           
         DC    AL1(USWS),AL2(WSPLIKE)                                           
         DC    AL1(USNW),AL2(SNWLIKE)                                           
         DC    AL1(UADW),AL2(WSPLIKE)                                           
         DC    AL1(UADC),AL2(ADCLIKE)                                           
         DC    AL1(UWSC),AL2(WSMLIKE)                                           
         DC    AL1(UWSM),AL2(WSMLIKE)                                           
         DC    AL1(UCBL),AL2(CBLLIKE)                                           
         DC    AL1(USCB),AL2(CBLLIKE)                                           
         DC    AL1(ULCB),AL2(LCBLIKE)                                           
         DC    AL1(UACB),AL2(LCBLIKE)                                           
         DC    AL1(URNT),AL2(PERUSE)                                            
         DC    AL1(UBSM),AL2(BSMLIKE)                                           
         DC    AL1(UIMS),AL2(BSMLIKE)                                           
         DC    AL1(UMUS),AL2(PERUSE)                                            
         DC    AL1(UMRR),AL2(PERUSE)                                            
         DC    AL1(UNBM),AL2(PERUSE)                                            
         DC    AL1(UFMU),AL2(PERUSE)                                            
         DC    AL1(URLO),AL2(PERUSE)                                            
         DC    AL1(UMVI),AL2(PERUSE)                                            
         DC    AL1(UMVN),AL2(PERUSE)                                            
         DC    AL1(UPRM),AL2(PERUSE)                                            
         DC    AL1(UPRR),AL2(PERUSE)                                            
         DC    AL1(USMI),AL2(PERUSE)                                            
         DC    AL1(USMN),AL2(PERUSE)                                            
         DC    AL1(UIDS),AL2(PERHOUR)                                           
         DC    AL1(UDIO),AL2(PERPRHR)                                           
         DC    AL1(URTK),AL2(RTKLIKE)                                           
         DC    AL1(UIVR),AL2(RTKLIKE)                                           
         DC    AL1(USTR),AL2(PERUSE)                                            
         DC    AL1(UDOR),AL2(PERUSE)                                            
         DC    AL1(UFGM),AL2(PERUSE)                                            
         DC    AL1(UBSR),AL2(BSRLIKE)                                           
         DC    AL1(URRN),AL2(RRNLIKE)                                           
         DC    AL1(UTAG),AL2(TAGLIKE)                                           
         DC    AL1(UADT),AL2(BSSLIKE)                                           
         DC    AL1(UADH),AL2(PERUSE)                                            
         DC    AL1(UADO),AL2(BSRLIKE)                                           
         DC    AL1(UARN),AL2(PERUSE)                                            
         DC    AL1(UREN),AL2(PERUSE)                                            
         DC    AL1(USRE),AL2(PERUSE)                                            
         DC    AL1(UDWN),AL2(PERUSE)                                            
         DC    AL1(UCNL),AL2(PERUSE)                                            
         DC    AL1(USCN),AL2(PERUSE)                                            
         DC    AL1(UPPF),AL2(PERUSE)                                            
         DC    AL1(UINS),AL2(BSSLIKE)                                           
         DC    AL1(UFGS),AL2(BSSLIKE)                                           
         DC    AL1(USFS),AL2(BSSLIKE)                                           
         DC    AL1(UPUB),AL2(BSSLIKE)                                           
         DC    AL1(UPBS),AL2(PERUSE)                                            
         DC    AL1(UINR),AL2(PERUSE)                                            
         DC    AL1(USIN),AL2(PERUSE)                                            
         DC    AL1(UFGR),AL2(PERUSE)                                            
         DC    AL1(USFR),AL2(PERUSE)                                            
******** DC    AL1(USOP)                                                        
******** DC    AL1(USOR)                                                        
******** DC    AL1(USRH)                                                        
******** DC    AL1(USOC)                                                        
******** DC    AL1(USON)                                                        
******** DC    AL1(USPP)                                                        
******** DC    AL1(USDR)                                                        
******** DC    AL1(USDH)                                                        
******** DC    AL1(USDC)                                                        
******** DC    AL1(USDN)                                                        
******** DC    AL1(USDP)                                                        
******** DC    AL1(USWR)                                                        
******** DC    AL1(USWH)                                                        
******** DC    AL1(USWC)                                                        
******** DC    AL1(USWN)                                                        
******** DC    AL1(USWP)                                                        
         DC    AL1(UCNM),AL2(PERUSE)                                            
         DC    AL1(UMVM),AL2(PERUSE)                                            
         DC    AL1(UNIM),AL2(PERUSE)                                            
         DC    AL1(UIHM),AL2(PERUSE)                                            
         DC    AL1(UINU),AL2(PERUSE)                                            
         DC    AL1(UNMU),AL2(PERUSE)                                            
         DC    AL1(USIU),AL2(PERUSE)                                            
         DC    AL1(USNU),AL2(PERUSE)                                            
         DC    AL1(UVRE),AL2(VRELIKE)                                           
         DC    AL1(UVNR),AL2(VNRLIKE)                                           
         DC    AL1(UEDS),AL2(PERUSE)                                            
         DC    AL1(UEQY),AL2(PERUSE)                                            
         DC    AL1(UMBO),AL2(PERUSE)                                            
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS A(NEXT TYPE) IN AUTYP                        *         
***********************************************************************         
                                                                                
GETUTY   NTR1  BASE=*,LABEL=*                                                   
         USING USETABD,R2                                                       
         L     R2,AUSE                                                          
         ZIC   RE,USEDSP                                                        
                                                                                
         USING USESUBD,R3                                                       
         LR    R3,R2                                                            
         AR    R3,RE                                                            
                                                                                
         L     R4,AUTYP                                                         
                                                                                
GUT10    CR    R3,R4                                                            
         JNH   GUT40                                                            
         CLI   0(R3),0                                                          
         JE    NO                                                               
         TM    USETYST,UPGRADE                                                  
         JO    GUT40                                                            
                                                                                
         CLI   USEEQU,UTAG                                                      
         JNE   GUT20                                                            
                                                                                
GUT20    TM    USESTAT2,HLDTYPE                                                 
         JZ    GUT30                                                            
         CLI   USETYPE,0                                                        
         JNE   GUT40                                                            
                                                                                
GUT30    GOTO1 USEVAL,DMCB,(X'80',USEEQU),USETYPE                               
                                                                                
         MVC   TGBYTE,TGUSXUNI                                                  
         NC    TGBYTE,TGUNEQU                                                   
         JNZ   NO                                                               
                                                                                
         MVC   SVUSSTA2,TGUSSTA2                                                
         MVI   TGUPFRTY,0                                                       
         ST    R3,AUTYP                                                         
         J     YES                                                              
                                                                                
GUT40    IC    RE,USESBLN                                                       
         AR    R3,RE                                                            
         J     GUT10                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS A(NEXT MAJOR) IN MAJORS                      *         
***********************************************************************         
                                                                                
GETMAJ   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AMAJ                                                          
         LTR   R3,R3                                                            
         JNZ   GMAJ10                                                           
         LA    R3,MAJTAB                                                        
         J     GMAJ20                                                           
                                                                                
GMAJ10   TM    TGUSTYST,MAJORS                                                  
         JZ    NO                                                               
         CLI   1(R3),X'FF'                                                      
         JE    NO                                                               
         LA    R3,1(R3)                                                         
                                                                                
GMAJ20   ST    R3,AMAJ                                                          
         GOTO1 MAJVAL,DMCB,(X'80',0(R3))                                        
         J     YES                                                              
                                                                                
***********************************************************************         
*        MAJOR TABLE                                                  *         
***********************************************************************         
                                                                                
MAJTAB   DC    AL1(0)                                                           
         DC    AL1(NY)                                                          
         DC    AL1(CHI)                                                         
         DC    AL1(LA)                                                          
         DC    AL1(NY+CHI)                                                      
         DC    AL1(NY+LA)                                                       
         DC    AL1(CHI+LA)                                                      
         DC    AL1(NY+CHI+LA)                                                   
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS NEXT ADDENDUM STATS IN ADSTATE               *         
***********************************************************************         
                                                                                
GETADS   NTR1  BASE=*,LABEL=*                                                   
         TM    TGUSSTA3,ADDENUSE                                                
         JO    GADS10                                                           
         OC    ADSTATE,ADSTATE                                                  
         JZ    NO                                                               
         XC    ADSTATE,ADSTATE                                                  
         J     YES                                                              
                                                                                
GADS10   CLC   ADSTATE,=2X'FF'                                                  
         JNE   GADS20                                                           
         MVC   ADSTATE,=C'GA'                                                   
         J     YES                                                              
                                                                                
GADS20   CLC   ADSTATE,=C'GA'                                                   
         JNE   GADS30                                                           
         MVC   ADSTATE,=C'KS'                                                   
         J     YES                                                              
                                                                                
GADS30   CLC   ADSTATE,=C'KS'                                                   
         JNE   GADS40                                                           
         MVC   ADSTATE,=C'NW'                                                   
         J     YES                                                              
                                                                                
GADS40   CLC   ADSTATE,=C'NW'                                                   
         JNE   NO                                                               
         MVC   ADSTATE,=C'TX'                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS A(NEXT MEDIA) IN AMEDIA                      *         
***********************************************************************         
                                                                                
GETMED   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AMEDIA                                                        
         LTR   R3,R3                                                            
         JNZ   GM10                                                             
         LA    R3,MEDTAB                                                        
         J     GM20                                                             
                                                                                
GM10     CLI   1(R3),X'FF'                                                      
         JE    NO                                                               
         LA    R3,1(R3)                                                         
                                                                                
GM20     MVC   TGBYTE,TGUSTYMD                                                  
         NC    TGBYTE,0(R3)                                                     
         JZ    GM10                                                             
         GOTO1 MEDVAL,DMCB,(X'80',0(R3))                                        
         ST    R3,AMEDIA                                                        
         J     YES                                                              
                                                                                
***********************************************************************         
*        MEDIA TABLE                                                  *         
***********************************************************************         
                                                                                
MEDTAB   DC    AL1(TV)                                                          
         DC    AL1(RADIO)                                                       
         DC    AL1(CABLE)                                                       
         DC    AL1(PRINT)                                                       
         DC    AL1(INTERNET)                                                    
         DC    AL1(NEWMEDIA)                                                    
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS A(NEXT COMMERCIAL TYPE) IN ACTY OR A(NEXT    *         
*        ACTRA TYPE) IN AATY                                          *         
***********************************************************************         
                                                                                
GETTYP   NTR1  BASE=*,LABEL=*                                                   
         XC    TGCTEQU,TGCTEQU                                                  
         XC    TGCTNAME,TGCTNAME                                                
                                                                                
         XC    TGCCTCDE,TGCCTCDE                                                
         XC    TGCCTEQU,TGCCTEQU                                                
         XC    TGCCTNM,TGCCTNM                                                  
                                                                                
         TM    TGUSSTAT,CANUSE                                                  
         JZ    GTYP10                                                           
         BAS   RE,GETATY                                                        
         J     XIT                                                              
                                                                                
GTYP10   BAS   RE,GETCTY                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE RETURNS A(NEXT ACTRA TYPE) IN AATY                   *         
***********************************************************************         
                                                                                
GETATY   NTR1                                                                   
         USING CCTYPD,R3                                                        
         L     R3,AATY                                                          
         LTR   R3,R3                                                            
         JNZ   GATY10                                                           
         L     R3,TGACTYPS                                                      
         J     GATY20                                                           
                                                                                
GATY10   LA    R3,CCTYPNXT                                                      
         CLI   0(R3),X'FF'                                                      
         JE    NO                                                               
                                                                                
GATY20   CLI   FLTATY,0                                                         
         JE    GATY20                                                           
         CLC   FLTATY,CCTYPEQU                                                  
         JNE   GATY10                                                           
                                                                                
         GOTO1 CCTYPVAL,DMCB,CCTYPCDE                                           
         MVC   SVTYPNAM,SPACES                                                  
         MVC   SVTYPNAM,CCTYPNME                                                
         ST    R3,AATY                                                          
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE RETURNS A(NEXT COMMERCIAL TYPE) IN ACTY              *         
***********************************************************************         
                                                                                
GETCTY   NTR1                                                                   
         USING CTYD,R3                                                          
         L     R3,ACTY                                                          
         LTR   R3,R3                                                            
         JNZ   GCTY10                                                           
                                                                                
         GOTO1 VALTYP,DMCB,0                                                    
         JNE   GCTY20                                                           
         XC    TGCTEQU,TGCTEQU                                                  
         MVC   SVTYPNAM,SPACES                                                  
         MVC   SVTYPNAM(8),=C'STANDARD'                                         
         MVC   ACTY,=4X'FF'                                                     
         J     YES                                                              
                                                                                
GCTY10   C     R3,=4X'FF'                                                       
         JNE   GCTY30                                                           
GCTY20   L     R3,TGACOMT                                                       
         J     GCTY40                                                           
                                                                                
GCTY30   LA    R3,CTYNEXT                                                       
         CLI   0(R3),X'FF'                                                      
         JE    NO                                                               
                                                                                
GCTY40   GOTO1 VALTYP,DMCB,CTYEQU                                               
         JNE   GCTY30                                                           
         GOTO1 CTYPVAL,DMCB,CTYEQU                                              
         MVC   SVTYPNAM,SPACES                                                  
         MVC   SVTYPNAM(L'CTYNAME),CTYNAME                                      
         ST    R3,ACTY                                                          
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF DEMOS FOR DEMO PAYMENT                *         
***********************************************************************         
                                                                                
         USING UNITTABD,R2                                                      
CLCDEMO  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLPDEMO)                                            
         JNE   NO                                                               
         MVI   TCDEMO,1                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF DEMOS FOR DEMO PAYMENT                *         
***********************************************************************         
                                                                                
         USING UNITTABD,R2                                                      
CLCDEM5  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLPDEM5)                                            
         JNE   NO                                                               
         MVI   TCDEMO,5                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS 2 DEMOS FOR DEMO PAYMENT                        *         
***********************************************************************         
                                                                                
CLCDEM2  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLDEM2)                                             
         JNE   NO                                                               
         MVI   TCDEMO,2                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS 3 DEMOS FOR DEMO PAYMENT                        *         
***********************************************************************         
                                                                                
CLCDEMA  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLDEMA)                                             
         JNE   NO                                                               
         MVI   TCDEMO,3                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSSLIKE SPOT/DAY    *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCSD    NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLSD)                                               
         JNE   YES                                                              
         MVI   TASDSP,1                                                         
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSSLIKE OT HOUR     *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCOT    NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLOT)                                               
         JNE   YES                                                              
         CLC   TGONOF,=C'OFF'                                                   
         JE    NO                                                               
         MVI   TASDOT,1                                                         
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSSLIKE DOUBLETIME  *         
*        HOUR                                                         *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCDT    NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLDT)                                               
         JNE   YES                                                              
         CLC   TGONOF,=C'OFF'                                                   
         JE    NO                                                               
         MVI   TASDDT,1                                                         
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSSLIKE TRAVEL HOUR *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCTRVH  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLTRVH)                                             
         JNE   YES                                                              
         MVC   TASDTRV,=H'100'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSSLIKE TRAVEL TIME *         
*        15 MINUTE INCREMENT                                          *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCTRV15 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLTRV15)                                            
         JNE   YES                                                              
         MVC   TASDTRV,=H'125'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSSLIKE PRIOR DAY   *         
*        WARDROBE HOUR                                                *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCPDWH  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLPDWH)                                             
         JNE   YES                                                              
         CLC   TGONOF,=C'OFF'                                                   
         JE    NO                                                               
         MVC   TASDPDW,=H'100'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSSLIKE PRIOR DAY   *         
*        WARDROBE 15 MINUTE INCREMENT                                 *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCPDW15 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLPDW15)                                            
         JNE   YES                                                              
         CLC   TGONOF,=C'OFF'                                                   
         JE    NO                                                               
         MVC   TASDPDW,=H'115'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSRLIKE SPOT/DAY    *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCRSD   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLRSD)                                              
         JNE   YES                                                              
         MVI   TASDRSP,1                                                        
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSRLIKE EACH 90     *         
*        MINUTES                                                      *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCR90M  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLR90M)                                             
         JNE   YES                                                              
         MVC   TASDRHM,=H'130'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSSLIKE 1ST TAG     *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCRTG1  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLRTG1)                                             
         JNE   YES                                                              
         MVI   TASDRTG,1                                                        
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSSLIKE ADDITIONAL  *         
*        TAG                                                          *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCRTGA  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLRTGA)                                             
         JNE   YES                                                              
         MVI   TASDRTG,2                                                        
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSMLIKE SPOT/DAY    *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCBSMSD NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLBSMSD)                                            
         JNE   YES                                                              
         MVI   TASDMSP,1                                                        
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSMLIKE 1ST HOUR    *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCBSM1  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLBSM1)                                             
         JNE   YES                                                              
         MVC   TASDMHM,=H'100'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR BSMLIKE ADDITIONAL  *         
*        20 MINUTES                                                   *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCBSM20 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLBSM20)                                            
         JNE   YES                                                              
         MVC   TASDMHM,=H'120'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR PER HOUR            *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCHOUR  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLHOUR)                                             
         JNE   YES                                                              
         MVC   TASDMHM,=H'100'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR RETAKE HOUR         *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCRTKH  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLRTKH)                                             
         JNE   YES                                                              
         MVC   TASDIHM,=H'100'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR RETAKE OT HOUR      *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCRTKO  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLRTKO)                                             
         JNE   YES                                                              
         MVC   TASDIHM,=H'900'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR RETAKE 10TH HOUR    *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCRTK10 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLRTK10)                                            
         JNE   YES                                                              
         MVC   TASDIHM,=H'1000'                                                 
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR RETAKE DT HOUR      *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCRTKD  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLRTKD)                                             
         JNE   YES                                                              
         MVC   TASDIHM,=H'1100'                                                 
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR RETAKE TRAVEL TIME  *         
*        HOUR                                                         *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCRTKT  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLRTKT)                                             
         JNE   YES                                                              
         MVC   TASDIHM,=H'100'                                                  
         MVC   TASDITR,=H'100'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR IVR 1ST HOUR        *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCIVRH  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLIVRH)                                             
         JNE   YES                                                              
         MVC   TASDIVH,=H'100'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR IVR ADDITIONAL      *         
*        30 MINUTES                                                   *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCIVR30 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLIVR30)                                            
         JNE   YES                                                              
         MVC   TASDIVH,=H'130'                                                  
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR IVR 30 MINUTES      *         
*        OF TRAVEL                                                    *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCIVRT  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLIVRT)                                             
         JNE   YES                                                              
         MVC   TASDIVH,=H'100'                                                  
         MVC   TASDIVT,=H'30'                                                   
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CLASS A 1ST USE              *         
***********************************************************************         
                                                                                
CLCCLA1  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLCLA1)                                             
         JNE   NO                                                               
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CLASS A 2ND USE              *         
***********************************************************************         
                                                                                
CLCCLA2  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLCLA2)                                             
         JNE   NO                                                               
         MVC   TCNUSES,=H'1'                                                    
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CLASS A 3RD USE              *         
***********************************************************************         
                                                                                
CLCCLA3  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLCLA3)                                             
         JNE   NO                                                               
         MVC   TCNUSES,=H'2'                                                    
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CLASS A 4TH-13TH USE         *         
***********************************************************************         
                                                                                
CLCCLA4  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLCLA4)                                             
         JNE   NO                                                               
         MVC   TCNUSES,=H'3'                                                    
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CLASS A 14TH+ USE            *         
***********************************************************************         
                                                                                
CLCCLA14 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLCLA14)                                            
         JNE   NO                                                               
         MVC   TCNUSES,=H'13'                                                   
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR PAX USE                      *         
***********************************************************************         
                                                                                
CLCPAX   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLPAX)                                              
         JNE   NO                                                               
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR ITN USE 1                    *         
***********************************************************************         
                                                                                
CLCITN1  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLITN1)                                             
         JNE   NO                                                               
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR ITN USE 2                    *         
***********************************************************************         
                                                                                
CLCITN2  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLITN2)                                             
         JNE   NO                                                               
         MVC   TCUNITS,=H'1'                                                    
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR ITN USE 3                    *         
***********************************************************************         
                                                                                
CLCITN3  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLITN3)                                             
         JNE   NO                                                               
         MVC   TCUNITS,=H'2'                                                    
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR ITN USES 4-13                *         
***********************************************************************         
                                                                                
CLCITN4  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLITN4)                                             
         JNE   NO                                                               
         MVC   TCUNITS,=H'3'                                                    
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR ITN USE 14+                  *         
***********************************************************************         
                                                                                
CLCITN14 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLITN14)                                            
         JNE   NO                                                               
         MVC   TCUNITS,=H'13'                                                   
         MVC   TCTUSES,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR WSM USES 1-50                *         
***********************************************************************         
                                                                                
CLCWSM   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLWSM)                                              
         JNE   NO                                                               
         LH    RE,SVUNITS                                                       
         AHI   RE,1                                                             
         STH   RE,SVUNITS                                                       
         STH   RE,TCUNITS                                                       
                                                                                
         CHI   RE,5                                                             
         JH    CWSM10                                                           
         MVC   SVDESC(12),=C'FOR USES 1-5'                                      
         J     YES                                                              
                                                                                
CWSM10   MVC   SVDESC(12),=C'FOR USE'                                           
         EDIT  TCUNITS,(2,SVDESC+UTWSMUSE-UTWSM),ALIGN=LEFT                     
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR WSM USE 51+                  *         
***********************************************************************         
                                                                                
CLCWSM51 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLWSM51)                                            
         JNE   NO                                                               
         MVC   TCUNITS,=H'51'                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USES 1-60              *         
***********************************************************************         
                                                                                
CLCC1    NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC1)                                               
         JNE   NO                                                               
         NI    TGUSTYST,X'FF'-UPGRADE                                           
         MVC   TCUNITS,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 57                 *         
***********************************************************************         
                                                                                
CLCC57   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC57)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'57'                                                   
                                                                                
         OI    TGUSTYST,UPGRADE                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'56'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 58                 *         
***********************************************************************         
                                                                                
CLCC58   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC58)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'58'                                                   
                                                                                
         OI    TGUSTYST,UPGRADE                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'57'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 59-60              *         
***********************************************************************         
                                                                                
CLCC59   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC59)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'59'                                                   
                                                                                
         OI    TGUSTYST,UPGRADE                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'58'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 61                 *         
***********************************************************************         
                                                                                
CLCC61   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC61)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'61'                                                   
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'60'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 62                 *         
***********************************************************************         
                                                                                
CLCC62   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC62)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'62'                                                   
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'61'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 63-65              *         
***********************************************************************         
                                                                                
CLCC63   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC63)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'63'                                                   
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'62'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 66                 *         
***********************************************************************         
                                                                                
CLCC66   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC66)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'66'                                                   
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'65'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 67-69              *         
***********************************************************************         
                                                                                
CLCC67   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC67)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'67'                                                   
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'66'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 70                 *         
***********************************************************************         
                                                                                
CLCC70   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC70)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'70'                                                   
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'69'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 71-100             *         
***********************************************************************         
                                                                                
CLCC71   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC71)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'71'                                                   
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'70'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 101-150            *         
***********************************************************************         
                                                                                
CLCC101  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC101)                                             
         JNE   NO                                                               
         MVC   TCUNITS,=H'101'                                                  
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'100'                                                 
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 151-200            *         
***********************************************************************         
                                                                                
CLCC151  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC151)                                             
         JNE   NO                                                               
         MVC   TCUNITS,=H'151'                                                  
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'150'                                                 
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 201-1000           *         
***********************************************************************         
                                                                                
CLCC201  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC201)                                             
         JNE   NO                                                               
         MVC   TCUNITS,=H'201'                                                  
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'200'                                                 
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 1001-2500          *         
***********************************************************************         
                                                                                
CLCC1001 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC1001)                                            
         JNE   NO                                                               
         MVC   TCUNITS,=H'1001'                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'1000'                                                
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF USES FOR CABLE USE 2501-3000          *         
***********************************************************************         
                                                                                
CLCC2501 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLC2501)                                            
         JNE   NO                                                               
         MVC   TCUNITS,=H'2501'                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHCBUN,=H'2500'                                                
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF LOCAL CABLE SUBSCRIBERS TO 1-50K      *         
***********************************************************************         
                                                                                
CLCL1    NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLL1)                                               
         JNE   NO                                                               
         CLI   TGUSTYP,ULCB50                                                   
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF LOCAL CABLE SUBSCRIBERS TO 50,001-    *         
*        100K                                                         *         
***********************************************************************         
                                                                                
CLCL50K  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLL50K)                                             
         JNE   NO                                                               
         CLI   TGUSTYP,ULCB100                                                  
         JNE   NO                                                               
         MVI   TGUPFRTY,ULCB50                                                  
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF LOCAL CABLE SUBSCRIBERS TO 100,001-   *         
*        150K                                                         *         
***********************************************************************         
                                                                                
CLCL100K NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLL100K)                                            
         JNE   NO                                                               
         CLI   TGUSTYP,ULCB150                                                  
         JNE   NO                                                               
         MVI   TGUPFRTY,ULCB100                                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF LOCAL CABLE SUBSCRIBERS TO 150,001-   *         
*        200K                                                         *         
***********************************************************************         
                                                                                
CLCL150K NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLL150K)                                            
         JNE   NO                                                               
         CLI   TGUSTYP,ULCB200                                                  
         JNE   NO                                                               
         MVI   TGUPFRTY,ULCB150                                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF LOCAL CABLE SUBSCRIBERS TO 200,001-   *         
*        250K                                                         *         
***********************************************************************         
                                                                                
CLCL200K NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLL200K)                                            
         JNE   NO                                                               
         CLI   TGUSTYP,ULCB250                                                  
         JNE   NO                                                               
         MVI   TGUPFRTY,ULCB200                                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF LOCAL CABLE SUBSCRIBERS TO 250,001-   *         
*        500K                                                         *         
***********************************************************************         
                                                                                
CLCL250K NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLL250K)                                            
         JNE   NO                                                               
         CLI   TGUSTYP,ULCB500                                                  
         JNE   NO                                                               
         MVI   TGUPFRTY,ULCB250                                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF LOCAL CABLE SUBSCRIBERS TO 500,001-   *         
*        750K                                                         *         
***********************************************************************         
                                                                                
CLCL500K NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLL500K)                                            
         JNE   NO                                                               
         CLI   TGUSTYP,ULCB750                                                  
         JNE   NO                                                               
         MVI   TGUPFRTY,ULCB500                                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF LOCAL CABLE SUBSCRIBERS TO 750,001-   *         
*        1M                                                           *         
***********************************************************************         
                                                                                
CLCL750K NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLL750K)                                            
         JNE   NO                                                               
         CLI   TGUSTYP,ULCB1M                                                   
         JNE   NO                                                               
         MVI   TGUPFRTY,ULCB750                                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF LOCAL CABLE SUBSCRIBERS TO 1M+        *         
***********************************************************************         
                                                                                
CLCL1M   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLL1M)                                              
         JNE   NO                                                               
         CLI   TGUSTYP,ULCBMAX                                                  
         JNE   NO                                                               
         MVI   TGUPFRTY,ULCB1M                                                  
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS WILDSPOT MAJORS                                 *         
***********************************************************************         
                                                                                
CLCWMAJ  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLWMAJ)                                             
         JNE   NO                                                               
         NI    TGUSTYST,X'FF'-UPGRADE                                           
         TM    TGUSTYST,MAJORS                                                  
         JZ    NO                                                               
         L     RE,AMAJ                                                          
         MVC   TCMAJORS,0(RE)                                                   
         MVC   SVDESC+UTWMAJ-UTWM(L'TGMACHAR),TGMACHAR                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS UNIT 1 WITH MAJOR                               *         
***********************************************************************         
                                                                                
CLCWM1   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLWM1)                                              
         JNE   NO                                                               
         NI    TGUSTYST,X'FF'-UPGRADE                                           
         TM    TGUSTYST,MAJORS                                                  
         JZ    NO                                                               
         L     RE,AMAJ                                                          
         CLI   0(RE),0                                                          
         JE    NO                                                               
         MVC   TCMAJORS,0(RE)                                                   
         MVC   SVDESC+UTWMA1-UTWM1(L'TGMACHAR),TGMACHAR                         
                                                                                
         MVC   TCUNITS,=H'1'                                                    
         OI    TGUSTYST,UPGRADE                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHUNT,=H'0'                                                    
         MVC   TAUHMAJ,TCMAJORS                                                 
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS UNIT 36 WITH MAJOR                              *         
***********************************************************************         
                                                                                
CLCWM36  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLWM36)                                             
         JNE   NO                                                               
         NI    TGUSTYST,X'FF'-UPGRADE                                           
         TM    TGUSTYST,MAJORS                                                  
         JZ    NO                                                               
         L     RE,AMAJ                                                          
         CLI   0(RE),0                                                          
         JE    NO                                                               
         MVC   TCMAJORS,0(RE)                                                   
         MVC   SVDESC+UTWMA36-UTWM36(L'TGMACHAR),TGMACHAR                       
                                                                                
         MVC   TCUNITS,=H'36'                                                   
         OI    TGUSTYST,UPGRADE                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHUNT,=H'35'                                                   
         MVC   TAUHMAJ,TCMAJORS                                                 
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS UNIT 101 WITH MAJOR                             *         
***********************************************************************         
                                                                                
CLCWM101 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLWM101)                                            
         JNE   NO                                                               
         NI    TGUSTYST,X'FF'-UPGRADE                                           
         TM    TGUSTYST,MAJORS                                                  
         JZ    NO                                                               
         L     RE,AMAJ                                                          
         CLI   0(RE),0                                                          
         JE    NO                                                               
         MVC   TCMAJORS,0(RE)                                                   
         MVC   SVDESC+UTWMA101-UTWM101(L'TGMACHAR),TGMACHAR                     
                                                                                
         MVC   TCUNITS,=H'101'                                                  
         OI    TGUSTYST,UPGRADE                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHUNT,=H'100'                                                  
         MVC   TAUHMAJ,TCMAJORS                                                 
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF WILDSPOT UNITS TO 1                   *         
***********************************************************************         
                                                                                
CLCW1    NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLW1)                                               
         JNE   NO                                                               
         L     RE,AMAJ                                                          
         CLI   0(RE),0                                                          
         JNE   NO                                                               
                                                                                
CLCW110  MVC   TCUNITS,=H'1'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF WILDSPOT UNITS TO 2                   *         
***********************************************************************         
                                                                                
CLCW2    NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLW2)                                               
         JNE   NO                                                               
         L     RE,AMAJ                                                          
         CLI   0(RE),0                                                          
         JNE   NO                                                               
                                                                                
         MVC   TCUNITS,=H'2'                                                    
                                                                                
         OI    TGUSTYST,UPGRADE                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHUNT,=H'1'                                                    
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF WILDSPOT UNITS TO 26                  *         
***********************************************************************         
                                                                                
CLCW26   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLW26)                                              
         JNE   NO                                                               
         L     RE,AMAJ                                                          
         CLI   0(RE),0                                                          
         JNE   NO                                                               
                                                                                
         MVC   TCUNITS,=H'26'                                                   
                                                                                
         OI    TGUSTYST,UPGRADE                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHUNT,=H'25'                                                   
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF WILDSPOT UNITS TO 61                  *         
***********************************************************************         
                                                                                
CLCW61   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLW61)                                              
         JNE   NO                                                               
         L     RE,AMAJ                                                          
         CLI   0(RE),0                                                          
         JNE   NO                                                               
                                                                                
         MVC   TCUNITS,=H'61'                                                   
                                                                                
         OI    TGUSTYST,UPGRADE                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHUNT,=H'60'                                                   
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF WILDSPOT UNITS TO 126                 *         
***********************************************************************         
                                                                                
CLCW126  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLW126)                                             
         JNE   NO                                                               
         L     RE,AMAJ                                                          
         CLI   0(RE),0                                                          
         JNE   NO                                                               
                                                                                
         MVC   TCUNITS,=H'126'                                                  
                                                                                
         OI    TGUSTYST,UPGRADE                                                 
                                                                                
         USING TAUHD,RE                                                         
         LA    RE,TCTAUHEL                                                      
         MVC   TAUHUNT,=H'125'                                                  
         J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF SNW UNITS TO 2                        *         
***********************************************************************         
                                                                                
CLCSNW2  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLSNW2)                                             
         JNE   NO                                                               
         MVC   TCUNITS,=H'2'                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF ADC UNITS TO 25                       *         
***********************************************************************         
                                                                                
CLCA25   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLA25)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'25'                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF ADC UNITS TO 26                       *         
***********************************************************************         
                                                                                
CLCA26   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLA26)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'26'                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF ADC UNITS TO 60                       *         
***********************************************************************         
                                                                                
CLCA60   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLA60)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'60'                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF ADC UNITS TO 61                       *         
***********************************************************************         
                                                                                
CLCA61   NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLA61)                                              
         JNE   NO                                                               
         MVC   TCUNITS,=H'61'                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF ADC UNITS TO 125                      *         
***********************************************************************         
                                                                                
CLCA125  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLA125)                                             
         JNE   NO                                                               
         MVC   TCUNITS,=H'125'                                                  
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF ADC UNITS TO 126                      *         
***********************************************************************         
                                                                                
CLCA126  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLA126)                                             
         JNE   NO                                                               
         MVC   TCUNITS,=H'126'                                                  
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR TAGLIKE/BSSLIKE     *         
*        1ST TAG AT SESSION RATE                                      *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCTAGS  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLTAGS)                                             
         JNE   YES                                                              
         CLI   TGUSEQU,UTAG                                                     
         JNE   CTAGS10                                                          
         CLI   TGUSTYP,UTAGSESS                                                 
         JNE   YES                                                              
CTAGS10  MVI   TASDTAG,1                                                        
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR TAGLIKE/BSSLIKE     *         
*        PER TAG 1-24/1-25/2-25                                       *         
***********************************************************************         
                                                                                
         USING TASDD,R3                                                         
CLCTAG12 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLTAG1)                                             
         JNE   CLCTAG2                                                          
         CLI   TGUSEQU,UTAG                                                     
         JNE   CTAG110                                                          
         CLI   TGUSTYP,UTAGREG                                                  
         JNE   NO                                                               
CTAG110  MVI   TASDTAG,1                                                        
         J     YES                                                              
                                                                                
CLCTAG2  CLC   UTLINE,=AL2(UTLTAG2)                                             
         JNE   YES                                                              
         CLI   TGUSEQU,UTAG                                                     
         JNE   CTAG210                                                          
         CLI   TGUSTYP,UTAGSESS                                                 
         JNE   NO                                                               
CTAG210  MVI   TASDTAG,1                                                        
         MVI   TCTAGS,2                                                         
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR TAGLIKE/BSSLIKE     *         
*        PER TAG 25-49/26-50                                          *         
***********************************************************************         
                                                                                
CLCTAG25 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLTAG25)                                            
         JNE   NO                                                               
         MVI   TCTAGS,26                                                        
         CLI   TGMEEQU,RADIO                                                    
         JE    YES                                                              
         MVI   TCTAGS,25                                                        
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS SESSION DETAILS ELEMENT FOR TAGLIKE/BSSLIKE     *         
*        PER TAG 50+/51+                                              *         
***********************************************************************         
                                                                                
CLCTAG50 NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLTAG50)                                            
         JNE   NO                                                               
         MVI   TCTAGS,51                                                        
         CLI   TGMEEQU,RADIO                                                    
         JE    YES                                                              
         MVI   TCTAGS,50                                                        
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NUMBER OF VARIATIONS FOR 1ST VNR USE            *         
***********************************************************************         
                                                                                
CLCVNR1  NTR1  BASE=*,LABEL=*                                                   
         CLC   UTLINE,=AL2(UTLVNR1)                                             
         JNE   NO                                                               
******** MVC   TCTUSES,=H'1'                                                    
         OI    TCPAYST2,TCVNR1U                                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
FLTUSE   DS    XL(L'TGUSEQU)       USE FILTER                                   
FLTATY   DS    XL(L'CCTYPEQU)      ACTRA TYPE FILTER                            
                                                                                
AUSE     DS    A                   A(USE TABLE ENTRY)                           
AUTYP    DS    A                   A(USE TABLE TYPE ENTRY)                      
AMAJ     DS    A                   A(MAJOR TABLE ENTRY)                         
AMEDIA   DS    A                   A(MEDIA TABLE ENTRY)                         
ACTY     DS    A                   A(COMMERCIAL TABLE ENTRY)                    
AATY     DS    A                   A(ACTRA TYPE ENTRY)                          
AART     DS    A                   A(AFM RATE)                                  
ACAT     DS    A                   A(CATEGORY TABLE ENTRY)                      
AUNIT    DS    A                   A(UNIT TABLE ENTRY)                          
                                                                                
SVUSSTA2 DS    XL(L'TGUSSTA2)      SAVED 2ND USE STATUS                         
AFMRATE  DS    CL(L'TACOAFM)                                                    
ADSTATE  DS    CL(L'TACOADST)      ADDENDUM STATE                               
                                                                                
SVTLKUP  DS    XL(L'ULTLKUP)       USE LOOKUP EQUATE FOR UNIT TABLE             
SVDESC   DS    CL(L'UTDESC)        LINE DESCRIPTION                             
                                                                                
ELTASD   DS    XL(TASDLNQ)         SESSION DETAILS ELEMENT                      
ELTACO   DS    XL(TACOLNQ2)        COMMERCIAL DETAILS ELEMENT                   
                                                                                
SVCATNAM DS    CL(L'CATNAME)       SAVED CATEGORY NAME                          
SVTYPNAM DS    CL(L'CCTYPNME)      SAVED ACTRA/COMMERCIAL TYPE NAME             
                                                                                
RATE     DS    CL12                RATE FROM SYSCALC                            
SVPAY    DS    F                   SAVED PAYMENT AMOUNT                         
SVUNITS  DS    H                   SAVED UNITS                                  
                                                                                
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
                                                                                
       ++INCLUDE TASYSCALCD                                                     
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
***********************************************************************         
*        DSECT COVER USE LOOKUP TABLE                                           
***********************************************************************         
                                                                                
USLUTABD DSECT                                                                  
ULTUSE   DS    AL1                 USE EQUATE                                   
ULTLKUP  DS    AL2                 USE LOOKUP EQUATE FOR UNIT TABLE             
BSSLIKE  EQU   1                   USE IS BSS-LIKE                              
PERUSE   EQU   2                   USE IS PER USE                               
PERDEMO  EQU   3                   USE IS PER DEMO                              
PERDEM2  EQU   4                   USE IS PER DEMO STARTING AT DEMO 2           
BSRLIKE  EQU   5                   USE IS BSR-LIKE                              
CLALIKE  EQU   6                   USE IS CLA-LIKE                              
PAXLIKE  EQU   7                   USE IS PAX-LIKE                              
ITNLIKE  EQU   8                   USE IS ITN-LIKE                              
WSMLIKE  EQU   9                   USE IS WSM-LIKE                              
CBLLIKE  EQU   10                  USE IS CBL-LIKE                              
LCBLIKE  EQU   11                  USE IS LCB-LIKE                              
BSMLIKE  EQU   12                  USE IS BSM-LIKE                              
PERHOUR  EQU   13                  USE IS PER HOUR                              
PERPRHR  EQU   14                  USE IS PER PROGRAM/HOUR                      
WSPLIKE  EQU   15                  USE IS WSP-LIKE                              
SNWLIKE  EQU   16                  USE IS SNW-LIKE                              
RRNLIKE  EQU   17                  USE IS RRN-LIKE                              
ADCLIKE  EQU   18                  USE IS ADC-LIKE                              
TAGLIKE  EQU   19                  USE IS TAG-LIKE                              
VNRLIKE  EQU   20                  USE IS VNR-LIKE                              
VRELIKE  EQU   21                  USE IS VRE-LIKE                              
RTKLIKE  EQU   22                  USE IS RTK-LIKE                              
IVRLIKE  EQU   23                  USE IS IVR-LIKE                              
ULTLNQ   EQU   *-USLUTABD                                                       
                                                                                
***********************************************************************         
*        DSECT COVERS UNIT TABLE                                                
***********************************************************************         
                                                                                
UNITTABD DSECT                                                                  
UTLKUP   DS    AL2                 USE LOOKUP EQUATE FOR UNIT TABLE             
UTLINE   DS    AL2                 LINE EQUATE                                  
UTLSD    EQU   1                   PER SPOT/DAY                                 
UTLOT    EQU   2                   PER OVERTIME HOUR                            
UTLDT    EQU   3                   PER DOUBLETIME HOUR                          
UTLTRVH  EQU   4                   FOR FIRST TRAVEL HOUR                        
UTLTRV15 EQU   5                   PER TRAVEL 15 MINUTE INCREMENT               
UTLPDWH  EQU   6                   FOR FIRST PDW HOUR                           
UTLPDW15 EQU   7                   PER PDW 15 MINUTE INCREMENT                  
UTLTAGS  EQU   8                   FOR 1ST TAG AT SESSION RATE                  
UTLTAG1  EQU   9                   PER TAG 1-24                                 
UTLTAG2  EQU   10                  PER TAG 2-25                                 
UTLTAG25 EQU   11                  PER TAG 25-49/26-50                          
UTLTAG50 EQU   12                  PER TAG 50+/51+                              
UTLPUSE  EQU   13                  PER USE                                      
UTLPDEMO EQU   14                  PER DEMO 1-4                                 
UTLDEM2  EQU   15                  FOR 2ND DEMO                                 
UTLDEMA  EQU   16                  FOR ADDITIONAL DEMO                          
UTLRSD   EQU   17                  RADIO PER SPOT/DAY                           
UTLR90M  EQU   18                  RADIO FOR EACH 90 MINUTES                    
UTLRTG1  EQU   19                  RADIO FOR FIRST TAG                          
UTLRTGA  EQU   20                  RADIO FOR ADDITIONAL TAG                     
UTLCLA1  EQU   21                  FOR 1ST CLASS A USE                          
UTLCLA2  EQU   22                  FOR 2ND CLASS A USE                          
UTLCLA3  EQU   23                  FOR 3RD CLASS A USE                          
UTLCLA4  EQU   24                  PER CLASS A USE 4-13                         
UTLCLA14 EQU   25                  PER CLASS A USE 14+                          
UTLPAX   EQU   26                  PER PAX USE                                  
UTLITN1  EQU   27                  FOR 1ST ITN USE                              
UTLITN2  EQU   28                  FOR 2ND ITN USE                              
UTLITN3  EQU   29                  FOR 3RD ITN USE                              
UTLITN4  EQU   30                  PER ITN USE 4-13                             
UTLITN14 EQU   31                  PER ITN USE 14+                              
UTLWSM   EQU   32                  FOR WSM USES 1-50                            
UTLWSM51 EQU   33                  PER WSM USE 51+                              
UTLC1    EQU   34                  FOR CABLE USE 1-56                           
UTLC57   EQU   35                  FOR CABLE USE 57                             
UTLC58   EQU   36                  FOR CABLE USE 58                             
UTLC59   EQU   37                  PER CABLE USE 59-60                          
UTLC61   EQU   38                  FOR CABLE USE 61                             
UTLC62   EQU   39                  PER CABLE USE 62                             
UTLC63   EQU   40                  PER CABLE USE 63-65                          
UTLC66   EQU   41                  PER CABLE USE 66                             
UTLC67   EQU   42                  PER CABLE USE 67-69                          
UTLC70   EQU   43                  PER CABLE USE 70                             
UTLC71   EQU   44                  PER CABLE USE 71-100                         
UTLL1    EQU   45                  FOR LOCAL CABLE USE 1-50K                    
UTLL50K  EQU   46                  FOR LOCAL CABLE USE 50,001-100K              
UTLL100K EQU   47                  FOR LOCAL CABLE USE 100,001-150K             
UTLL150K EQU   48                  FOR LOCAL CABLE USE 150,001-200K             
UTLL200K EQU   49                  FOR LOCAL CABLE USE 200,001-250K             
UTLL250K EQU   50                  FOR LOCAL CABLE USE 250,001-500K             
UTLL500K EQU   51                  FOR LOCAL CABLE USE 500,001-750K             
UTLL750K EQU   52                  FOR LOCAL CABLE USE 750,001-1M               
UTLL1M   EQU   53                  FOR LOCAL CABLE USE OVER 1M                  
UTLBSMSD EQU   54                  FOR 1ST HOUR                                 
UTLBSM1  EQU   55                  PER SPOT/DAY                                 
UTLBSM20 EQU   56                  FOR EACH ADDITIONAL 20 MINUTES               
UTLHOUR  EQU   57                  PER HOUR                                     
UTLWMAJ  EQU   58                  FOR MAJOR(S)                                 
UTLW1    EQU   59                  FOR WILDSPOT UNIT 1                          
UTLW2    EQU   60                  PER WILDSPOT UNITS 2-25                      
UTLW26   EQU   61                  PER WILDSPOT UNITS 26-60                     
UTLW61   EQU   62                  PER WILDSPOT UNITS 61-125                    
UTLW126  EQU   63                  PER WILDSPOT UNITS 126+                      
UTLSNW2  EQU   64                  PER SNW UNIT 2-255                           
UTLA25   EQU   65                  FOR ADC UNIT 25                              
UTLA26   EQU   66                  PER ADC UNITS 26-60                          
UTLA60   EQU   67                  FOR ADC UNIT 60                              
UTLA61   EQU   68                  PER ADC UNITS 61-125                         
UTLA125  EQU   69                  FOR ADC UNIT 125                             
UTLA126  EQU   70                  PER ADC UNITS 126+                           
UTLVNR1  EQU   71                  FOR FIRST VNR VARIATION                      
UTLRTKH  EQU   72                  PER RETAKE HOUR                              
UTLRTKO  EQU   73                  PER RETAKE OVERTIME HOUR                     
UTLRTK10 EQU   74                  PER RETAKE 2ND OVERTIME HOUR                 
UTLRTKD  EQU   75                  PER RETAKE DOUBLETIME HOUR                   
UTLRTKT  EQU   76                  PER RETAKE TRAVEL HOUR                       
UTLIVRH  EQU   77                  FOR IVR 1ST HOUR                             
UTLIVR30 EQU   78                  PER IVR ADDITIONAL 30 MINUTES                
UTLIVRT  EQU   79                  PER IVR 30 MINUTES OF TRAVEL TIME            
UTLPDEM5 EQU   80                  PER DEMO 5+                                  
UTLWM1   EQU   81                  PER UNIT 1-35 WITH MAJOR                     
UTLWM36  EQU   82                  PER UNIT 36-100 WITH MAJOR                   
UTLWM101 EQU   83                  PER UNIT 101+ WITH MAJOR                     
UTLC101  EQU   84                  PER CABLE USE 101-150                        
UTLC151  EQU   85                  PER CABLE USE 151-200                        
UTLC201  EQU   86                  PER CABLE USE 201-1000                       
UTLC1001 EQU   87                  PER CABLE USE 1001-2500                      
UTLC2501 EQU   88                  PER CABLE USE 2501-3000                      
UTDESC   DS    CL30                LINE DESCRIPTION                             
UTLNQ    EQU   *-UNITTABD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAREP6A   10/04/16'                                      
         END                                                                    
