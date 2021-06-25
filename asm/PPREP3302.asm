*          DATA SET PPREP3302  AT LEVEL 175 AS OF 05/01/02                      
*PHASE PP3302A,+0                                                               
         TITLE 'PP3302 - PRINTPAK PUB USAGE REPORT'                             
PP3302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP3302                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R9,PPFILEC                                                       
         LA    R4,1(R9)                                                         
         LA    R4,4095(R4)                                                      
         USING PPFILED,R9,R4                                                    
         LA    RC,SPACEND                                                       
         USING PP33WRK,RC                                                       
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         SPACE                                                                  
         CLI   MODE,PROCBUY                                                     
         BE    PRBUY                                                            
         CLI   MODE,LBUYPUB                                                     
         BE    PUBL                                                             
         CLI   MODE,FBUYREQ                                                     
         BE    REQF                                                             
         CLI   MODE,FBUYCLI                                                     
         BE    CLTF                                                             
         CLI   MODE,LBUYCLI                                                     
         BE    CLTL                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
         SPACE 2                                                                
*  PROGRAM WILL PRINT THREE DIFFERENT SECTIONS    *                             
*                                                 *                             
*  IF QOPT1=C  PGM WILL PRINT ALL 3 SECTIONS      *                             
*              1) PUBS PER CLIENT                 *                             
*              2) AGENCY PUB SUMMARY              *                             
*              3) SUMMARY TABLE                   *                             
*                                                 *                             
*  IF QOPT1=A  PGM WILL PRINT 2ND AND 3D SECTION  *                             
*              2) AGENCY PUB SUMMARY              *                             
*              3) SUMMARY TABLE                   *                             
*                                                 *                             
*  IF QOPT1=S  PGM WILL PRINT ONLY 3D SECTION     *                             
*              3) SUMMARY TABLE                   *                             
*                                                 *                             
         EJECT                                                                  
*        REQUEST FIRST                                                          
REQF     DS    0H                                                               
         MVI   FCRDBUY,X'21'       SET TO READ PUB POINTERS                     
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFFC                                      
*                                                                               
         XC    INSLIM,INSLIM       SET INSERTION LIMIT                          
         CLI   QOPT2,C' '                                                       
         BE    EXIT                                                             
         PACK  DUB,QOPT2(2)                                                     
         CVB   R0,DUB                                                           
         ST    R0,INSLIM                                                        
         B     EXIT                                                             
         SPACE 2                                                                
*        RUN FIRST                                                              
RUNF     DS    0H                                                               
         RELOC (R3)                                                             
         L     RF,=A(BUFFALOC)                                                  
         AR    RF,R3                                                            
         ST    RF,ABUFFC                                                        
         B     EXIT                                                             
         SPACE 2                                                                
*        RUN LAST                                                               
RUNL     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        CLIENT FIRST                                                           
CLTF     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        PRODUCT BUY                                                            
PRBUY    DS    0H                                                               
         L     RF,TGROSS                                                        
         A     RF,GROSS                                                         
         ST    RF,TGROSS                                                        
*                                                                               
         L     RF,TINS                                                          
         LA    RF,1(RF)                                                         
         ST    RF,TINS                                                          
*                                                                               
         MVC   SVPUBK,PBUYKPUB     STORE PUB KEY                                
         B     EXIT                                                             
         SPACE 2                                                                
*        LAST BUYPUB                                                            
PUBL     DS    0H                                                               
         CLC   TINS,=F'0'                                                       
         BE    EXIT                                                             
         MVC   BFKPUB,SVPUBK                                                    
         MVC   BFICTR,TINS                                                      
         MVC   BFCCTR,=F'1'                                                     
         MVC   BFGROSS,TGROSS                                                   
         SPACE                                                                  
         L     RF,CTPUB            ADD PUB TOTS TO CLT TOTS                     
         LA    RF,1(RF)                                                         
         ST    RF,CTPUB                                                         
         L     RF,CTINS                                                         
         A     RF,TINS                                                          
         ST    RF,CTINS                                                         
         L     RF,CTGRS                                                         
         A     RF,TGROSS                                                        
         ST    RF,CTGRS                                                         
         SPACE                                                                  
         XC    TINS,TINS                                                        
         XC    TGROSS,TGROSS                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,BFREC                                
         B     EXIT                                                             
         EJECT                                                                  
* THIS SECTION COMPUTES AND PRINTS PUBS PER CLIENT *                            
         SPACE                                                                  
*        CLIENT LAST                                                            
         SPACE                                                                  
CLTL     DS    0H                                                               
         XC    BFREC,BFREC                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,BFREC,1                             
         MVC   PLCLTC,PBUYKCLT                                                  
         MVC   PLCLTN,PCLTNAME                                                  
         B     CL4B                                                             
         SPACE                                                                  
CL4      DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,BFREC,1                              
         SPACE                                                                  
CL4B     DS    0H                                                               
         TM    DMCB+8,X'80'        TEST EOF                                     
         BNZ   CL20                                                             
         SPACE                                                                  
CL4BB    XC    KEY,KEY             ** SET KEY TO READ PUB REC **                
         MVC   KEY(1),PAGYKMED     MEDIA                                        
         MVC   KEY+1(6),BFKPUB     PUB CODE/ZONE/ED                             
         MVC   KEY+7(2),PAGYKAGY   AGENCY                                       
         MVI   KEY+9,X'81'         RECORD CODE                                  
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    CL4C                                                             
         MVC   PLPUBN(18),=C'NAME NOT AVAILABLE'                                
         B     CL4D                                                             
         SPACE                                                                  
CL4C     GOTO1 GETNAME                                                          
         MVC   PLPUBN,PUBNAME                                                   
         MVC   PLPUBN+132,PUBZNAME                                              
         OC    PLPUBN+132,SPACES                                                
         SPACE                                                                  
CL4D     MVC   PCODE(6),BFKPUB                                                  
         GOTO1 PUBEDIT,DMCB,(C'S',PCODE),PLPUBC                                 
         EDIT  (4,BFGROSS),(14,PLGROSS),2,                             X        
               COMMAS=YES,MINUS=YES,                                   X        
               FLOAT=$                                                          
         EDIT  (4,BFICTR),(7,PLINS)                                             
         CLI   RCSUBPRG,1          IS IT AGENCY TOTALS                          
         BNE   CL4E                                                             
         EDIT  (4,BFCCTR),(6,PLCLTOT)                                           
         B     RQL15                                                            
         SPACE                                                                  
CL4E     CLI   QOPT1,C'C'          IF QOPT1 NOT = C,                            
         BNE   CL4                 SKIP PUBS PER CLIENT REPORT.                 
         GOTO1 REPORT                                                           
         B     CL4                                                              
         SPACE                                                                  
CL20     DS   0H                                                                
         CLI   QOPT1,C'C'                                                       
         BNE   CL20A                                                            
         CLI   PLCLTC,C' '         IF CLT NAME IN P LINE, EXIT                  
         BNE   EXIT                SINCE THIS IS CLT WITH NO DATA.              
         XC    P,P                                                              
         GOTO1 REPORT              PRINT BLANK LINE BETWEEN CLIENTS             
         SPACE                                                                  
CL20A    L     RF,ATCLT            ADD CLT TOTS TO AGENCY TOTS                  
         LA    RF,1(RF)                                                         
         ST    RF,ATCLT                                                         
         L     RF,ATINS                                                         
         A     RF,CTINS                                                         
         ST    RF,ATINS                                                         
         CLI   QOPT1,C'C'                                                       
         BNE   CL20B                                                            
         SPACE                                                                  
         MVI   SPACING,3                                                        
         MVC   PLPUBC(12),=C'** TOTALS **'                                      
         EDIT  (4,CTPUB),(4,PLPUBN+5)                                           
         EDIT  (4,CTGRS),(14,PLGROSS),2,                               X        
               COMMAS=YES,FLOAT=$,MINUS=YES                                     
         EDIT  (4,CTINS),(7,PLINS)                                              
         GOTO1 REPORT                                                           
         SPACE                                                                  
CL20B    XC    CTPUB,CTPUB         CLEAR CLIENT TOTS                            
         XC    CTINS,CTINS                                                      
         XC    CTGRS,CTGRS                                                      
         LA    R3,1                                                             
         LA    R5,2                                                             
         O     R5,=X'80000000'                                                  
         GOTO1 BUFFALO,DMCB,=C'ADD',ABUFFC,(R3),(R5)                            
         O     R3,=X'80000000'                                                  
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFFC,(R3)                               
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
* THIS SECTION PRINTS AGENCY SUMMARY OF PUBS *                                  
         SPACE                                                                  
*        REQUEST LAST                                                           
         SPACE                                                                  
REQL     DS    0H                                                               
         MVI   RCSUBPRG,1          SET HEADER FOR AGENCY TOTALS                 
         MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACES                                                         
         MVC   P+132,SPACES                                                     
         XC    BFREC,BFREC                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,BFREC,2                             
         B     RQL10                                                            
         SPACE                                                                  
RQL05    GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,BFREC,2                              
         SPACE                                                                  
RQL10    DS    0H                                                               
         TM    DMCB+8,X'80'        TEST EOF                                     
         BO    TBL00                                                            
         L     RF,ATPUB            ADD UP PUB TOTAL                             
         LA    RF,1(RF)                                                         
         ST    RF,ATPUB                                                         
         CLI   QOPT1,C'S'          IF QOPT1=S,                                  
         BE    RQL05               SKIP AGENCY PUB SUMMARY REPORT               
         OC    INSLIM,INSLIM       TEST HAVE INSERTION LIMIT                    
         BE    CL4BB                                                            
         CLC   BFICTR,INSLIM                                                    
         BL    RQL05                                                            
         B     CL4BB                                                            
RQL15    GOTO1 REPORT                                                           
         B     RQL05                                                            
         EJECT                                                                  
* THIS SECTION COMPUTES TOTALS TABLE *                                          
         SPACE                                                                  
TBL00    DS    0H                                                               
         USING TBLD,R2                                                          
         XC    BFREC,BFREC                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,BFREC,2                             
         B     TBL20                                                            
         SPACE                                                                  
TBL10    GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,BFREC,2                              
         SPACE                                                                  
TBL20    DS    0H                                                               
         TM    DMCB+8,X'80'                                                     
         BO    TBLPRNT                                                          
         SPACE                                                                  
TBLPRC   DS    0H                                                               
         LA    R6,TABN             BCT LIMIT TO R6                              
         LA    R5,TOTSTBL          TBL ADDRS TO R5                              
TP10     CLC   BFICTR,0(R5)        IS INSERT COUNT IN RANGE LIMIT               
         BL    TP00                YES.                                         
         LA    R5,FLDLEN(R5)                                                    
         BCT   R6,TP10             NO.                                          
         SPACE                                                                  
TP00     DS    0H                                                               
         L     R7,15(R5)                                                        
         LA    R7,1(R7)            INCREMENT PUB TOTAL                          
         ST    R7,15(R5)                                                        
         SPACE                                                                  
         ICM   R0,15,BFGROSS       ROUND GROSS TO DOLLARS                       
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                ROUNDED GRS OF PUB REC IN R1                 
         SPACE                                                                  
         L     R7,19(R5)           GRS OF TBL INTO R7                           
         AR    R7,R1               ADD PUB REC GRS TO TBL GRS                   
         ST    R7,19(R5)                                                        
         SPACE                                                                  
         L     R7,ATGRS                                                         
         AR    R7,R1               INCREMENT TOTAL GROSS                        
         ST    R7,ATGRS                                                         
         SPACE                                                                  
         L     R7,23(R5)                                                        
         ICM   R8,15,BFICTR                                                     
         AR    R7,R8                                                            
         STCM  R7,15,23(R5)        INCREMENT INSERT TOTAL                       
         B     TBL10                                                            
         EJECT                                                                  
* THIS SECTION PRINTS TOTALS TABLE *                                            
         SPACE                                                                  
TBLPRNT  DS    0H                                                               
         MVC   P,SPACES                                                         
         MVC   P+132,SPACES                                                     
         LA    R6,TABN             BCT LIMIT TO R6                              
         LA    R5,TOTSTBL+6                                                     
         MVI   RCSUBPRG,2          SET HEADER FOR TOTS TABLE                    
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
TPRNT10  MVC   TBLRNG,0(R5)                                                     
         CLC   =F'0',9(R5)         IS THERE ANY PUB DATA                        
         BL    TP10A                                                            
         MVI   TBLPUB+4,C'0'        NO.                                         
         B     TP10AA                                                           
         SPACE                                                                  
TP10A    EDIT  (4,9(R5)),(5,TBLPUB)                                             
         SPACE                                                                  
         ICM   R1,15,9(R5)         COMPUTE PUB PCT                              
         MVC   BB,ATPUB                                                         
         BAS   RE,PERCNT                                                        
         EDIT  (R1),(3,TBLPBPC)                                                 
         SPACE                                                                  
         ICM   R1,15,9(R5)         COMPUTE PUB CUME                             
         A     R1,PUBCUMW                                                       
         ST    R1,PUBCUMW                                                       
         EDIT  (4,PUBCUMW),(5,TBLPBCM)                                          
         SPACE                                                                  
         BAS   RE,PERCNT           COMPUTE PUB CUME PCT                         
         EDIT  (R1),(3,TBLPBCMP)                                                
         SPACE 2                                                                
TP10AA   CLC   =F'0',13(R5)        IS THERE ANY GROSS DATA                      
         BL    TP10B                                                            
         MVI   TBLGRS+11,C'0'      NO.                                          
         B     TP10C                                                            
         SPACE                                                                  
TP10B    EDIT  (4,13(R5)),(13,TBLGRS),                                 X        
               COMMAS=YES,FLOAT=$,                                     X        
               MINUS=YES                                                        
         SPACE                                                                  
         ICM   R1,15,13(R5)        COMPUTE GROSS PCT                            
         MVC   BB,ATGRS                                                         
         BAS   RE,PERCNT                                                        
         EDIT  (R1),(4,TBLGRSPC),                                      X        
               MINUS=YES                                                        
         SPACE                                                                  
         ICM   R1,15,13(R5)        COMPUTE GRS CUME                             
         A     R1,GRSCUMW                                                       
         ST    R1,GRSCUMW                                                       
         EDIT  (4,GRSCUMW),(13,TBLGRSCM),                              X        
               COMMAS=YES,FLOAT=$,                                     X        
               MINUS=YES                                                        
         SPACE                                                                  
         L     R1,GRSCUMW          COMPUTE GRS CUME PCT                         
         BAS   RE,PERCNT                                                        
         EDIT  (R1),(4,TBLGRCMP),                                      X        
               MINUS=YES                                                        
         SPACE 2                                                                
TP10C    CLC   =F'0',17(R5)        IS THERE ANY INS DATA                        
         BL    TP10CC                                                           
         MVI   TBLINS+4,C'0'                                                    
         B     TP10D               NO                                           
         SPACE                                                                  
TP10CC   EDIT  (4,17(R5)),(5,TBLINS)                                            
         SPACE                                                                  
         ICM   R1,15,17(R5)        COMPUTE INS PCT                              
         MVC   BB,ATINS                                                         
         BAS   RE,PERCNT                                                        
         EDIT  (R1),(3,TBLIPCT)                                                 
         SPACE                                                                  
         ICM   R1,15,17(R5)        COPMUTE INS CUME                             
         A     R1,INSCUMW                                                       
         ST    R1,INSCUMW                                                       
         EDIT  (4,INSCUMW),(5,TBLICUM)                                          
         SPACE                                                                  
         BAS   RE,PERCNT           COMPUTE INS CUME PCT                         
         EDIT  (R1),(3,TBLICPCT)                                                
         SPACE                                                                  
TP10D    GOTO1 REPORT                                                           
         LA    R5,FLDLEN(R5)                                                    
         BCT   R6,TPRNT10                                                       
         SPACE                                                                  
         GOTO1 REPORT                         BLNK LINE BEFORE TOTS             
         MVC   TBLD+3(14),=C'*** TOTALS ***'                                    
         EDIT  (4,ATINS),(5,TBLINS)                                             
         EDIT  (4,ATPUB),(5,TBLPUB)                                             
         EDIT  (4,ATGRS),(13,TBLGRS),                                  X        
               COMMAS=YES,FLOAT=$,                                     X        
               MINUS=YES                                                        
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
PERCNT   DS    0H                                                               
         M     R0,=F'100'                                                       
         SLDA  R0,1                                                             
         D     R0,BB                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                R1 CONTAINS PCT ANSWER                       
         BR    RE                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
TOTSTBL  DS    0C                                                               
         DC    AL4(002),CL11'    001    ',12X'00'                               
         DC    AL4(003),CL11'    002    ',12X'00'                               
         DC    AL4(004),CL11'    003    ',12X'00'                               
         DC    AL4(005),CL11'    004    ',12X'00'                               
         DC    AL4(006),CL11'    005    ',12X'00'                               
         DC    AL4(007),CL11'    006    ',12X'00'                               
         DC    AL4(008),CL11'    007    ',12X'00'                               
         DC    AL4(009),CL11'    008    ',12X'00'                               
         DC    AL4(010),CL11'    009    ',12X'00'                               
         DC    AL4(011),CL11'    010    ',12X'00'                               
         DC    AL4(021),CL11'  011-020  ',12X'00'                               
         DC    AL4(031),CL11'  021-030  ',12X'00'                               
         DC    AL4(041),CL11'  031-040  ',12X'00'                               
         DC    AL4(051),CL11'  041-050  ',12X'00'                               
         DC    AL4(061),CL11'  051-060  ',12X'00'                               
         DC    AL4(071),CL11'  061-070  ',12X'00'                               
         DC    AL4(081),CL11'  071-080  ',12X'00'                               
         DC    AL4(091),CL11'  081-090  ',12X'00'                               
         DC    AL4(101),CL11'  091-100  ',12X'00'                               
         DC    AL4(99999),CL11'  101 +    ',12X'00'                             
TABN     EQU   (*-TOTSTBL)/FLDLEN                                               
*                                                                               
         ORG   TOTSTBL                                                          
TOTSTBLD DS    0C             ***  DSECT OF TOTSTBL  ***                        
         DS    XL4                 INSERT RANGE NUMBER                          
         DS    CL11                INSERT RANGE HEADER                          
         DS    XL4                 NUMBER OF PUBS                               
         DS    XL4                 GROSS DOLLARS                                
         DS    XL4                 NUMBER OF INSERTS                            
         ORG                                                                    
*                                                                               
FLDLEN   EQU   27                                                               
*                                                                               
         BUFF  LINES=1000,ROWS=2,COLUMNS=3,FLAVOR=BINARY,KEYLIST=(6,A)          
         EJECT                                                                  
PLINED   DSECT                 *** DSECT FOR P LINE ***                         
         DS    CL2                                                              
PLCLTC   DS    CL3                 CLIENT CODE                                  
         DS    CL1                                                              
PLCLTN   DS    CL20                CLIENT NAME                                  
         DS    CL3                                                              
PLPUBC   DS    CL15                PUB CODE                                     
         DS    CL1                                                              
PLPUBN   DS    CL20                PUB NAME                                     
         DS    CL4                                                              
PLGROSS  DS    CL14                GROSS                                        
         DS    CL2                                                              
PLINS    DS    CL7                 INSERTS                                      
         DS    CL2                                                              
PLCLTOT  DS    CL6                                                              
         SPACE                                                                  
*                                                                               
TBLD     DSECT          *** DSECT FOR SUMMARY-TABLE LINES ***                   
         DS    CL7                                                              
TBLRNG   DS    CL11                INSERT RANGE                                 
         DS    CL4                                                              
TBLPUB   DS    CL5                 PUBS                                         
         DS    CL2                                                              
TBLPBPC  DS    CL3                 PUB PERCENT                                  
         DS    CL2                                                              
TBLPBCM  DS    CL5                 PUB CUMULATIVE                               
         DS    CL2                                                              
TBLPBCMP DS    CL3                 PUB CUM PERCENT                              
         DS    CL5                                                              
TBLINS   DS    CL5                 INSERTS                                      
         DS    CL2                                                              
TBLIPCT  DS    CL3                 INSERT PERCENT                               
         DS    CL2                                                              
TBLICUM  DS    CL5                 INSERT CUME                                  
         DS    CL2                                                              
TBLICPCT DS    CL3                 INSERT CUME PERCENT                          
         DS    CL4                                                              
TBLGRS   DS    CL13                GROSS                                        
         DS    CL2                                                              
TBLGRSPC DS    CL4                 GROSS PERCENT                                
         DS    CL2                                                              
TBLGRSCM DS    CL13                GROSS CUMULATIVE                             
         DS    CL2                                                              
TBLGRCMP DS    CL4                 GROSS CUM PERCENT                            
         EJECT                                                                  
PP33WRK  DSECT                 *** DSECT FOR PROGRAM WORK AREA ***              
*                                                                               
ABUFFC   DS    A                                                                
INSLIM   DS    F                                                                
TGROSS   DS    F                                                                
TINS     DS    F                                                                
SVPUBK   DS    XL6                                                              
*                                                                               
PCODE    DS    CL6                                                              
CTPUB    DS    F                                                                
CTINS    DS    F                                                                
CTGRS    DS    F                                                                
ATPUB    DS    F                                                                
ATINS    DS    F                                                                
ATGRS    DS    F                                                                
ATCLT    DS    F                                                                
BB       DS    F                                                                
PUBCUMW  DS    F                                                                
GRSCUMW  DS    F                                                                
INSCUMW  DS    F                                                                
         SPACE 2                                                                
BFREC    DS    0XL18           *** DSECT OF BUFF RECORD ***                     
BFKPUB   DS    XL6                 PUB KEY                                      
*                                                                               
BFICTR   DS    XL4                 INSERTION COUNTER                            
BFCCTR   DS    XL4                 CLIENT COUNTER                               
BFGROSS  DS    XL4                 GROSS                                        
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'175PPREP3302 05/01/02'                                      
         END                                                                    
