*          DATA SET DEMTOVSAM  AT LEVEL 021 AS OF 12/03/20                      
*PROCESS USING(WARN(15))                                                        
***********************************************************************         
* >>>>>>> THIS PROGRAM IS BEING RETIRED DURING JAN/FEB 2021. <<<<<<<< *         
*                                                                     *         
* IT HAS BEEN SPLIT INTO TWO INDEPENDANT PROGRAMS:                    *         
* DEVSUPDT (PHASE DEVSUPD) NOW PERFORMS THE MODE=UPDATE FUNCTION, A   *         
*   SPECIALISED FUNCTION LARGELY UNRELATED TO THE OTHER MODES AND     *         
*   WHICH NEVER RUNS AS A SORT EXIT.                                  *         
* DEVSUTIL (PHASE DEVSUT) NOW PERFORMS THE MODE=COPY AND MODE=COMPARE *         
*   UTILITY FUNCTIONS WHICH CAN BOTH RUN AS SORT EXITS WHEN REQUIRED. *         
*                                                                     *         
***********************************************************************         
*PHASE DEM2VSMA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE DELDPRGE                  *** USED TO TEST FOR PURGED DATA             
*INCLUDE DELDDCNT                  *** USED TO COUNT RECORDS BY TYPE            
*INCLUDE NSIWEEK                   *** FOR DELDPRGE                             
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                    *** FOR DELDPRGE                             
*INCLUDE ADDAY                     *** FOR DELDPRGE                             
*INCLUDE BINSRCH2                  *** FOR DELDDCNT                             
*INCLUDE HEXOUT                    *** FOR DELDDCNT                             
*INCLUDE LOADER                                                                 
*INCLUDE REGSAVE                                                                
*ENTRY   E35                       *** PROGRAM ENTRY POINT                      
*                                                                               
         TITLE 'COPY DEMO FILES TO VSAM FORMAT (REFORMATTING KEY)'              
* THREE MODES (VIA MODE=XXX PARAMETER CARD):                                    
*                                                                               
* MODE=COPY COPIES A DEMO FILE. OUTPUT IS ALWAYS VSAM FORMAT, BUT               
*        INPUT CAN BE DANDX OR VSAM FORMAT (SEE INRECFORM=). THIS IS            
*        THE DEFAULT MODE WHEN RUNNING AS AN E35 EXIT TO DFSORT.                
* MODE=LOAD IS A SYNONYM FOR MODE=COPY                                          
* MODE=UPDATE TAKES AN UPDATE FILE THAT WOULD HAVE GONE INTO A DELD/            
*        PVLD UPDATE STEP, COMPARES IT TO THE VSAM FILE, REFORMATS IT           
*        TO THE CORRECT VSAM FORMAT AND OUTPUTS A FILE TO BE MERGED             
*        WITH THE VSAM FILE (IT ALSO CREATES A BACKOUT FILE).                   
* MODE=COMPARE WORKS EXACTLY LIKE MODE=COPY EXCEPT THAT IT SIMPLY               
*        COMPARES THE INPUT FILE WITH THE TARGET VSAM FILE, IGNORING            
*        ALL RECORDS MARKED AS DELETED, AND REPORTS ANY DIFFERENCES.            
*                                                                               
* INRECFORM=VSAM IMPLIES THAT THE INPUT FILE RECORDS ARE ALREADY IN             
*        VSAM FORMAT (I.E., NO FORMAT CONVERSION IS NECESSARY).                 
* INRECFORM=DANDX IMPLIES THAT THE INPUT FILE RECORDS ARE IN DANDX              
*        FORMAT. THE RECORDS WILL BE CONVERTED TO VSAM FORMAT ON                
*        OUTPUT.                                                                
*                                                                               
* PROGRAM CAN RUN AS A DFSORT E35 EXIT, IN WHICH CASE PARAMETER CARDS           
* ARE READ FROM D2VIN RATHER THAN SYSIN. IF D2VIN CAN'T BE OPENED, WE           
* ASSUME COPY MODE. ONLY COPY AND COMPARE MODES VALID IN DFSORT EXIT.           
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
* >>>>>>>>>>>> THIS IS *NOT* THE PROGRAM ENTRY POINT! <<<<<<<<<<<<<<<<          
*                                                                               
* THE ENTRY POINT IS AT LABEL E35, WHETHER OR NOT WE ARE RUNNING AS A           
* DFSORT E35 EXIT!                                                              
*                                                                               
* IMPORTANT NOTE REGARDING IDF DEBUGGING:                                       
*                                                                               
*  WHEN THIS PROGRAM RUNS AS A DFSORT E35 EXIT, AN IDF "DBREAK" COMMAND         
*  MUST BE USED TO GET IDF CONTROL. DBREAK ONLY ACCEPTS A LOAD MODULE           
*  NAME, AND (OPTIONALLY) A CSECT NAME (WITHIN THE NAMED LOAD MODULE).          
*  IN THIS PROGRAM, HOWEVER, THE E35 ENTRY POINT IS NOT A CSECT. I.E.,          
*  WE CAN'T DIRECTLY TELL IDF TO SET A DBREAK AT LABEL E35.                     
*                                                                               
*  IT'S ALMOST AS GOOD, HOWEVER, TO SET A DBREAK AT LABEL "INIT",               
*  BECAUSE WE UNCONDITIONALLY JUMP TO "INIT" ALMOST IMMEDIATELY AFTER           
*  REACHING THE E35 ENTRY POINT. TO SET THAT DBREAK, WE JUST NEED TO            
*  KNOW THE OFFSET TO LABEL "INIT" FROM THE START OF THE LOAD MODULE            
*  (WHICH WE KNOW FROM THE ASSEMBLER LISTING).                                  
*                                                                               
*  SO TO DEBUG AN "A" VERSION OF DEM2VSM: IF "INIT" IS LOCATED AT               
*  OFFSET X'7C' FROM THE START OF THE LOAD MODULE, ISSUE THE FOLLOWING          
*  IDF COMMANDS BEFORE ISSUING A "RUN" COMMAND:                                 
*                                                                               
*  ==> DBREAK (DEM2VSMA.)+X'7C'                                                 
*  ==> LAN LOAD DEM2VSAM                                                        
*                                                                               
***********************************************************************         
*                                                                               
DEM2VSAM CSECT                                                                  
*                                                                               
         ENTRY E35                 PROGRAM ENTRY POINT                          
         ENTRY DATEVAL             "TODAY'S" DATE: FORMAT MM/DD/YY              
         ENTRY DEMTABS             V(DEMTABS)                                   
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,**D2VS**,RA,WORK=V(REGSAVE)                                    
         LARL  R7,COMMON                                                        
         USING COMMON,R7                                                        
         MVI   E35EXIT,NO                                                       
*                                                                               
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     DS    0H                                                               
         L     R9,VCPRINT                                                       
         USING DPRINT,R9           R9=A(PRINT CSECT)                            
         LARL  R3,OUTAREA                                                       
         USING OTREC,R3            R3=A(OUTPUT RECORD)                          
VSREC    USING DVREC,OTVSREC       VSAM RECORD IN OUTPUT RECORD                 
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'99'                                                      
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(17),=C'DEMO TO VSAM. E35' ASSUME E35                       
*                                                                               
         GOTO1 =V(LOADER),DMCB,=C'T00AD1  ',0                                   
         ICM   RE,15,4(R1)         GET A(DEMTABS)                               
         JZ    *+2                 CAN'T GET ADDRESS ?!?                        
         ST    RE,DEMTABS          PUBLISH VIA ENTRY POINT                      
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(10,DATEVAL)  TODAY: MM/DD/YY              
*                                                                               
         CLI   E35EXIT,YES         TEST RUNNING AS A DFSORT E35 EXIT            
         JNE   INIT10                                                           
         L     RE,=V(SYSIN)        PICK UP SYSIN DCB IN CARDS                   
         MVC   DCBDDNAM-IHADCB(,RE),=CL8'D2VIN' CHANGE DDNAME                   
         BRAS  RE,VALPARMS         VALIDATE PARAMETER CARD(S)                   
         CLI   MODE,COMPARE        COMPARE MODE?                                
         JE    INIT20              YES, NEEDS OLD/NEW FILE                      
         J     INIT30              ELSE SEE IF VSAM FILE NEEDED                 
*                                                                               
INIT10   DS    0H                                                               
         STCM  R3,7,DSNXTRCT+1     EXTRACT INPUT DSN INTO TITLE                 
         LA    R1,DSNXTRCT                                                      
         LARL  RF,IFILE                                                         
         STCM  R1,7,DCBEXLSA-IHADCB(RF) SET EXLST ADDRESS IN DCB                
         RDJFCB ((RF))                                                          
         MVC   TITLE+14(4),=C'DSN='                                             
         MVC   TITLE+18(44),0(R3)                                               
*                                                                               
         OPEN  (IFILE,INPUT)                                                    
*                                                                               
         BRAS  RE,VALPARMS         VALIDATE PARAMETER CARD(S)                   
*                                                                               
         CLI   MODE,COMPARE        COMPARE MODE?                                
         JE    INIT20              YES, NEEDS OLD/NEW FILE                      
         OPEN  (OFILE,OUTPUT)      ELSE NEEDS OUTPUT FILE(S)                    
         CLI   SPLIT,YES           SPLIT FILE?                                  
         JNE   INIT30              NO                                           
         OPEN  (OFIL2,OUTPUT)                                                   
         J     INIT30                                                           
*                                                                               
INIT20   DS    0H                                                               
         OPEN  (NEWFILE,OUTPUT,OLDFILE,OUTPUT,DIFFILE,OUTPUT)                   
*                                                                               
INIT30   DS    0H                                                               
         CLI   MODE,COPY           UPDATE OR COMPARE MODE?                      
         JE    INITX               SKIP IF COPY MODE                            
*                                                                               
         OPEN  (DEMACB)            FOR UPDATE OR COMPARE MODE OPEN VSAM         
         LTR   RF,RF               RF HOLDS ERROR RETURN CODE                   
         JNZ   *+2                                                              
         CLI   SPLIT,YES           SPLIT FILE?                                  
         JNE   INIT40              NO                                           
         OPEN  (DEMAC2)                                                         
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
INIT40   DS    0H                                                               
         CLI   MODE,UPDATE         UPDATE MODE?                                 
         JNE   INITX                                                            
         OPEN  (BKFILE,OUTPUT)     OPEN BACKOUT FILES                           
         CLI   SPLIT,YES                                                        
         JNE   INITX                                                            
         OPEN  (BKFIL2,OUTPUT)                                                  
*                                                                               
INITX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* MAIN LOOP                                                           *         
***********************************************************************         
         USING INREC,R2                                                         
MAIN     DS    0H                                                               
         CLI   E35EXIT,YES         TEST RUNNING AS A DFSORT E35 EXIT            
         JNE   MAIN02                                                           
         LTR   R2,R2               YES, R2 IS ALREADY A(RECORD)                 
         JNZ   MAIN10                                                           
         J     EOFIN               OR R2 IS ZERO IF NO MORE RECORDS             
*                                                                               
MAIN02   DS    0H                                                               
         LARL  R1,IFILE                                                         
         GET   (1)                 GET NEXT RECORD                              
         LR    R2,R1                                                            
*                                                                               
MAIN10   DS    0H                                                               
         AP    CTRIN,=P'1'         COUNT RECORDS IN                             
         AP    CTRINFMS,=P'1'      COUNT RECORDS IN (FOR THIS F/M/S)            
*                                                                               
         CLI   INPARM,VSAM         INRECFORM=VSAM?                              
         JE    MAINV10             YES, SKIP TO VSAM CODE                       
*                                                                               
*        INPUT IS DANDX. HANDLE CONVERSION TO VSAM FORMAT                       
*                                                                               
         CLC   INLENMVS,=Y(INPLENQ) IS THIS A DIRECTORY RECORD?                 
         JE    MAIND50             YES, WHOLE DIFFERENT REFORMAT                
*                                                                               
*        'NORMAL' DEMO RECORD (I.E. NOT PASSIVE)                                
*                                                                               
         OC    INKEY,INKEY         TEST FOR NULL KEY                            
         JZ    *+2                 ABSOLUTELY SHOULD NOT HAPPEN                 
*                                                                               
         AP    CTRFIL,=P'1'                                                     
         LLC   R0,INSTAT           STATUS BYTE                                  
         NILF  GR0,DANDXFL#        ISOLATE LOGICAL FILE NUMBER                  
         CHI   R0,TESTFIL#         IS THIS A TEST FILE RECORD?                  
         JNE   MAIND22             NO                                           
         AP    CTRFILTD,=P'1'      COUNT FILE TEST DATA                         
         CLI   TESTDATA,YES        ARE WE EXPECTING TEST DATA?                  
         JNE   MAINETST            NO, ABORT THIS RUN                           
*                                                                               
MAIND22  DS    0H                                                               
         XR    R0,R0                                                            
         ICM   R0,B'0011',INLENMVS ACCUMULATE INPUT MVS BYTES                   
         LG    GRF,IFBYTES                                                      
         AGFR  GRF,R0                                                           
         STG   GRF,IFBYTES                                                      
         LG    GRF,FMSBYTES        BYTE COUNT PER FILE/MEDIA/SOURCE             
         AGFR  GRF,R0                                                           
         STG   GRF,FMSBYTES                                                     
*                                                                               
         TM    INSTAT,DVSDEL       IS IT DELETED?                               
         JZ    MAIND23                                                          
         AP    CTRFILD,=P'1'       YES: COUNT DELETED FILE RECORDS              
         J     MAINDDEL                                                         
*                                                                               
MAIND23  DS    0H                                                               
         CLI   MODE,UPDATE         UPDATE MODE?                                 
         JE    MAIND24             YES, DON'T DO PURGE CHECK                    
         LA    R1,INKEYMAJ                                                      
         BRAS  R8,MAINPRGE         SHOULD DIRECTORY RECORD BE PURGED?           
         JNE   MAIND24             NO, KEEP IT                                  
         AP    CTRFILPL,=P'1'      COUNT FILE RECORDS IGNORED AS PURGED         
         J     MAINDDEL            IGNORE RECORD                                
*                                                                               
MAIND24  DS    0H                                                               
         TM    INSTAT,DVSEXTP      TEST EXTENDED FLAG (SHOULD BE ZERO)          
         JZ    *+10                                                             
         AP    CTRFILX,=P'1'       COUNT (INCORRECT) EXTENDED FLAG              
*                                                                               
         AP    CTRMINAV,=P'1'      COUNT TOTAL MINOR KEYS FOR AVG CALC          
         CLC   THISKMAJ,INKEYMAJ   SAME MAJOR KEY AS THE LAST RECORD            
         JNE   MAIND26             NO, SKIP                                     
         AP    IMINORS,=P'1'       COUNT MINORS THIS MAJOR                      
         CLC   THISKMIN,INKEYMIN   SAME MINOR KEY AS LAST RECORD?               
         JNE   MAIND28             NO,                                          
         J     MAIND30             YES, SKIP IT'S A DUPLICATE                   
*                                                                               
MAIND26  DS    0H                                                               
         AP    CTRMAJKY,=P'1'      COUNT MAJOR KEYS                             
*                                                                               
         IF (CLC,THISKFMS,NE,INKEYFMS),AND, WHEN F/M/S CHANGES                  
            (OC,THISKFMS,THISKFMS,NZ)        AND NOT FIRST TIME:                
           BAS RE,PRNTCURT                   PRINT TOTS. FOR THIS F/M/S         
         ENDIF ,                                                                
*                                                                               
         CP    CTRMINMX,IMINORS    WAS LAST MAXIMUM MINORS SO FAR               
         JNL   *+10                                                             
         MVC   CTRMINMX,IMINORS    YES, SAVE IT                                 
         ZAP   IMINORS,=P'1'                                                    
*                                                                               
MAIND28  DS    0H                                                               
         CP    CTRDUPMX,IDUPKEY    WAS LAST MAXIMUM DUPS SO FAR                 
         JNL   *+10                                                             
         MVC   CTRDUPMX,IDUPKEY    YES, SAVE IT                                 
         ZAP   IDUPKEY,=P'0'                                                    
         MVC   THISKEY,INKEY       SAVE NEW KEY                                 
         MVI   SEQNO,0             RESET SEQUENCE NUMBER                        
         J     MAIND40                                                          
*                                                                               
MAIND30  DS    0H                                                               
         AP    IDUPKEY,=P'1'       COUNT DUPLICATES THIS KEY                    
         AP    CTRDUPAV,=P'1'      COUNT FOR AVERAGE CALC LATER                 
         LLC   R1,SEQNO            GET SEQUENCE NUMBER                          
         LTR   R1,R1               IS THIS FIRST FOR THIS KEY                   
         JNZ   *+10                                                             
         AP    CTRDUPKY,=P'1'      YES, COUNT NUMBER OF KEYS DUPLICATED         
         LA    R1,1(,R1)                                                        
         STC   R1,SEQNO            INCREMENT SEQUENCE NUMBER                    
         CLI   SEQNO,250                                                        
         JH    *+2                 DIE IF SEQNO > 250                           
*                                                                               
MAIND40  DS    0H                                                               
         XR    RE,RE                                                            
         ICM   RE,B'0011',INLENMVS MVS LENGTH                                   
         LR    R1,RE               SAVE FOR MVCL                                
         LA    RE,1(,RE)           INCREASE MVS LENGTH BY 1                     
         SLL   RE,16                                                            
         ST    RE,OTRDW            SET O/P MVS LEN                              
         MVC   VSREC.DVKEY(L'INKEY),INKEY COPY MAJOR/MINOR KEY                  
         MVC   VSREC.DVKEYSEQ,SEQNO       INSERT SEQUENCE NUMBER                
         ICM   RE,B'0011',INLENDDS DDS LENGTH (HI 2 BYTES IRRELEVANT)           
         LA    RE,1(,RE)           INCREASE DDS LENGTH BY 1                     
         STCM  RE,B'0011',VSREC.DVLENDDS SET O/P DDS LEN                        
         LA    R0,VSREC.DVKEY+L'DVKEY+L'DVLENDDS 1ST BYTE FOR REMAINDER         
         AHI   R1,-(L'INRDW+L'INKEY+L'INLENDDS) LESS LEN DONE SO FAR            
         LA    RE,INKEY+L'INKEY+L'INLENDDS FIRST BYTE TO MOVE REST FROM         
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE REST OF RECORD                          
         JO    *+2                 DESTRUCTIVE MOVE!                            
         NI    VSREC.DVSTAT,X'FF'-(DVSEXTP+DANDXFL#) 0 UNWANTED STATUS          
         J     MAINOUT             KEEP RECORD                                  
*                                                                               
*        PASSIVE DEMO RECORD, AND DELETED POINTERS (UPDATE)                     
*                                                                               
MAIND50  DS    0H                                                               
         OC    INKEYMAJ,INKEYMAJ   TEST FOR NULL KEY                            
         JZ    *+2                 ABSOLUTELY SHOULD NOT HAPPEN                 
*                                                                               
         AP    CTRDIR,=P'1'        COUNT DIRECTORY RECORDS                      
         LLC   R0,INPSTAT          STATUS BYTE                                  
         NILF  GR0,DANDXFL#        ISOLATE LOGICAL FILE NUMBER                  
         CHI   R0,TESTFIL#         IS THIS A TEST FILE RECORD?                  
         JNE   MAIND52             NO                                           
         AP    CTRDIRTD,=P'1'      COUNT TEST DATA POINTERS                     
         CLI   TESTDATA,YES        ARE WE EXPECTING TEST DATA?                  
         JNE   MAINETST            NO, ABORT THIS RUN                           
*                                                                               
MAIND52  DS    0H                                                               
         LA    R0,INPLENQ          ACCUMULATE INPUT DDS BYTES                   
         LG    GRF,IDBYTES                                                      
         AGFR  GRF,R0                                                           
         STG   GRF,IDBYTES                                                      
         LG    GRF,FMSBYTES        BYTE COUNT PER FILE/MEDIA/SOURCE             
         AGFR  GRF,R0                                                           
         STG   GRF,FMSBYTES                                                     
*                                                                               
         TM    INPSTAT,DVSEXTP     TEST EXTENDED                                
         JO    MAIND54                                                          
         AP    CTRDNX,=P'1'        NO, COUNT NOT EXTENDED                       
*                                                                               
         TM    INPSTAT,DVSDEL      IS IT DELETED?                               
         JZ    MAIND58             NO, PROCESS                                  
         AP    CTRDNXD,=P'1'       COUNT NOT EXTENDED AND DELETED               
         J     MAIND56             PROCESS DELETED                              
*                                                                               
MAIND54  DS    0H                                                               
         AP    CTRPASX,=P'1'       COUNT EXTENDED PASSIVES                      
         TM    INPSTAT,DVSDEL                                                   
         JZ    MAIND58             PROCESS IF NOT DELETED                       
         AP    CTRPASXD,=P'1'      COUNT DELETED EXTENDED PASSIVES              
*                                                                               
MAIND56  DS    0H                                                               
         CLI   MODE,UPDATE         UPDATE MODE?                                 
         JNE   MAINDDEL            NO: IGNORE DELETED PASSIVES                  
*                                                                               
MAIND58  DS    0H                                                               
         XC    THISKMIN,THISKMIN   CLEAR MINOR PART                             
         CLC   THISKMAJ,INKEYMAJ   SAME KEY AS THE LAST RECORD                  
         JE    *+2                 REALLY SHOULD NOT HAPPEN                     
*                                                                               
         IF (CLC,THISKFMS,NE,INKEYFMS),AND, WHEN F/M/S CHANGES                  
            (OC,THISKFMS,THISKFMS,NZ)        AND NOT FIRST TIME:                
           BAS RE,PRNTCURT                   PRINT TOTS. FOR THIS F/M/S         
         ENDIF ,                                                                
*                                                                               
         MVC   THISKMAJ,INKEYMAJ                                                
*                                                                               
         CLI   MODE,UPDATE         UPDATE MODE?                                 
         JE    MAIND60             YES, DON'T DO PURGE CHECK                    
         LA    R1,INKEYMAJ                                                      
         BRAS  R8,MAINPRGE         SHOULD DIRECTORY RECORD BE PURGED?           
         JNE   MAIND60             NO, KEEP IT                                  
         AP    CTRDIRPL,=P'1'      COUNT DIR RECORDS IGNORED AS PURGED          
         J     MAINDDEL            IGNORE RECORD                                
*                                                                               
MAIND60  DS    0H                                                               
         MVC   VSREC.DVKEYMAJ,INKEYMAJ        COPY MAJOR KEY                    
         XC    VSREC.DVKEYMIN,VSREC.DVKEYMIN  CLEAR MINOR KEY                   
         MVI   VSREC.DVKEYSEQ,PASSIVE# INDICATE PASSIVE IN KEY SEQ FLD.         
         MVC   VSREC.DVSTAT,INPSTAT      COPY STATUS                            
         NI    VSREC.DVSTAT,X'FF'-DANDXFL# ZERO UNWANTED STATUS                 
         MVI   VSREC.DVPEOR,0            NON-EXTENDED END OF RECORD             
         LA    R1,DVPRLNQ                NON-EXTENDED RECORD LENGTH             
         TM    VSREC.DVSTAT,DVSEXTP      EXTENDED PASSIVE?                      
         JZ    MAIND64                   NO, SKIP                               
         MVC   VSREC.DVPEL(2),=AL1(X'01',DVPLENQ) DUMMY ELCODE/LENGTH           
         MVC   VSREC.DVPDATA,INPDATA     COPY DATA TO ELEMENT                   
         MVI   VSREC.DVPEEOR,0           EXTENDED END OF RECORD                 
         LA    R1,DVPERLNQ         EXTENDED RECORD LENGTH                       
*                                                                               
MAIND64  DS    0H                                                               
         STCM  R1,B'0011',VSREC.DVLENDDS SET DDS LENGTH                         
         LA    R1,4(,R1)                                                        
         SLL   R1,16                                                            
         ST    R1,OTRDW            SET MVS LENGTH                               
         J     MAINOUT                                                          
*                                                                               
MAINDDEL DS    0H                                                               
         IF (TM,COUNTFLG,COUNTYES,O),AND, IF COUNT= CARD IS PRESENT...          
            (CLC,CNTLSTMJ,NE,INKEYMAJ),AND, ..AND MAJOR KEY CHANGED...          
            (CLI,MODE,NE,UPDATE)          ...AND MODE ISN'T UPDATE:             
*                                      DELDDCNT REQUIRES DANDX-STYLE            
           MVC   CNTARKEY,INKEYMAJ     MAJOR KEY                                
           MVC   CNTLSTMJ,CNTARKEY     REMEMBER LAST MAJOR KEY COUNTED          
           OI    CNTARSTA,DVSDEL       MARK DELETED SO LDCOUNT KNOWS            
           MVI   PLCOUNT,1             TELL LDCOUNT TO "COUNT"                  
           GOTO1 VLDCOUNT,PLCOUNT      INCREMENT APPROPRIATE COUNTER            
         ENDIF ,                                                                
         MVC   E35RC,=H'4'         SET RC=4 - DELETE (IF DFSORT EXIT)           
         J     MAINNEXT            IGNORE RECORD                                
*                                                                               
         EJECT                                                                  
*                                                                               
*        INPUT IS VSAM FORMAT. NO CONVERSION NEEDED                             
*                                                                               
MAINV10  DS    0H                                                               
         LR    R0,R3               OUTPUT AREA                                  
         LR    RE,R2               INPUT RECORD                                 
         LH    R1,INLENMVS         LENGTH                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY INPUT TO OUTPUT AREA                    
         JO    *+2                 DESTRUCTIVE MOVE!                            
*                                                                               
         CLI   VSREC.DVKEYSEQ,PASSIVE#   PASSIVE?                               
         JE    MAINV50             YES, SKIP                                    
*                                                                               
*        'NORMAL' VSAM RECORD (I.E. NOT PASSIVE)                                
*                                                                               
         AP    CTRFIL,=P'1'                                                     
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,B'0011',OTLENMVS ACCUMULATE INPUT MVS BYTES                   
         LG    GRF,IFBYTES                                                      
         AGFR  GRF,R0                                                           
         STG   GRF,IFBYTES                                                      
         LG    GRF,FMSBYTES        BYTE COUNT PER FILE/MEDIA/SOURCE             
         AGFR  GRF,R0                                                           
         STG   GRF,FMSBYTES                                                     
*                                                                               
         TM    VSREC.DVSTAT,DVSDEL IS IT DELETED?                               
         JZ    MAINV22                                                          
         AP    CTRFILD,=P'1'       YES: COUNT DELETED FILE RECORDS              
         J     MAINVDEL                                                         
*                                                                               
MAINV22  DS    0H                                                               
         CLI   MODE,UPDATE         UPDATE MODE?                                 
         JE    MAINV24             YES, DON'T DO PURGE CHECK                    
         LA    R1,VSREC.DVKEYMAJ                                                
         BRAS  R8,MAINPRGE         SHOULD RECORD BE PURGED?                     
         JNE   MAINV24             NO, KEEP IT                                  
         AP    CTRFILPL,=P'1'      COUNT DIR RECORDS IGNORED AS PURGED          
         J     MAINVDEL            IGNORE RECORD                                
*                                                                               
MAINV24  DS    0H                                                               
         AP    CTRMINAV,=P'1'      COUNT TOTAL MINOR KEYS FOR AVG CALC          
         CLC   THISKMAJ,VSREC.DVKEYMAJ   SAME MAJOR KEY AS LAST RECORD?         
         JNE   MAINV26             NO, SKIP                                     
         AP    IMINORS,=P'1'       COUNT MINORS THIS MAJOR                      
         CLC   THISKMIN,VSREC.DVKEYMIN   SAME MINOR KEY AS LAST RECORD?         
         JNE   MAINV28             NO,                                          
         J     MAINV30             YES, SKIP                                    
*                                                                               
MAINV26  DS    0H                                                               
         AP    CTRMAJKY,=P'1'      COUNT MAJOR KEYS                             
*                                                                               
         IF (CLC,THISKFMS,NE,VSREC.DVKEYFMS),AND, WHEN F/M/S CHANGES            
            (OC,THISKFMS,THISKFMS,NZ)        AND NOT FIRST TIME:                
           BAS RE,PRNTCURT                   PRINT TOTS. FOR THIS F/M/S         
         ENDIF ,                                                                
*                                                                               
         CP    CTRMINMX,IMINORS    WAS LAST MAXIMUM MINORS SO FAR               
         JNL   *+10                                                             
         MVC   CTRMINMX,IMINORS    YES, SAVE IT                                 
         ZAP   IMINORS,=P'1'                                                    
*                                                                               
MAINV28  DS    0H                                                               
         CP    CTRDUPMX,IDUPKEY    WAS LAST MAXIMUM DUPS SO FAR                 
         JNL   *+10                                                             
         MVC   CTRDUPMX,IDUPKEY    YES, SAVE IT                                 
         ZAP   IDUPKEY,=P'0'                                                    
         MVC   THISKEY,VSREC.DVKEY SAVE NEW KEY                                 
         MVI   SEQNO,0             RESET SEQUENCE NUMBER                        
         J     MAINV40                                                          
*                                                                               
MAINV30  DS    0H                                                               
         AP    IDUPKEY,=P'1'       COUNT DUPLICATES THIS KEY                    
         AP    CTRDUPAV,=P'1'      COUNT FOR AVERAGE CALC LATER                 
         LLC   R1,SEQNO            GET SEQUENCE NUMBER                          
         LTR   R1,R1               IS THIS FIRST FOR THIS KEY                   
         JNZ   *+10                                                             
         AP    CTRDUPKY,=P'1'      YES, COUNT NUMBER OF KEYS DUPLICATED         
         LA    R1,1(,R1)                                                        
         STC   R1,SEQNO            INCREMENT SEQUENCE NUMBER                    
         CLI   SEQNO,250                                                        
         JH    *+2                 DIE IF SEQNO > 250                           
*                                                                               
MAINV40  DS    0H                                                               
         MVC   VSREC.DVKEYSEQ,SEQNO RESET SEQUENCE NUMBER                       
         J     MAINOUT                                                          
*                                                                               
*        PASSIVE VSAM RECORD                                                    
*                                                                               
MAINV50  DS    0H                                                               
         AP    CTRDIR,=P'1'        COUNT DIRECTORY RECORDS                      
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,B'0011',OTLENMVS ACCUMULATE INPUT MVS BYTES                   
         LG    GRF,IDBYTES                                                      
         AGFR  GRF,R0                                                           
         STG   GRF,IDBYTES                                                      
         LG    GRF,FMSBYTES        BYTE COUNT PER FILE/MEDIA/SOURCE             
         AGFR  GRF,R0                                                           
         STG   GRF,FMSBYTES                                                     
*                                                                               
         TM    VSREC.DVSTAT,DVSEXTP EXTENDED?                                   
         JO    MAINV54                                                          
         AP    CTRDNX,=P'1'        NO, COUNT NOT EXTENDED                       
*                                                                               
         TM    VSREC.DVSTAT,DVSDEL IS IT DELETED?                               
         JZ    MAINV58             NO, PROCESS                                  
         AP    CTRDNXD,=P'1'       COUNT NOT EXTENDED AND DELETED               
         J     MAINV56             PROCESS DELETED                              
*                                                                               
MAINV54  DS    0H                                                               
         AP    CTRPASX,=P'1'       COUNT EXTENDED PASSIVES                      
         TM    VSREC.DVSTAT,DVSDEL                                              
         JZ    MAINV58             PROCESS IF NOT DELETED                       
         AP    CTRPASXD,=P'1'      COUNT DELETED EXTENDED PASSIVES              
*                                                                               
MAINV56  DS    0H                                                               
         CLI   MODE,UPDATE         UPDATE MODE?                                 
         JNE   MAINVDEL            NO: IGNORE DELETED PASSIVES                  
*                                                                               
MAINV58  DS    0H                                                               
         IF (CLC,THISKFMS,NE,VSREC.DVKEYFMS),AND, WHEN F/M/S CHANGES            
            (OC,THISKFMS,THISKFMS,NZ)        AND NOT FIRST TIME:                
           BAS RE,PRNTCURT                   PRINT TOTS. FOR THIS F/M/S         
         ENDIF ,                                                                
*                                                                               
         MVC   THISKEY,VSREC.DVKEY SAVE NEW KEY                                 
*                                                                               
         CLI   MODE,UPDATE         UPDATE MODE?                                 
         JE    MAINOUT             YES, DON'T DO PURGE CHECK                    
         LA    R1,VSREC.DVKEYMAJ                                                
         BRAS  R8,MAINPRGE         SEE IF REC SHOULD HAVE BEEN PURGED           
         JNE   MAINOUT             NO, KEEP IT                                  
         AP    CTRDIRPL,=P'1'      COUNT DIR RECORDS IGNORED AS PURGED          
*                                                                               
MAINVDEL DS    0H                                                               
         IF (TM,COUNTFLG,COUNTYES,O),AND, IF COUNT= CARD IS PRESENT...          
            (CLC,CNTLSTMJ,NE,VSREC.DVKEYMAJ),AND, ...& NEW MAJOR KEY...         
            (CLI,MODE,NE,UPDATE)          ...AND MODE ISN'T UPDATE:             
*                                      DELDDCNT REQUIRES DANDX-STYLE            
           MVC   CNTARKEY,VSREC.DVKEYMAJ MAJOR KEY                              
           MVC   CNTLSTMJ,CNTARKEY     REMEMBER LAST MAJOR KEY COUNTED          
           OI    CNTARSTA,DVSDEL       MARK DELETED SO LDCOUNT KNOWS            
           MVI   PLCOUNT,1             TELL LDCOUNT TO "COUNT"                  
           GOTO1 VLDCOUNT,PLCOUNT      INCREMENT APPROPRIATE COUNTER            
         ENDIF ,                                                                
         MVC   E35RC,=H'4'         SET RC=4 - DELETE (IF DFSORT EXIT)           
         J     MAINNEXT            IGNORE RECORD                                
*                                                                               
         EJECT                                                                  
*                                                                               
* OUTPUT VSAM FORMAT RECORD                                                     
*                                                                               
MAINOUT  DS    0H                                                               
         BRAS  RE,RECCHECK         CHECK FORMAT OF OUTPUT RECORD                
*                                                                               
         IF (TM,COUNTFLG,COUNTYES,O),AND, IF COUNT= CARD IS PRESENT...          
            (CLC,CNTLSTMJ,NE,4(R3)),AND,  ...AND CHANGED MAJOR KEY...           
            (CLI,MODE,NE,UPDATE)          ...AND MODE ISN'T UPDATE:             
*                                      DELDDCNT REQUIRES DANDX-STYLE            
           MVC   CNTARKEY,4(R3)        COPY MAJOR KEY                           
           MVC   CNTLSTMJ,CNTARKEY     REMEMBER LAST MAJOR KEY COUNTED          
           MVI   CNTARSTA,0            ZERO STATUS                              
           MVI   PLCOUNT,1             TELL LDCOUNT TO "COUNT"                  
           GOTO1 VLDCOUNT,PLCOUNT      INCREMENT APPROPRIATE COUNTER            
         ENDIF ,                                                                
*                                                                               
         CLI   MODE,COPY           IF COPY MODE,                                
         JE    MAINO10             GO OUTPUT THE RECORD                         
*                                                                               
         CLI   MODE,UPDATE         IF UPDATE MODE,                              
         JNE   MAINO20                                                          
         BRAS  RE,MERGEMAJ         DEAL WITH EXISTING RECS FOR MAJOR            
*                                                                               
         OC    OTRDW,OTRDW         MERGEMAJ ZEROS THIS IF RECORD IS             
         JNZ   MAINO10             ALREADY ON FILE (MM30) SO DROP IT            
         MVC   E35RC,=H'4'         SET RC=4 - DELETE (IF DFSORT EXIT)           
         J     MAINNEXT                                                         
*                                                                               
MAINO10  DS    0H                                                               
         LR    R0,R3               WRITE RECORD                                 
         BRAS  RE,PUTOUT                                                        
         J     MAINNEXT                                                         
*                                                                               
MAINO20  DS    0H                                                               
         CLI   MODE,COMPARE        IF COMPARE MODE,                             
         JNE   *+2                 (CAN'T BE ANYTHING ELSE)                     
         BRAS  RE,COMPARE_FILES    JUST COMPARE WITH VSAM FILE                  
*                                                                               
MAINNEXT DS    0H                                                               
         CLI   E35EXIT,YES         TEST RUNNING AS A DFSORT E35 EXIT            
         JNE   MAIN                                                             
         J     E35GOBAK            YES, RETURN TO DFSORT FOR NEXT REC           
*                                                                               
         EJECT                                                                  
*                                                                               
MAINETST DS    0H                                                               
         LARL  R1,LTGTDF1                                                       
         MVC   P(L'LTGTDF1),0(R1)                                               
         GOTOR VPRINTER                                                         
         LARL  R1,LTGTDF2                                                       
         MVC   P+10(L'LTGTDF2),0(R1)                                            
         GOTOR VPRINTER                                                         
         GOTOR VPRINTER                                                         
         ABEND 101                                                              
*                                                                               
*        TEST IF INPUT RECORD WOULD HAVE BEEN PURGED BY DELDPRGE AND            
*        SET CC EQUAL IF IT WOULD.                                              
*        NOTE DELDPRGE REQUIRES A DANDX DIRECTORY RECORD                        
*                                                                               
MAINPRGE DS    0H                                                               
         CLI   DTF#,1              WAS FILE SPECIFIED                           
         JL    MAINPRGX            NO, CC NEQ TO KEEP RECORD                    
*                                                                               
         IF (CLC,PRGLSTMJ,NE,0(R1))  IF MAJOR KEY HAS CHANGED:                  
           MVC   PRGARKEY,0(R1)    COPY MAJOR KEY                               
           MVC   PRGLSTMJ,PRGARKEY REMEMBER LAST MAJOR KEY WE EXAMINED          
           MVI   PRGARSTA,0        ZERO STATUS (WE HANDLE DELETED BIT)          
           XC    PRGARDA,PRGARDA   CLEAR D/A                                    
           MVI   PURGEFLG,0        CLEAR PURGE FLAG                             
           GOTOR VLDPURGE,PLPURGE  CALL DELDPRGE                                
         ENDIF ,                                                                
         CLI   PURGEFLG,X'FF'      RECORD TO BE PURGED?                         
*                                  CC EQ IF SO, ELSE CC NE                      
*                                                                               
MAINPRGX DS    0H                                                               
         BR    R8                  RETURN TO CALLER VIA R8                      
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* MISC. ROUTINES                                                      *         
***********************************************************************         
XIT1     DS    0H                                                               
         XIT1  ,                                                                
*                                                                               
SETRC    DS    0H                                                               
         CH    R1,RETCODE          SET RETURN CODE IF HIGHER                    
         JNH   *+8                                                              
         STH   R1,RETCODE                                                       
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* END OF INPUT FILE                                                   *         
***********************************************************************         
EOFIN    DS    0H                                                               
         CP    CTRMINMX,IMINORS    WAS LAST MAXIMUM MINORS SO FAR               
         JNL   *+10                                                             
         MVC   CTRMINMX,IMINORS    YES, SAVE IT                                 
         CP    CTRDUPMX,IDUPKEY    WAS LAST MAXIMUM DUPS SO FAR                 
         JNL   *+10                                                             
         MVC   CTRDUPMX,IDUPKEY    YES, SAVE IT                                 
                                                                                
         IF (CLC,THISKFMS,NE,=X'FFFFFF')  IF NOT EOF DIR. RECORD:               
           MVC P(L'THISKFMS),THISKFMS     PRINT LAST F/M/S KEY                  
         ELSE ,                                                                 
           MVC P(L'THISKFMS),=C'*FF'      DENOTE SPECIAL EOF RECORD             
         ENDIF ,                                                                
         EDIT  CTRINFMS,(15,P+15),COMMAS=YES,ZERO=NOBLANK                       
         LG    GR1,FMSBYTES               # BYTES                               
         IF (CLI,MODE,NE,UPDATE)          PRINT IN KB UNLESS UPDATE             
           AGHI  GR1,512                  +512                                  
           SRAG  GR1,GR1,10               /1024                                 
           MVC   P+50(2),=C'KB'                                                 
         ENDIF ,                                                                
         CVDG  GR1,LONG                   VALUE IS IN DUB1+DUB2                 
         EDIT  (P8,DUB2),(15,P+35),COMMAS=YES,ZERO=NOBLANK                      
         GOTO1 VPRINTER                                                         
         MVC   P(30),=C'-----          ---------------'                         
         GOTO1 VPRINTER                                                         
         AP    CTRFMSTO,CTRINFMS          KEEP A RUNNING F/M/S TOTAL            
         MVC   P(5),=C'TOTAL'                                                   
         EDIT  CTRFMSTO,(15,P+15),COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 VPRINTER                                                         
                                                                                
         SELECT CLI,MODE,EQ                                                     
                                                                                
           WHEN (UPDATE)           MODE=UPDATE:                                 
             XC    VSREC.DVKEY,VSREC.DVKEY                                      
             BRAS  RE,MERGEMAJ         COMPLETE EXISTING RECS FOR MAJOR         
             CLOSE (IFILE,,DEMACB,,OFILE,,BKFILE)                               
             IF (CLI,SPLIT,EQ,YES)     SPLIT FILE?                              
               CLOSE (DEMAC2,,OFIL2,,BKFIL2)                                    
             ENDIF ,                                                            
                                                                                
           WHEN (COPY)             MODE=COPY:                                   
             IF (TM,COUNTFLG,COUNTYES,O) IF COUNT= CARD IS PRESENT:             
               MVI   PLCOUNT,X'FF'       TELL LDCOUNT TO PRINT TOTALS           
               GOTO1 VLDCOUNT,PLCOUNT                                           
             ENDIF ,                                                            
             IF (CLI,E35EXIT,EQ,NO)    IF RUNNING AS A DFSORT E35 EXIT:         
               CLOSE (IFILE,,OFILE)                                             
               IF (CLI,SPLIT,EQ,YES)     SPLIT FILE?                            
                 CLOSE (OFIL2)                                                  
               ENDIF ,                                                          
             ENDIF ,                                                            
                                                                                
           WHEN (COMPARE)          MODE=COMPARE:                                
             IF (TM,COUNTFLG,COUNTYES,O) IF COUNT= CARD IS PRESENT:             
               MVI   PLCOUNT,X'FF'       TELL LDCOUNT TO PRINT TOTALS           
               GOTO1 VLDCOUNT,PLCOUNT                                           
             ENDIF ,                                                            
             XC    VSREC.DVKEY,VSREC.DVKEY                                      
             BRAS  RE,COMPARE_FILES    COMPLETE COMPARE                         
             CLOSE (DEMACB,,NEWFILE,,OLDFILE,,DIFFILE)                          
             IF (CLI,SPLIT,EQ,YES)     SPLIT FILE?                              
               CLOSE (DEMAC2)                                                   
             ENDIF ,                                                            
                                                                                
           OTHRWISE ,                                                           
             J *+2                 INVALID MODE ?!?                             
                                                                                
         ENDSEL ,                                                               
                                                                                
         IF (CLI,E35EXIT,EQ,NO)    IF WE'RE NOT RUNNING AS AN E35 EXIT:         
           CLOSE (IFILE)             CLOSE FILE                                 
         ENDIF ,                                                                
                                                                                
         BAS   RE,TOTALS           PRINT TOTALS                                 
                                                                                
         CLI   E35EXIT,YES         IF NOT RUNNING AS A DFSORT EXIT              
         JNE   XBASE               USE NORMAL RETURN TO MVS.                    
                                                                                
         IF (CLC,RETCODE,GE,=H'16') IF WE'VE SET A SEVERE RC:                   
           MVC   E35RC,=H'16'       TELL DFSORT TO TERMINATE WITH RC=16         
         ELSE ,                                                                 
           MVC   E35RC,=H'8'        NORMALLY SET DFSORT RC=8 AT EOF             
         ENDIF ,                                                                
                                                                                
         J     E35GOBAK             RETURN TO DFSORT                            
*                                                                               
XBASE    DS    0H                                                               
         XBASE RC=RETCODE,RL=2     RETURN BACK TO MVS                           
*                                                                               
         DROP  VSREC                                                            
         DROP  R3                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* CHECK FORMAT OF RECORD AT R3. COUNT IF BAD. FIX MISSING TERMINATOR  *         
*        COUNT ERRORS BY TYPE.                                        *         
*        FIX MISSING TERMINATOR.                                      *         
*        PRINT FIRST THREE BYTES                                      *         
***********************************************************************         
         USING OTREC,R3            R3=A(OUTPUT RECORD)                          
VSREC    USING DVREC,OTVSREC       VSAM RECORD IN OUTPUT RECORD                 
*                                                                               
RECCHECK DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
         LH    RE,OTLENMVS         GET MVS LENGTH                               
         AHI   RE,-4               LESS RDW LENGTH                              
         CLM   RE,B'0011',VSREC.DVLENDDS  SHOULD BE >= DDS LENGTH               
         JL    RECCHE1             RDW LENGTH INCORRECT                         
         ICM   RE,B'0011',VSREC.DVLENDDS GET DDS LENGTH                         
         LA    RE,VSREC.DVREC(RE)  POINT PAST END OF DDS RECORD                 
         LA    R1,VSREC.DVFRSTEL   POINT TO FIRST ELEMENT                       
         XR    R0,R0                                                            
*                                                                               
RECCH10  DS    0H                                                               
         CLI   0(R1),0             TEST EOR (COULD BE NO ELEMENTS)              
         JE    RECCH20                                                          
         ICM   R0,B'0001',1(R1)    ELEMENT LENGTH                               
         JZ    RECCHE3             BAD ELEMENT LENGTH                           
         AR    R1,R0                                                            
         CR    R1,RE               TEST AT END                                  
         JL    RECCH10             NOT YET, LOOP                                
         JH    RECCHE2             GONE PAST IT, DDS LENGTH BAD                 
*                                                                               
         AP    CTRRCW01,=P'1'      COUNT MISSING EOR                            
         MVI   CTRRC,YES                                                        
         LARL  RF,LTCRCW01                                                      
         MVC   P+12(L'LTCRCW01),0(RF)                                           
         MVI   0(R1),0             ADD AN EOR                                   
         LA    R1,1(,R1)                                                        
         J     RECCH22             ADJUST LENGTH                                
*                                                                               
RECCH20  DS    0H                                                               
         LA    R1,1(,R1)           EOR MUST BE LAST BYTE OF DDS RECORD          
         CR    R1,RE                                                            
         JE    RECCHXIT                                                         
         AP    CTRRCW02,=P'1'      EOR BEFORE END OF DDS RECORD                 
         MVI   CTRRC,YES                                                        
         LARL  RF,LTCRCW02                                                      
         MVC   P+12(L'LTCRCW02),0(RF)                                           
*                                                                               
RECCH22  DS    0H                                                               
         LA    RE,VSREC.DVREC      POINT TO BEGINNING OF DDS REC                
         SR    R1,RE               GET NEW LENGTH                               
         STCM  R1,B'0011',VSREC.DVLENDDS SET DDS LENGTH                         
         LA    R1,4(,R1)                                                        
         STH   R1,OTLENMVS         SET MVS LENGTH                               
         J     RECCHPRT                                                         
*                                                                               
RECCHE1  DS    0H                                                               
         AP    CTRRCE01,=P'1'      RDW LENGTH TOO SMALL                         
         MVI   CTRRC,YES                                                        
         LARL  RF,LTCRCE01                                                      
         MVC   P+12(L'LTCRCE01),0(RF)                                           
         J     RECCHPRT                                                         
*                                                                               
RECCHE2  DS    0H                                                               
         AP    CTRRCE02,=P'1'      DDS LENGTH TOO SMALL                         
         MVI   CTRRC,YES                                                        
         LARL  RF,LTCRCE02                                                      
         MVC   P+12(L'LTCRCE02),0(RF)                                           
         J     RECCHPRT                                                         
*                                                                               
RECCHE3  DS    0H                                                               
         AP    CTRRCE03,=P'1'      ZERO ELEMENT LENGTH                          
         MVI   CTRRC,YES                                                        
         LARL  RF,LTCRCE03                                                      
         MVC   P+12(L'LTCRCE03),0(RF)                                           
*                                                                               
RECCHPRT DS    0H                                                               
         CLC   LRECCHKY,VSREC.DVREC  HAVE WE PRINTED THIS ONE ALREADY?          
         JNE   *+14                NO, SKIP                                     
         MVC   P+12(30),P+11       RESET TO SPACES                              
         J     RECCHXIT                                                         
*                                                                               
         MVC   LRECCHKY,VSREC.DVREC                                             
         MVC   P(7),=C'KEY(3)='                                                 
         MVC   P+7(3),LRECCHKY                                                  
         GOTOR VPRINTER                                                         
*                                                                               
RECCHXIT DS    0H                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         DROP  VSREC                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS FOR THE CURRENTLY PROCESSING FILE/MEDIA/SOURCE.        *         
* R2 = A(CURRENTLY PROCESSED RECORD)                                  *         
***********************************************************************         
PRNTCURT DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID1(50),=C'FMS                   #RECORDS              +        
               #BYTES'                                                          
         MVC   MID2,SPACES                                                      
         MVC   MID2(50),=C'--------------------------------------------+        
               ------'                                                          
*                                                                               
         MVC   P(L'THISKFMS),THISKFMS     PRINT THIS F/M/S KEY                  
         AP    CTRINFMS,=P'-1'            DON'T COUNT CURRENT RECORD            
         EDIT  CTRINFMS,(15,P+15),COMMAS=YES,ZERO=NOBLANK                       
         AP    CTRFMSTO,CTRINFMS          KEEP A RUNNING F/M/S TOTAL            
         ZAP   CTRINFMS,=P'1'             RESET RECORD COUNTER                  
         LG    GR1,FMSBYTES               # BYTES                               
         ICM   R0,B'0011',0(R2)           RECLEN (FROM RDW)                     
         SGFR  GR1,R0                                                           
         STG   GR1,FMSBYTES                                                     
         IF (CLI,MODE,NE,UPDATE)          PRINT IN KB UNLESS UPDATE             
           AGHI  GR1,512                  +512                                  
           SRAG  GR1,GR1,10               /1024                                 
           MVC   P+50(2),=C'KB'                                                 
         ENDIF ,                                                                
         CVDG  GR1,LONG                   VALUE IS IN DUB1+DUB2                 
         EDIT  (P8,DUB2),(15,P+35),COMMAS=YES,ZERO=NOBLANK                      
         GOTO1 VPRINTER                                                         
         XC    FMSBYTES,FMSBYTES          REINIT FMS BYTE COUNTER...            
         STH   R0,FMSBYTES+L'FMSBYTES-2   ...WITH RECLEN (FROM RDW)             
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB,RA                                                            
         EJECT                                                                  
***********************************************************************         
* PUT OUTPUT RECORD AT 0(R0)                                          *         
* NOTE IF UNDER DFSORT, SPLIT O/P VIA DFSORT INCLUDE/OMIT CARDS       *         
***********************************************************************         
PUTOUT   NTR1  BASE=(*,PUTOUT_X),LABEL=*                                        
         LR    RF,R0                                                            
*                                                                               
         CLI   SPLIT,YES                                                        
         JNE   PUTOUT10                                                         
         CLC   4(3,RF),=C'RTN'     'RTN' RECORDS ARE HALF OF DEMVSMN            
         JE    PUTOUT20            AND FORM THE SECOND HALF OF SPLIT            
*                                                                               
PUTOUT10 DS    0H                                                               
         AP    CTROUT,=P'1'        COUNT OFILE RECORDS OUT                      
         XR    RE,RE                                                            
         ICM   RE,B'0011',0(RF)                                                 
         LG    GR1,OTBYTES         ACCUMULATE OUTPUT BYTES                      
         AGFR  GR1,RE                                                           
         STG   GR1,OTBYTES                                                      
         LARL  R1,OFILE                                                         
         J     PUTOUT50                                                         
*                                                                               
PUTOUT20 DS    0H                                                               
         AP    CTROU2,=P'1'        COUNT OFIL2 'RTN' RECORDS OUT                
         XR    RE,RE                                                            
         ICM   RE,B'0011',0(RF)                                                 
         LG    GR1,OTBYTE2         ACCUMULATE OUTPUT DDS BYTES                  
         AGFR  GR1,RE                                                           
         STG   GR1,OTBYTE2                                                      
         LARL  R1,OFIL2                                                         
*                                                                               
PUTOUT50 DS    0H                                                               
         CH    RE,OMAXRLEN         SET MAXIMUM LRECL                            
         JNH   *+8                                                              
         STH   RE,OMAXRLEN                                                      
*                                                                               
         IF (CLI,E35EXIT,EQ,NO)    IF NOT RUNNING AS A DFSORT E35 EXIT:         
           PUT   (1),(0)             WRITE RECORD POINTED TO BY R0              
         ENDIF ,                     (O/W, DFSORT WILL WRITE THE REC.)          
*                                                                               
         J     XIT1                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
PUTOUT_X EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* PUT OUTPUT RECORD AT 0(R0) TO BACKOUT FILE                          *         
***********************************************************************         
PUTBAK   NTR1  BASE=(*,PUTBAK_X),LABEL=*                                        
         LR    RF,R0                                                            
*                                                                               
         CLI   SPLIT,YES                                                        
         JNE   PUTBAK1                                                          
         CLC   4(3,RF),=C'RTN'     'RTN' RECORDS ARE HALF OF DEMVSMN            
         JE    PUTBAK2             AND FORM THE SECOND HALF OF SPLIT            
*                                                                               
PUTBAK1  DS    0H                                                               
         LARL  R1,BKFILE                                                        
         PUT   (1),(0)             WRITE RECORD AT (R0) TO BACKOUT FILE         
         AP    CTRBAK,=P'1'        COUNT BACKOUT RECORDS                        
         J     PUTBAKX                                                          
*                                                                               
PUTBAK2  DS    0H                                                               
         LARL  R1,BKFIL2                                                        
         PUT   (1),(0)             WRITE RECORD AT (R0) TO BACKOUT FILE         
         AP    CTRBA2,=P'1'        COUNT BACKOUT RECORDS                        
*                                                                               
PUTBAKX  DS    0H                                                               
         J     XIT1                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
PUTBAK_X EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT RECORD TOTALS ETC.                                            *         
***********************************************************************         
*                                                                               
TOTALS NTR1  BASE=*,LABEL=*                                                     
*                                                                               
       LAY   RA,4096(,RB)          THIS ROUTINE NEEDS A 2ND BASE REG.           
       USING (TOTALS+4096,TOTALS_X),RA                                          
                                                                                
       ZAP   LINE,=P'99'                                                        
       MVC   MID1,SPACES                                                        
       MVC   MID1(7),=C'SUMMARY'                                                
       MVC   MID2,SPACES                                                        
       MVC   MID2(7),=C'-------'                                                
                                                                                
       IF (CP,CTRMAJKY,EQ,=P'0') AVOID ZERO DIVIDE                              
         ZAP CTRMINAV,=P'0'                                                     
       ELSE ,                                                                   
         ZAP   PL16,CTRMINAV     TOTAL NUMBER OF MINOR KEYS                     
         IF (NZ)                 IF ZERO, NOTHING TO DO                         
           MP    PL16,=P'100'      TWO DECIMAL PLACES                           
           DP    PL16,CTRMAJKY     TOTAL NUMBER OF MAJOR KEYS                   
           AP    PL16LOW,PL16LOW   DOUBLE REMAINDER                             
           IF (CP,PL16LOW,H,CTRMAJKY)                                           
             AP    PL16HIGH,=P'1'  IF > DIVISOR, ROUND RESULT UP                
           ENDIF ,                                                              
           MVC   CTRMINAV,PL16     COPY AS AVERAGE                              
         ENDIF ,                                                                
       ENDIF ,                                                                  
                                                                                
       IF (CP,CTRDUPKY,EQ,=P'0') AVOID ZERO DIVIDE                              
         ZAP CTRDUPAV,=P'0'                                                     
       ELSE ,                                                                   
         ZAP   PL16,CTRDUPAV     TOTAL NUMBER OF DUPLICATE KEYS                 
         IF (NZ)                 IF ZERO, NOTHING TO DO                         
           MP    PL16,=P'100'      TWO DECIMAL PLACES                           
           DP    PL16,CTRDUPKY     TOTAL NUMBER OF KEYS DUPLICATED              
           AP    PL16LOW,PL16LOW   DOUBLE REMAINDER                             
           IF (CP,PL16LOW,H,CTRDUPKY)                                           
            AP    PL16HIGH,=P'1'   IF > DIVISOR, ROUND RESULT UP                
           ENDIF ,                                                              
           MVC   CTRDUPAV,PL16     COPY AS AVERAGE                              
         ENDIF ,                                                                
       ENDIF ,                                                                  
                                                                                
       LARL  R1,LTCIN            RECORDS READ                                   
       MVC   P(L'LTCIN),0(R1)                                                   
       EDIT  CTRIN,(15,P+35),COMMAS=YES,ZERO=NOBLANK                            
       ZAP   DUB,CTRAOF          DUB= #RECS. ALREADY ON FILE +                  
       AP    DUB,CTRDTDF              #RECS. THAT MATCH EXCEPT DATE             
       IF (CP,CTRIN,NE,=P'0'),AND,                                              
          (CP,CTRIN,EQ,DUB)                                                     
         LARL  R1,LTCWPDUP         WARNING: POSSIBLE ERROR                      
         MVC   P+55(L'LTCWPDUP),0(R1)                                           
         LA    R1,4                SET RC TO 4: NOTHING TO UPDATE               
         BRAS  RE,SETRC                                                         
       ENDIF ,                                                                  
       GOTOR VPRINTER                                                           
                                                                                
       LG    GR1,IFBYTES         BYTES                                          
       AG    GR1,IDBYTES         BYTES                                          
       IF (CGFI,GR1,H,999999)                                                   
         AGHI  GR1,512             +512                                         
         SRAG  GR1,GR1,10          /1024                                        
         MVC   P+50(2),=C'KB'                                                   
       ENDIF ,                                                                  
                                                                                
       CVDG  GR1,LONG            VALUE IS IN DUB1+DUB2                          
       LARL  R1,LTCINB           BYTES READ, INC RDW                            
       MVC   P(L'LTCINB),0(R1)                                                  
       MVC   P+30(20),ED_PATTERN1 ###,###,###,###,###                           
       ED    P+30(20),DUB2                                                      
       GOTOR VPRINTER                                                           
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCFIL           DA RECORDS READ                                
       MVC   P(L'LTCFIL),0(R1)                                                  
       EDIT  CTRFIL,(15,P+35),COMMAS=YES,ZERO=NOBLANK                           
       GOTOR VPRINTER                                                           
                                                                                
       LG    GR1,IFBYTES         BYTES                                          
       IF (CGFI,GR1,H,999999)                                                   
         AGHI  GR1,512             +512                                         
         SRAG  GR1,GR1,10          /1024                                        
         MVC   P+50(2),=C'KB'                                                   
       ENDIF ,                                                                  
                                                                                
       CVDG  GR1,LONG            VALUE IS IN DUB1+DUB2                          
       LARL  R1,LTCFILB          DA BYTES READ, INC RDW                         
       MVC   P(L'LTCFILB),0(R1)                                                 
       MVC   P+30(20),ED_PATTERN1 ###,###,###,###,###                           
       ED    P+30(20),DUB2                                                      
       GOTOR VPRINTER                                                           
                                                                                
       IF (CP,CTRFILTD,NE,=P'0')                                                
         LARL  R1,LTCFILTD         TEST DATA RECORDS READ                       
         MVC   P+2(L'LTCFILTD),0(R1)                                            
         EDIT  CTRFILTD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
       ENDIF ,                                                                  
                                                                                
       IF (CP,CTRFILX,NE,=P'0')                                                 
         LARL  R1,LTCFILX        DA RECORDS MARKED AS EXTENDED PSV              
         MVC   P+2(L'LTCFILX),0(R1)                                             
         EDIT  CTRFILX,(15,P+35),COMMAS=YES,ZERO=NOBLANK                        
         LARL  R1,LTCMBZ         PRINT WARNING: SHOULD NEVER HAPPEN             
         MVC   P+55(L'LTCMBZ),0(R1)                                             
         LA    R1,8              RETURN CODE 8 IF UNEXPECTED EXT FLAG           
         BRAS  RE,SETRC                                                         
         GOTOR VPRINTER                                                         
       ENDIF ,                                                                  
                                                                                
       IF (CLI,MODE,EQ,UPDATE)   IF MODE=UPDATE:                                
         LARL  R1,LTCFILM          DA RECORDS MARKED DELETED                    
         MVC   P+2(L'LTCFILM),0(R1)                                             
       ELSE ,                                                                   
         LARL  R1,LTCFILD          O/W RECORDS REALLY ARE DELETED               
         MVC   P+2(L'LTCFILD),0(R1)                                             
       ENDIF ,                                                                  
                                                                                
       EDIT  CTRFILD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                          
       IF (CP,CTRFILD,NE,=P'0'),AND,                                            
          (CLI,MODE,EQ,UPDATE)   CHECK FOR DELETED MINOR KEYS                   
         LARL  R1,LTCFLDUP       IGNORED: CAN'T DELETE SINGLE MINORS            
         MVC   P+55(L'LTCFLDUP),0(R1)                                           
         LA    R1,16             RETURN CODE 16 IF UNEXPECTED DELETES           
         BRAS  RE,SETRC                                                         
       ENDIF ,                                                                  
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCMAJKY         MAJOR KEYS (EXCLUDING PASSIVES)                
       MVC   P+2(L'LTCMAJKY),0(R1)                                              
       EDIT  CTRMAJKY,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCMINMX         MAXIMUM MINOR KEYS PER MAJOR                   
       MVC   P+2(L'LTCMINMX),0(R1)                                              
       EDIT  CTRMINMX,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCMINAV         AVERAGE MINOR KEYS PER MAJOR                   
       MVC   P+2(L'LTCMINAV),0(R1)                                              
       EDIT  CTRMINAV,(15,P+35),2,COMMAS=YES,ZERO=NOBLANK                       
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCDUPKY         DUPLICATED KEYS                                
       MVC   P+2(L'LTCDUPKY),0(R1)                                              
       EDIT  CTRDUPKY,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCDUPMX         MAXIMUM DUPLICATES PER KEY                     
       MVC   P+2(L'LTCDUPMX),0(R1)                                              
       EDIT  CTRDUPMX,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCDUPAV         AVERAGE DUPLICATES PER KEY                     
       MVC   P+2(L'LTCDUPAV),0(R1)                                              
       EDIT  CTRDUPAV,(15,P+35),2,COMMAS=YES,ZERO=NOBLANK                       
       GOTOR VPRINTER                                                           
                                                                                
       IF (CLI,DTF#,NE,0),AND,   IF WE CHECKED FOR PEELED:                      
          (CLI,MODE,NE,UPDATE)                                                  
         LARL  R1,LTCFILPL         RECORDS PEELED                               
         MVC   P+2(L'LTCFILPL),0(R1)                                            
         EDIT  CTRFILPL,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
       ENDIF ,                                                                  
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCDIR           DIRECTORY RECORDS READ                         
       MVC   P(L'LTCDIR),0(R1)                                                  
       EDIT  CTRDIR,(15,P+35),COMMAS=YES,ZERO=NOBLANK                           
       GOTOR VPRINTER                                                           
                                                                                
       LG    GR1,IDBYTES         BYTES                                          
       IF (CGFI,GR1,H,999999)                                                   
         AGHI  GR1,512             +512                                         
         SRAG  GR1,GR1,10          /1024                                        
         MVC   P+50(2),=C'KB'                                                   
       ENDIF ,                                                                  
       CVDG  GR1,LONG            VALUE IS IN DUB1+DUB2                          
       LARL  R1,LTCDIRB          DIRECTORY BYTES READ, INC RDW                  
       MVC   P(L'LTCDIRB),0(R1)                                                 
       MVC   P+30(20),ED_PATTERN1 ###,###,###,###,###                           
       ED    P+30(20),DUB2                                                      
       GOTOR VPRINTER                                                           
                                                                                
       IF (CP,CTRFILTD,NE,=P'0')                                                
         LARL  R1,LTCDIRTD         TEST DATA RECORDS READ                       
         MVC   P+2(L'LTCDIRTD),0(R1)                                            
         EDIT  CTRDIRTD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
       ENDIF ,                                                                  
                                                                                
       LARL  R1,LTCPASX          EXTENDED PASSIVES                              
       MVC   P+2(L'LTCPASX),0(R1)                                               
       EDIT  CTRPASX,(15,P+35),COMMAS=YES,ZERO=NOBLANK                          
       GOTOR VPRINTER                                                           
                                                                                
       IF (CLI,MODE,EQ,UPDATE)   IF MODE=UPDATE:                                
         LARL  R1,LTCPASXM         EXTENDED PASSIVES *MARKED* DELETED           
         MVC   P+2(L'LTCPASXM),0(R1)                                            
       ELSE ,                                                                   
         LARL  R1,LTCPASXD         O/W THEY REALLY ARE DELETED                  
         MVC   P+2(L'LTCPASXD),0(R1)                                            
       ENDIF ,                                                                  
       EDIT  CTRPASXD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCDNX           OTHER DIRECTORY RECORDS                        
       MVC   P+2(L'LTCDNX),0(R1)                                                
       EDIT  CTRDNX,(15,P+35),COMMAS=YES,ZERO=NOBLANK                           
       GOTOR VPRINTER                                                           
                                                                                
       IF (CLI,MODE,EQ,UPDATE)   IF MODE=UPDATE:                                
         LARL  R1,LTCDNXD          THIS COULD BE "ACTIVE" OR PASSIVE            
         MVC   P+2(L'LTCDNXD),0(R1)                                             
       ELSE ,                                                                   
         LARL  R1,LTCDNXP          O/W: CAN ONLY BE PASSIVE                     
         MVC   P+2(L'LTCDNXP),0(R1)                                             
       ENDIF ,                                                                  
       EDIT  CTRDNXD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                          
       GOTOR VPRINTER                                                           
                                                                                
       IF (CLI,DTF#,NE,0),AND,   IF WE CHECKED FOR PEELED:                      
          (CLI,MODE,NE,UPDATE)                                                  
         LARL  R1,LTCDIRPL         DIRECTORY RECORDS PEELED                     
         MVC   P+2(L'LTCDIRPL),0(R1)                                            
         EDIT  CTRDIRPL,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
       ENDIF ,                                                                  
       GOTOR VPRINTER                                                           
                                                                                
       IF (CLI,MODE,NE,COMPARE)  IF MODE=COPY OR MODE=UPDATE:                   
                                                                                
         LARL  R1,LTCOUT           OFILE RECORDS WRITTEN                        
         MVC   P(L'LTCOUT),0(R1)                                                
         IF (CLI,MODE,EQ,UPDATE)   IF UPDATE MODE:                              
           MVC   P(7),=C'UPDATE '                                               
           MVC   P+7(L'LTCOUT),0(R1)                                            
         ENDIF ,                                                                
         EDIT  CTROUT,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
         GOTOR VPRINTER                                                         
                                                                                
         LG    GR1,OTBYTES         BYTES                                        
         IF (CGFI,GR1,H,999999)                                                 
           AGHI  GR1,512             +512                                       
           SRAG  GR1,GR1,10          /1024                                      
           MVC   P+50(2),=C'KB'                                                 
         ENDIF ,                                                                
         CVDG  GR1,LONG            VALUE IS IN DUB1+DUB2                        
         LARL  R1,LTCOUTB          OFILE BYTES WRITTEN, INC RDW                 
         MVC   P(L'LTCOUTB),0(R1)                                               
         MVC   P+30(20),ED_PATTERN1 ###,###,###,###,###                         
         ED    P+30(20),DUB2                                                    
         GOTOR VPRINTER                                                         
                                                                                
         IF (CLI,SPLIT,EQ,YES)                                                  
           LARL  R1,LTCOU2           OFIL2 RECORDS WRITTEN                      
           MVC   P(L'LTCOU2),0(R1)                                              
           IF (CLI,MODE,EQ,UPDATE)   IF MODE=UPDATE:                            
             MVC   P(7),=C'UPDATE '                                             
             MVC   P+7(L'LTCOU2),0(R1)                                          
           ENDIF ,                                                              
           EDIT  CTROU2,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
           GOTOR VPRINTER                                                       
           LG    GR1,OTBYTE2         BYTES                                      
           IF (CGFI,GR1,H,999999)                                               
             AGHI  GR1,512             +512                                     
             SRAG  GR1,GR1,10          /1024                                    
             MVC   P+50(2),=C'KB'                                               
           ENDIF ,                                                              
           CVDG  GR1,LONG            VALUE IS IN DUB1+DUB2                      
           LARL  R1,LTCOU2B          OFIL2 BYTES WRITTEN, INC RDW               
           MVC   P(L'LTCOU2B),0(R1)                                             
           MVC   P+30(20),ED_PATTERN1 ###,###,###,###,###                       
           ED    P+30(20),DUB2                                                  
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
                                                                                
         LARL  R1,LTCMAXLN         MAX O/P RECORD LENGTH, INC RDW               
         MVC   P(L'LTCMAXLN),0(R1)                                              
         LH    R1,OMAXRLEN                                                      
         CVD   R1,DUB1                                                          
         EDIT  (P8,DUB1),(15,P+35),COMMAS=YES,ZERO=NOBLANK                      
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCAVGLN         AVG O/P RECORD LENGTH, INC RDW               
         MVC   P(L'LTCAVGLN),0(R1)                                              
         XC    DUB1,DUB1                                                        
         ZAP   DUB2,CTROUT                                                      
         AP    DUB2,CTROU2                                                      
         IF (NZ)                                                                
           CVBG  GRE,LONG                                                       
           XGR   GR0,GR0           CALC. AVERAGE LRECL. GR0/1=BYTES             
           LG    GR1,OTBYTES                                                    
           AG    GR1,OTBYTE2                                                    
           DLGR  GR0,GRE           DON'T CARE ABOUT ROUNDING                    
           CVDG  GR1,LONG                                                       
         ENDIF ,                                                                
         EDIT  (P8,DUB2),(15,P+35),COMMAS=YES,ZERO=NOBLANK                      
         GOTOR VPRINTER                                                         
                                                                                
         IF (CLI,MODE,EQ,UPDATE)   IF MODE=UPDATE:                              
           LARL  R1,LTCBAK           BKFILE RECORDS WRITTEN                     
           MVC   P(L'LTCBAK),0(R1)                                              
           EDIT  CTRBAK,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
           GOTOR VPRINTER                                                       
           IF (CLI,SPLIT,EQ,YES)                                                
             LARL  R1,LTCBA2           BKFIL2 RECORDS WRITTEN                   
             MVC   P(L'LTCBA2),0(R1)                                            
             EDIT  CTRBA2,(15,P+35),COMMAS=YES,ZERO=NOBLANK                     
             GOTOR VPRINTER                                                     
           ENDIF ,                                                              
         ENDIF ,                                                                
         GOTOR VPRINTER                                                         
                                                                                
       ELSE ,                      IF MODE=COMPARE:                             
                                                                                
         LARL  R1,LTCNEWF          NEWFILE RECORDS WRITTEN                      
         MVC   P(L'LTCNEWF),0(R1)                                               
         EDIT  CTRNEWF,(15,P+35),COMMAS=YES,ZERO=NOBLANK                        
         GOTOR VPRINTER                                                         
         LARL  R1,LTCOLDF          OLDFILE RECORDS WRITTEN                      
         MVC   P(L'LTCOLDF),0(R1)                                               
         EDIT  CTROLDF,(15,P+35),COMMAS=YES,ZERO=NOBLANK                        
         GOTOR VPRINTER                                                         
         LARL  R1,LTCDIFF          DIFFILE RECORDS WRITTEN                      
         MVC   P(L'LTCDIFF),0(R1)                                               
         EDIT  CTRDIFF,(15,P+35),COMMAS=YES,ZERO=NOBLANK                        
         GOTOR VPRINTER                                                         
         GOTOR VPRINTER                                                         
                                                                                
       ENDIF ,                                                                  
                                                                                
       IF (CLI,CTRRC,EQ,YES)       IF RECCHECK FLAGGED SOMETHING:               
         IF (CP,CTRRCE01,NE,=P'0')                                              
           LARL  R1,LTCRCE01         RDW LENGTH TOO SMALL                       
           MVC   P(L'LTCRCE01),0(R1)                                            
           EDIT  CTRRCE01,(15,P+35),COMMAS=YES,ZERO=NOBLANK                     
           MVC   P+55(9),=C'**ERROR**'                                          
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
                                                                                
         IF (CP,CTRRCE02,NE,=P'0')                                              
           LARL  R1,LTCRCE02         DDS LENGTH TOO SMALL                       
           MVC   P(L'LTCRCE02),0(R1)                                            
           EDIT  CTRRCE02,(15,P+35),COMMAS=YES,ZERO=NOBLANK                     
           MVC   P+55(9),=C'**ERROR**'                                          
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
                                                                                
         IF (CP,CTRRCE03,NE,=P'0')                                              
           LARL  R1,LTCRCE03         ELEMENT LENGTH ZERO                        
           MVC   P(L'LTCRCE03),0(R1)                                            
           EDIT  CTRRCE03,(15,P+35),COMMAS=YES,ZERO=NOBLANK                     
           MVC   P+55(9),=C'**ERROR**'                                          
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
                                                                                
         IF (CP,CTRRCW01,NE,=P'0')                                              
           LARL  R1,LTCRCW01         MISSING EOR, ADDED                         
           MVC   P(L'LTCRCW01),0(R1)                                            
           EDIT  CTRRCW01,(15,P+35),COMMAS=YES,ZERO=NOBLANK                     
           MVC   P+55(9),=C'*WARNING*'                                          
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
                                                                                
         IF (CP,CTRRCW02,NE,=P'0')                                              
           LARL  R1,LTCRCW02         EOR BEFORE RECORD END, FIXED               
           MVC   P(L'LTCRCW02),0(R1)                                            
           EDIT  CTRRCW02,(15,P+35),COMMAS=YES,ZERO=NOBLANK                     
           MVC   P+55(9),=C'*WARNING*'                                          
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
         GOTOR VPRINTER                                                         
       ENDIF ,                                                                  
                                                                                
       IF (CLI,MODE,EQ,UPDATE)     IF MODE=UPDATE:                              
                                                                                
         LARL  R1,LTCINSRT         NEW RECORDS ADDED TO FILE                    
         MVC   P(L'LTCINSRT),0(R1)                                              
         EDIT  CTRINSRT,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCREPL          RECORDS WITH SAME KEY AS FILE                
         MVC   P(L'LTCREPL),0(R1)                                               
         EDIT  CTRREPL,(15,P+35),COMMAS=YES,ZERO=NOBLANK                        
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCREPLD         DELETED RECORDS WITH SAME KEY                
         MVC   P(L'LTCREPLD),0(R1)                                              
         EDIT  CTRREPLD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCAOF           RECORDS IDENTICAL TO FILE, IGNORED           
         MVC   P(L'LTCAOF),0(R1)                                                
         EDIT  CTRAOF,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCDTDF          RECORDS MATCH EXCEPT FOR CONV. DATE          
         MVC   P(L'LTCDTDF),0(R1)                                               
         EDIT  CTRDTDF,(15,P+35),COMMAS=YES,ZERO=NOBLANK                        
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCDDF           DELETING DELETED RECORD, IGNORED             
         MVC   P(L'LTCDDF),0(R1)                                                
         EDIT  CTRDDF,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCDOF           DELETED RECORDS ON FILE REPLACED             
         MVC   P(L'LTCDOF),0(R1)                                                
         EDIT  CTRDOF,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCDELET         UNMATCHED DA MINOR KEYS DELETED              
         MVC   P(L'LTCDELET),0(R1)                                              
         EDIT  CTRDELET,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCDELWR         "DELETE" RECORDS WRITTEN                     
         MVC   P(L'LTCDELWR),0(R1)                                              
         EDIT  CTRDELWR,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCPTRD          DIR REC ASSUMED DELETED POINTER              
         MVC   P(L'LTCPTRD),0(R1)                                               
         EDIT  CTRPTRD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                        
         GOTOR VPRINTER                                                         
                                                                                
         IF (CP,CTRERP2F,NE,=P'0')                                              
           LARL  R1,LTCERP2F         PASSIVE REPLACE MAJOR, IGNORED             
           MVC   P(L'LTCERP2F),0(R1)                                            
           EDIT  CTRERP2F,(15,P+35),COMMAS=YES,ZERO=NOBLANK                     
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
                                                                                
         IF (CP,CTRERF2P,NE,=P'0')                                              
           LARL  R1,LTCERF2P         MAJOR REPLACE PASSIVE, IGNORED             
           MVC   P(L'LTCERF2P),0(R1)                                            
           EDIT  CTRERF2P,(15,P+35),COMMAS=YES,ZERO=NOBLANK                     
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
                                                                                
         IF (CP,CTRERDNF,NE,=P'0')                                              
           LARL  R1,LTCERDNF         KEY TO DELETE NOT FOUND, IGNORED           
           MVC   P(L'LTCERDNF),0(R1)                                            
           EDIT  CTRERDNF,(15,P+35),COMMAS=YES,ZERO=NOBLANK                     
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
         GOTOR VPRINTER                                                         
                                                                                
         MVC   P+20+15-5(5),=C'ADDED'                                           
         MVC   P+35+15-8(8),=C'REPLACED'                                        
         MVC   P+50+15-7(7),=C'DELETED'                                         
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCMJ            MINOR KEY SETS                               
         MVC   P(L'LTCMJ),0(R1)                                                 
         EDIT  CTRNEWMJ,(15,P+20),COMMAS=YES,ZERO=NOBLANK                       
         EDIT  CTRUPDMJ,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         EDIT  CTRDELMJ,(15,P+50),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCMN            MINOR KEYS                                   
         MVC   P(L'LTCMN),0(R1)                                                 
         EDIT  CTRNEWMN,(15,P+20),COMMAS=YES,ZERO=NOBLANK                       
         EDIT  CTRUPDMN,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         EDIT  CTRDELMN,(15,P+50),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCPV            ORDINARY PASSIVES                            
         MVC   P(L'LTCPV),0(R1)                                                 
         EDIT  CTRNEWPV,(15,P+20),COMMAS=YES,ZERO=NOBLANK                       
         EDIT  CTRUPDPV,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         EDIT  CTRDELPV,(15,P+50),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCPX            EXTENDED PASSIVES                            
         MVC   P(L'LTCPX),0(R1)                                                 
         EDIT  CTRNEWPX,(15,P+20),COMMAS=YES,ZERO=NOBLANK                       
         EDIT  CTRUPDPX,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         EDIT  CTRDELPX,(15,P+50),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
         GOTOR VPRINTER                                                         
                                                                                
       ELSEIF (CLI,MODE,EQ,COMPARE)  ELSE, IF COMPARE MODE:                     
                                                                                
         LARL  R1,LTCCMPDL         DELETED VSAM RECORDS IGNORED                 
         MVC   P(L'LTCCMPDL),0(R1)                                              
         EDIT  CTRCMPDL,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         IF (CLI,DTF#,NE,0)        IF WE CHECKED FOR PEELED:                    
           LARL  R1,LTCCMPPL      VSAM RECS. IGNORED (SHOULD BE PEELED)         
           MVC   P(L'LTCCMPPL),0(R1)                                            
           EDIT  CTRCMPPL,(15,P+35),COMMAS=YES,ZERO=NOBLANK                     
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
                                                                                
         LARL  R1,LTCCMPEQ         MATCHED RECORDS                              
         MVC   P(L'LTCCMPEQ),0(R1)                                              
         EDIT  CTRCMPEQ,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCCMPUN         UNMATCHED INPUT RECORDS                      
         MVC   P(L'LTCCMPUN),0(R1)                                              
         EDIT  CTRCMPUN,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCCMPUV         UNMATCHED VSAM RECORDS                       
         MVC   P(L'LTCCMPUV),0(R1)                                              
         EDIT  CTRCMPUV,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCCMPNE         MATCHED KEY, DATA DIFFERENT                  
         MVC   P(L'LTCCMPNE),0(R1)                                              
         EDIT  CTRCMPNE,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
*                                  THESE THREE COUNTERS SHOULD BE ZERO          
         IF (CP,CTRCMPUN,NE,=P'0'),OR, UNMATCHED INPUT RECORDS                  
            (CP,CTRCMPUV,NE,=P'0'),OR, UNMATCHED VSAM RECORDS                   
            (CP,CTRCMPNE,NE,=P'0')     MATCHED KEY BUT DATA DIFFERENT           
           LA    R1,16              IF NOT, COMPARE FAILED, SET RC 16           
           BRAS  RE,SETRC                                                       
         ENDIF ,                                                                
         GOTOR VPRINTER                                                         
                                                                                
       ENDIF ,                                                                  
                                                                                
       L     R2,VPRNTER            MUST CLOSE PRINT FILE BECAUSE DFSORT         
       CLOSE ((R2))                FREES ITS STORAGE IF RUNNING AND             
*                                  JOB ABENDS SC03                              
                                                                                
       J     XIT1                                                               
*                                                                               
       LTORG                                                                    
*                                                                               
       DROP  RB,RA                                                              
*                                                                               
TOTALS_X EQU   *                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE BACKOUT AND EXTRA UPDATE RECORDS FOR THE CURRENT MAJOR KEY.   *         
*                                                                     *         
* THIS ROUTINE IS ONLY EXECUTED IN UPDATE MODE.                       *         
*                                                                     *         
* R3 = A(CURRENT UPDATE RECORD) DSECT DVREC. KEY NULL IF EOF CALL     *         
* VSAREA CONTAINS THE CURRENT VSAM RECORD.                            *         
* VSMKMAJ CONTAINS THE CURRENT MAJOR KEY.                             *         
*                                                                     *         
* -AN UPDATE RECORD WHOSE KEY IS NOT PRESENT IN THE VSAM FILE WILL BE *         
*  INSERTED INTO THE VSAM FILE. THEREFORE, AN EMPTY DELETED COPY OF   *         
*  THE NEW RECORD MUST BE WRITTEN TO THE BACKOUT FILE.                *         
* -AN UPDATE RECORD WITH THE SAME KEY AS A VSAM RECORD WILL REPLACE   *         
*  THAT RECORD AND THE ORIGINAL RECORD MUST BE WRITTEN TO THE BACKOUT *         
*  FILE.                                                              *         
* -AN UPDATE RECORD THAT IS DELETED AND NOT A PASSIVE MUST BE FROM A  *         
*  DELETED POINTER AND INDICATES A WHOLE MAJOR GROUP MUST BE DELETED. *         
*  SO WRITE DELETES FOR EVERY RECORD AND WRITE ORIGINAL RECORDS TO    *         
*  THE BACKOUT FILE.                                                  *         
*                                                                     *         
* THE EFFECT OF ALL THIS IS THAT IF THE BACKOUT FILE IS ADDED TO THE  *         
* VSAM FILE IN THE SAME WAY AS THE UPDATE FILE, IT WILL EFFECTIVELY   *         
* REVERSE THE UPDATE.                                                 *         
***********************************************************************         
MERGEMAJ NTR1  BASE=(*,MERGEMAJ_X),LABEL=*                                      
*                                                                               
UPDREC   USING OTREC,R3            R3=A(OUTPUT RECORD)                          
UPD      USING DVREC,UPDREC.OTVSREC VSAM RECORD IN OUTPUT RECORD                
         LARL  R4,VSRDW                                                         
VSMREC   USING OTREC,R4            R4=A(CURRENT VSAM RECORD)                    
VSM      USING DVREC,VSMREC.OTVSREC VSAM RECORD IN CURRENT VSAM RECORD          
*                                                                               
         OC    VSMKMAJ,VSMKMAJ     FIRST TIME?                                  
         JZ    MM20                YES, SKIP                                    
*                                                                               
         CLC   UPD.DVKEYMAJ,VSMKMAJ HAVE WE CHANGED MAJOR KEY?                  
         JE    MM30                NO, SKIP                                     
*                                                                               
MM10     DS    0H                                                               
         CLC   VSM.DVKEYMAJ,VSMKMAJ MORE FOR THIS MAJOR ON FILE?                
         JNE   MM20                NO, SKIP                                     
         TM    VSM.DVSTAT,DVSDEL   IS OLD RECORD DELETED                        
         JNZ   MM12                YES, DON'T DELETE AGAIN                      
         AP    CTRDELET,=P'1'                                                   
         CLI   VSM.DVKEYSEQ,PASSIVE# IS OLD RECORD A PASSIVE?                   
         JE    *+10                                                             
         AP    CTRDELMN,=P'1'      NO, IT'S A DELETED MINOR                     
*                                                                               
         AP    CTRDELWR,=P'1'                                                   
         MVC   DELRKEY,VSM.DVKEY   BUILD A DELETE FROM OLD VSAM REC             
         LA    R0,DELREC                                                        
         BRAS  RE,PUTOUT           WRITE DELETE TO UPDATE                       
         LR    R0,R4                                                            
         BRAS  RE,PUTBAK           WRITE ORIGINAL RECORD TO BACKOUT             
*                                                                               
MM12     DS    0H                                                               
         BRAS  RE,GETVNX           GET NEXT VSAM RECORD                         
         J     MM10                                                             
*                                                                               
MM20     DS    0H                                                               
         OC    UPD.DVKEYMAJ,UPD.DVKEYMAJ IS THIS THE EOF CALL                   
         JZ    MMX                 YES, ALL DONE                                
*                                                                               
         MVC   VSMKMAJ,UPD.DVKEYMAJ SET NEW MAJOR KEY                           
         IF (CLI,SPLIT,EQ,YES),AND,                                             
            (CLC,VSMKMAJ(3),EQ,=C'RTN') 'RTN' RECS. ARE HALF OF DEMVSMN         
           BRAS  RE,GETVF2              AND FORM THE 2ND HALF OF SPLIT          
         ELSE ,                                                                 
           BRAS  RE,GETVFI         GET FIRST VSAM RECORD FOR NEW MAJOR          
         ENDIF ,                                                                
*                                                                               
         CLC   VSM.DVKEYMAJ,VSMKMAJ IS THE UPDATE A NEW MAJOR?                  
         JNE   MM28                YES, SKIP                                    
*                                  MAJOR KEY ALREADY EXISTS:                    
         CLI   VSM.DVKEYSEQ,PASSIVE# IS EXISTING MAJOR A PASSIVE?               
         JNE   MM25                NO, SKIP                                     
*                                                                               
*                                  EXISTING MAJOR KEY IS A PASSIVE:             
         CLI   UPD.DVKEYSEQ,PASSIVE# SO NEW RECORD MUST BE A PASSIVE            
         JE    MM24                                                             
         AP    CTRERP2F,=P'1'      CAN'T CHANGE PASSIVE TO FILE RECORD          
         XC    VSMKMAJ,VSMKMAJ     CLEAR SO WON'T PURGE PASSIVE                 
         J     MM60                IGNORE THIS RECORD                           
*                                                                               
MM24     DS    0H                                                               
         LA    R1,CTRUPDPV         POSIT UPDATED PASSIVE                        
         TM    UPD.DVSTAT,DVSEXTP  IS EXTENDED FLAG ON?                         
         JZ    *+8                                                              
         LA    R1,CTRUPDPX         POSIT UPDATED EXTENDED PASSIVE               
         TM    UPD.DVSTAT,DVSDEL   TEST UPDATE RECORD IS DELETED                
         JZ    *+8                                                              
         LA    R1,CTRDELPV         YES, SO DELETING PASSIVE                     
         TM    UPD.DVSTAT,DVSEXTP+DVSDEL TEST DELETED AND EXTENDED              
         JNO   *+8                                                              
         LA    R1,CTRDELPX         YES, SO DELETING EXTENDED PASSIVE            
         AP    0(L'CTRS,R1),=P'1'                                               
         J     MM30                                                             
*                                  EXISTING MAJOR KEY FILE RECORD:              
MM25     DS    0H                                                               
         CLI   UPD.DVKEYSEQ,PASSIVE# IS NEW RECORD A DIRECTORY RECORD?          
         JE    MM26                YES, SO NOT REALLY A PASSIVE                 
         TM    UPD.DVSTAT,DVSDEL   TEST UPDATE RECORD IS DELETED                
         JO    *+2                 SHOULD BE DROPPED AT MAIND22                 
         AP    CTRUPDMJ,=P'1'                                                   
         J     MM30                                                             
*                                                                               
MM26     DS    0H                                                               
         TM    UPD.DVSTAT,DVSDEL   TEST UPDATE RECORD IS DELETED                
         JO    MM27                                                             
         AP    CTRERF2P,=P'1'      CAN'T CHANGE FILE RECORD TO PASSIVE          
         XC    VSMKMAJ,VSMKMAJ     CLEAR SO WON'T PURGE MAJOR                   
         J     MM60                IGNORE THIS RECORD                           
*                                                                               
MM27     DS    0H                                                               
         AP    CTRPTRD,=P'1'       IT'S A DELETED POINTER                       
         AP    CTRDELMJ,=P'1'      DELETING A WHOLE MAJOR.                      
         J     MM60                DROP THIS RECORD  AND NEXT CALL WILL         
*                                  DELETE WHOLE GROUP.                          
*                                                                               
*                                  MAJOR KEY DOES NOT YET EXIST:                
MM28     DS    0H                                                               
         TM    UPD.DVSTAT,DVSDEL   TEST UPDATE RECORD IS DELETED                
         JZ    *+14                                                             
         AP    CTRERDNF,=P'1'      NO MAJOR KEY TO DELETE                       
         J     MM60                IGNORE THIS RECORD                           
*                                                                               
         LA    R1,CTRNEWPV         POSIT NEW PASSIVE                            
         TM    UPD.DVSTAT,DVSEXTP  IS EXTENDED FLAG ON?                         
         JZ    *+8                                                              
         LA    R1,CTRNEWPX         POSIT NEW EXTENDED PASSIVE                   
         CLI   UPD.DVKEYSEQ,PASSIVE# IS NEW RECORD A PASSIVE?                   
         JE    *+8                                                              
         LA    R1,CTRNEWMJ         IT'S A NEW MAJOR                             
         AP    0(L'CTRS,R1),=P'1'                                               
*                                                                               
MM30     DS    0H                                                               
         CLC   UPD.DVKEY,VSM.DVKEY COMPARE NEW RECORD WITH FILE                 
         JH    MM40                UPDATE HIGHER THAN FILE                      
         JL    MM50                UPDATE LOWER THAN FILE                       
*                                  UPDATE EQUAL TO FILE                         
         AP    CTRREPL,=P'1'       COUNT REPLACEMENT RECORDS                    
         CLI   UPD.DVKEYSEQ,PASSIVE# IS NEW RECORD A PASSIVE?                   
         JE    *+10                                                             
         AP    CTRUPDMN,=P'1'      NO, IT'S A REPLACED MINOR                    
*                                                                               
* WE NOW COMPARE THE OLD (VSAM) VS NEW (IFILE) RECORD, TO SEE IF THEY           
* DIFFER. IF THEY MATCH BYTE-FOR-BYTE, THEN THERE'S NO REASON TO WRITE          
* THE NEW VERSION OF THE RECORD.                                                
*                                                                               
* HOWEVER: MANY OF OUR DEMOS CONVERSION PROGRAMS STORE THE CONVERSION           
* EXECUTION *DATE* INTO AN ELEMENT WITHIN THE GENERATED RECORD (E.G.,           
* SEE DSECT "MARELEM" IN DEDEMFILE). THERE REALLY ISN'T ANY GOOD REASON         
* TO OVERWRITE A VSAM RECORD IF THE *ONLY* DIFFERENCE BETWEEN THE OLD           
* AND NEW VERSIONS IS THE CONVERSION RUN DATE.                                  
*                                                                               
* UNFORTUNATELY, NOT ALL OF OUR CONVERSIONS ARE CONSISTENT REGARDING            
* THE ELEMENT IN WHICH THAT DATE IS STORED (IF IT'S STORED AT ALL).             
* THEREFORE, WE HAVE TO BE VERY SELECTIVE ABOUT WHICH RECORD TYPE(S) WE         
* USE FOR THIS APPROACH.                                                        
*                                                                               
* FOR THIS "SMART" COMPARE, WE SAVE BOTH THE "OLD" AND "NEW" CONVERSION         
* RUN DATES, AND WE *CLEAR* THOSE DATES WITHIN BOTH RECORD BUFFERS              
* PRIOR TO THE CLCL. IF THE CLCL INDICATES THAT THE RECORDS DIFFER, WE          
* RESTORE THE SAVED DATES INTO THEIR RESPECTIVE BUFFERS BEFORE                  
* CONTINUING, AND WE WRITE THE UPDATED RECORD. BUT IF THE "NORMALIZED"          
* RECORDS *MATCH*, THEN WE DO *NOT* WRITE THE VSAM RECORD. INSTEAD, WE          
* SIMPLY INCREMENT A DEDICATED COUNTER INDICATING THE NUMBER OF RECORDS         
* THAT WE DID *NOT* WRITE FOR THIS SPECIFIC REASON (I.E., THAT THE ONLY         
* DIFFERENCE BETWEEN THE RECORDS IS THE CONVERSION RUN DATE).                   
*                                                                               
         SR  R2,R2                 R2 = CONVERSION DATE FROM IFILE              
         SR  R5,R5                 R5 = CONVERSION DATE FROM VSAM FILE          
         IF (CLC,=C'RTN',EQ,UPD.DVKEYMAJ),OR,    LOCAL MONTHLIES                
            (CLC,=C'RON',EQ,UPD.DVKEYMAJ),ANDIF, LOCAL DAILIES                  
            (CLI,UPD.DVFRSTEL,EQ,MARCODEQ),AND, CONFIRM X'01' ELEMS.            
            (CLI,VSM.DVFRSTEL,EQ,MARCODEQ)                                      
           USING MARELEM,R1                                                     
           LA    R1,UPD.DVFRSTEL   R1 = A(1ST ELEM. IN REPLACEMENT)             
           ICM   R2,3,MARDATE      SAVE THE CONVERSION DATE                     
           XC    MARDATE,MARDATE   CLEAR DATE IN I/O AREA                       
           LA    R1,VSM.DVFRSTEL   R1 = A(1ST ELEM. IN ORIGINAL)                
           ICM   R5,3,MARDATE      SAVE THE CONVERSION DATE                     
           XC    MARDATE,MARDATE   CLEAR DATE IN I/O AREA                       
           DROP  R1                                                             
         ENDIF ,                                                                
*                                                                               
         LR    R0,R3               COMPARE REPLACEMENT                          
         LH    R1,0(R3)                                                         
         LR    RE,R4               WITH ORIGINAL                                
         LH    RF,0(R4)                                                         
         CLCL  R0,RE                                                            
         JNE   MM32                THE RECORDS DIFFER                           
*                                                                               
*                                  RECORDS MATCH: NO NEED TO OVERWRITE          
         IF  (CR,R2,EQ,R5)         IF THE CONVERSION DATES MATCH:               
           AP  CTRAOF,=P'1'         INCREMENT "ALREADY ON FILE" COUNTER         
         ELSE ,                    O/W:                                         
           AP  CTRDTDF,=P'1'        INCREMENT "MATCH EXCEPT FOR DATE"           
         ENDIF ,                                                                
         XC    0(4,R3),0(R3)       CLEAR RDW SO DROPPED UPON RETURN             
         J     MM36                                                             
*                                                                               
MM32     DS    0H                                                               
         USING MARELEM,R1                                                       
         IF (CHI,R2,NE,0)          IF WE SAVED "NEW" DATE, RESTORE IT           
           LA    R1,UPD.DVFRSTEL     R1 = A(1ST ELEM. IN REPLACEMENT)           
           STCM  R2,3,MARDATE        RESTORE THE CONVERSION DATE                
         ENDIF ,                                                                
         IF (CHI,R5,NE,0)          IF WE SAVED "OLD" DATE, RESTORE IT           
           LA    R1,VSM.DVFRSTEL     R1 = A(1ST ELEM. IN ORIGINAL)              
           STCM  R5,3,MARDATE        RESTORE THE CONVERSION DATE                
         ENDIF ,                                                                
         DROP  R1                                                               
*                                                                               
         TM    VSM.DVSTAT,DVSDEL   IS OLD RECORD DELETED                        
         JZ    MM34                                                             
         TM    UPD.DVSTAT,DVSDEL   YES, IS NEW RECORD DELETED                   
         JO    *+14                                                             
         AP    CTRDOF,=P'1'        NO, COUNT ALREADY ON FILE BUT DELTD          
         J     MM34                                                             
*                                                                               
         AP    CTRDDF,=P'1'        BOTH DELETED SO NO POINT IN OUTPUT           
         XC    0(4,R3),0(R3)       CLEAR RDW SO DROPPED UPON RETURN             
         J     MM36                                                             
*                                                                               
MM34     DS    0H                                                               
         LR    R0,R4                                                            
         BRAS  RE,PUTBAK           WRITE ORIGINAL RECORD TO BACKOUT             
*                                                                               
         TM    UPD.DVSTAT,DVSDEL   IS NEW RECORD DELETED                        
         JZ    MM36                                                             
         AP    CTRREPLD,=P'1'      YES, COUNT AND                               
         AP    CTRDELWR,=P'1'      REPLACE WITH A DELETE RECORD                 
         MVC   DELRKEY,UPD.DVKEY   (SHOULD BE A PASSIVE)                        
         MVC   0(DELRMLNQ,R3),DELREC                                            
*                                                                               
MM36     DS    0H                                                               
         BRAS  RE,GETVNX           GET NEXT VSAM RECORD                         
         J     MMX                                                              
*                                  FILE IS LOW: NEEDS TO BE DELETED             
MM40     DS    0H                                                               
         TM    VSM.DVSTAT,DVSDEL   IS OLD RECORD DELETED                        
         JNZ   MM42                YES, DON'T DELETE AGAIN                      
*                                                                               
         AP    CTRDELET,=P'1'                                                   
         CLI   VSM.DVKEYSEQ,PASSIVE# IS OLD RECORD A PASSIVE?                   
         JE    *+10                                                             
         AP    CTRDELMN,=P'1'      NO, IT'S A DELETED MINOR                     
         AP    CTRDELWR,=P'1'                                                   
         MVC   DELRKEY,VSM.DVKEY   BUILD A DELETE FROM VSAM REC                 
         LA    R0,DELREC                                                        
         BRAS  RE,PUTOUT           WRITE DELETE TO UPDATE                       
         LR    R0,R4                                                            
         BRAS  RE,PUTBAK           WRITE ORIGINAL RECORD TO BACKOUT             
*                                                                               
MM42     DS    0H                                                               
         BRAS  RE,GETVNX           GET NEXT VSAM RECORD                         
         J     MM30                RETRY COMPARE                                
*                                                                               
*                                  FILE IS HIGH: UPDATE IS AN INSERT            
MM50     DS    0H                                                               
         AP    CTRINSRT,=P'1'      COUNT INSERTS                                
         CLI   UPD.DVKEYSEQ,PASSIVE# IS OLD RECORD A PASSIVE?                   
         JE    *+10                                                             
         AP    CTRNEWMN,=P'1'      NO, IT'S A NEW MINOR                         
         MVC   DELRKEY,UPD.DVKEY   BUILD A DELETE FROM UPDATE RECORD            
         LA    R0,DELREC                                                        
         BRAS  RE,PUTBAK           WRITE DELETE TO BACKOUT                      
         J     MMX                                                              
*                                                                               
MM60     DS    0H                                                               
         XC    0(4,R3),0(R3)       CLEAR RDW SO DROPPED UPON RETURN             
*                                                                               
MMX      DS    0H                                                               
         J     XIT1                DROP THIS RECORD                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  UPDREC,VSMREC                                                    
         DROP  RB                                                               
*                                                                               
MERGEMAJ_X EQU *                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
* COMPARE INPUT FILE WITH VSAM FILE.                                  *         
*                                                                     *         
* R3 = A(CURRENT INPUT RECORD) DSECT DVREC. KEY NULL IF EOF CALL      *         
* VSAREA CONTAINS THE CURRENT VSAM RECORD.                            *         
*                                                                     *         
* WE COUNT THE DIFFERENCES, IGNORING DELETED RECORDS                  *         
* THE FIRST N OF EACH TYPE OF DIFFERENCE IS WRITTEN TO A SEQUENTIAL   *         
* FILES. N HAS A DEFAULT VALUE. CAN BE OVERRIDDEN WITH MAXCOMP= CARD  *         
***********************************************************************         
COMPARE_FILES NTR1 BASE=(*,COMPARE_FILES_X),LABEL=*                             
*                                                                               
OUTREC   USING OTREC,R3            R3=A(OUTPUT RECORD)                          
OUT      USING DVREC,OUTREC.OTVSREC VSAM RECORD IN OUTPUT RECORD                
         LARL  R4,VSRDW                                                         
VSMREC   USING OTREC,R4            R4=A(CURRENT VSAM RECORD)                    
VSM      USING DVREC,VSMREC.OTVSREC VSAM RECORD IN CURRENT VSAM RECORD          
*                                                                               
         OC    VSMREC.OTRDW,VSMREC.OTRDW HAVE WE READ FIRST VSAM REC            
         JNZ   CP10                YES, SKIP                                    
         XC    VSMKEY,VSMKEY       CLEAR KEY                                    
         XC    SPLITKEY,SPLITKEY   CLEAR KEY SAVE AREA                          
         BRAS  RE,GETVFI           GET FIRST VSAM RECORD (+SET VSMFILE)         
*                                                                               
CP10     DS    0H                                                               
         TM    VSM.DVSTAT,DVSDEL   TEST THIS RECORD DELETED?                    
         JZ    CP12                NO, SKIP                                     
         AP    CTRCMPDL,=P'1'      COUNT DELETED VSAM RECORDS IGNORED           
         BRAS  R8,CPNX             GET NEXT VSAM RECORD                         
         J     CP10                                                             
*                                                                               
CP12     DS    0H                                                               
         BRAS  R8,CPTPRGE          WOULD DELDPRGE PURGE THIS RECORD?            
         JNE   CP20                NO, SKIP                                     
         AP    CTRCMPPL,=P'1'      COUNT VSAM RECORDS IGNORED AS PURGED         
         BRAS  R8,CPNX             GET NEXT VSAM RECORD                         
         J     CP10                                                             
*                                                                               
CP20     DS    0H                                                               
         OC    OUT.DVKEYMAJ,OUT.DVKEYMAJ IS THIS THE EOF CALL                   
         JZ    CP22                YES, SEE IF ANY MORE VSAM RECORDS            
         CLI   OUT.DVKEY,X'FF'     IS THIS TRAILER                              
         JNE   CP30                NO, SKIP                                     
*                                                                               
CP22     DS    0H                                                               
         CLI   VSM.DVKEY,X'FF'     IS THIS END OF VSAM FILE                     
         JE    CPX                 YES, EXIT                                    
         J     CP40                GO COUNT UNMATCHED VSAM                      
*                                                                               
CP30     DS    0H                                                               
         CLC   OUT.DVKEY,VSM.DVKEY COMPARE KEYS                                 
         JH    CP40                OUT HIGH, GO COUNT UNMATCHED VSAM            
         JE    CP50                EQUAL, SEE IF REALLY EQUAL                   
         AP    CTRCMPUN,=P'1'      COUNT UNMATCHED FILE                         
         BRAS  RE,PUTOLD           COPY TO OLD FILE                             
         J     CPX                                                              
*                                                                               
CP40     DS    0H                                                               
         AP    CTRCMPUV,=P'1'      COUNT UNMATCHED VSAM                         
         BRAS  RE,PUTNEW           COPY TO NEW FILE                             
         BRAS  R8,CPNX             GET NEXT VSAM RECORD                         
         J     CP10                RETRY COMPARE                                
*                                                                               
CP50     DS    0H                                                               
         LR    R0,R3               COMPARE NEW RECORD                           
         LH    R1,0(R3)                                                         
         LR    RE,R4               WITH VSAM                                    
         LH    RF,0(R4)                                                         
         CLCL  R0,RE                                                            
         JNE   *+14                                                             
         AP    CTRCMPEQ,=P'1'      COUNT MATCHING KEYS AND EQUAL                
         J     CP58                                                             
*                                                                               
         AP    CTRCMPNE,=P'1'      COUNT MATCHING KEYS BUT NOT EQUAL            
         BRAS  RE,PUTDIF           COPY BOTH TO DIF FILE                        
*                                                                               
CP58     DS    0H                                                               
         BRAS  R8,CPNX             GET NEXT VSAM RECORD                         
*                                                                               
CPX      DS    0H                                                               
         J     XIT1                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*        GET NEXT VSAM RECORD, RETURN VIA R8                                    
*                                                                               
CPNX     DS    0H                                                               
         BRAS  RE,GETVNX           GET NEXT VSAM RECORD (FRM LAST FILE)         
         CLI   SPLIT,YES                                                        
         JNE   CPNXX               EXIT IF NOT DEALING WITH SPLIT               
         CLC   4(3,R4),=C'RTN'     HAVE WE GONE PAST RTN RECORDS?               
         JNH   CPNXX               NOT YET. RETURN THIS RECORD                  
         CLI   SPLITKEY,0          IF SPLITKEY STILL ZERO, WE HAVEN'T           
         JNE   CPNX2               STARTED ON THE RTN RECORDS SO SAVE           
         MVC   SPLITKEY,4(R4)      KEY OF THIS RECORD WHICH CAME FROM           
         XC    VSMKEY,VSMKEY       THE MAIN FILE, GET THE FIRST RECORD          
         BRAS  RE,GETVF2           FROM THE RTN SPLIT FILE                      
         J     CPNXX               AND RETURN IT                                
*                                                                               
CPNX2    DS    0H                                                               
         CLI   SPLITKEY,X'FF'      WE'VE FINISHED WITH THE RTN RECORDS          
         JE    CPNXX               IF FF, WE ARE BACK ON THE MAIN FILE          
*                                                                               
         MVC   VSMKEY,SPLITKEY     ELSE RESET TO THE SAVED KEY AND GET          
         BRAS  RE,GETVFI           FIRST MAIN FILE RECORD AFTER SPLIT           
         MVI   SPLITKEY,X'FF'      INDICATE BACK ON THE MAIN FILE               
*                                                                               
CPNXX    DS    0H                                                               
         BR    R8                                                               
*                                                                               
*        TEST IF VSAM RECORD WOULD HAVE BEEN PURGED BY DELDPRGE AND             
*        SET CC EQUAL IF IT WOULD.                                              
*        NOTE DELDPRGE REQUIRES A DANDX DIRECTORY RECORD                        
*                                                                               
CPTPRGE  DS    0H                                                               
         CLI   DTF#,1              WAS FILE SPECIFIED                           
         BLR   R8                  NO, CC NEQ TO KEEP RECORD                    
         IF (CLC,PRGLSTMJ,NE,VSM.DVKEY)  IF MAJOR KEY HAS CHANGED:              
           MVC   PRGARKEY,VSM.DVKEY  COPY MAJOR KEY                             
           MVC   PRGLSTMJ,PRGARKEY   REMEMBER LAST MAJOR KEY WE CHECKED         
           MVC   PRGARSTA,VSM.DVSTAT COPY STATUS                                
           XC    PRGARDA,PRGARDA     CLEAR D/A                                  
           MVI   PURGEFLG,0          CLEAR PURGE FLAG                           
           GOTOR VLDPURGE,PLPURGE    CALL DELDPRGE                              
         ENDIF ,                                                                
         CLI   PURGEFLG,X'FF'      RECORD TO BE PURGED?                         
         BR    R8                  CC EQ IF SO, ELSE CC NE                      
*                                                                               
         DROP  OUTREC,VSMREC                                                    
         EJECT                                                                  
***********************************************************************         
* VSAM READ ROUTINES. KEY IN VSMKEY, VSAM RECORD RETURNED AT 0(R4)    *         
***********************************************************************         
         USING IFGRPL,R2                                                        
VSMREC   USING OTREC,R4            R4=A(CURRENT VSAM RECORD)                    
VSM      USING DVREC,VSMREC.OTVSREC VSAM RECORD IN CURRENT VSAM RECORD          
*                                                                               
GETVFI   NTR1  ,                   GET FIRST RECORD FOR MAJOR KEY               
         MVI   VFILEID,MAINFIL     FROM MAIN FILE                               
         LARL  R2,DEMRPL                                                        
         J     GETVF                                                            
*                                                                               
GETVF2   NTR1  ,                   GET FIRST RECORD FOR MAJOR KEY               
         MVI   VFILEID,SPLITFIL    FROM SPLIT FILE                              
         LARL  R2,DEMRP2                                                        
*                                                                               
GETVF    DS    0H                                                               
         NI    RPLOPT1,255-RPLSEQ                                               
         OI    RPLOPT1,RPLDIR+RPLKGE                                            
         J     GETVSM                                                           
*                                                                               
*                                                                               
GETVNX   NTR1  ,                   GET NEXT VSAM RECORD                         
*                                                                               
         LARL  R2,DEMRPL                                                        
         CLI   VFILEID,SPLITFIL                                                 
         JNE   *+10                FROM CURRENT FILE                            
         LARL  R2,DEMRP2                                                        
         NI    RPLOPT1,255-(RPLDIR+RPLKGE)                                      
         OI    RPLOPT1,RPLSEQ      SET OPTCD=SEQ                                
*                                                                               
GETVSM   DS    0H                                                               
         GET   RPL=(R2)            READ VSAM RECORD                             
         LTR   RF,RF                                                            
         JNZ   *+2                 VSAM ERROR BEFORE WAIT                       
         CHECK RPL=(R2)                                                         
         OC    RPLFDBK,RPLFDBK                                                  
         JNZ   *+2                 VSAM ERROR                                   
         L     R1,RPLRLEN          RETURNED RECORD LENGTH                       
         LA    R1,4(,R1)           ADD 4 FOR RDW LENGTH                         
         SLL   R1,16               AND SHIFT TO CORRECT POSITION                
         ST    R1,VSMREC.OTRDW                                                  
*                                                                               
         J     XIT1                                                             
*                                                                               
         DROP  VSMREC,R2                                                        
         EJECT                                                                  
***********************************************************************         
* PUT RECORD AT 0(R4) TO NEW FILE (COMPARE MODE)                      *         
***********************************************************************         
PUTNEW   DS    0H                                                               
         ST    RE,SAVERE                                                        
         CP    CTRNEWF,MAXCOMP                                                  
         JNL   PUTNEWX                                                          
*                                                                               
         AP    CTRNEWF,=P'1'       COUNT NEWFILE RECORDS                        
         LARL  R1,NEWFILE                                                       
         PUT   (1),(4)             WRITE RECORD AT (R4) TO NEW FILE             
*                                                                               
PUTNEWX  DS    0H                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
***********************************************************************         
* PUT RECORDS AT 0(R3) AND AT 0(R4) TO DIF FILE (COMPARE MODE)        *         
***********************************************************************         
PUTDIF   DS    0H                                                               
         ST    RE,SAVERE                                                        
         CP    CTRDIFF,MAXCOMP                                                  
         JNL   PUTDIFX                                                          
*                                                                               
         AP    CTRDIFF,=P'1'       COUNT DIFFILE RECORDS                        
         LARL  R1,DIFFILE                                                       
         PUT   (1),(3)             WRITE RECORD AT (R3) TO DIF FILE             
         LARL  R1,DIFFILE                                                       
         PUT   (1),(4)             WRITE RECORD AT (R4) TO DIF FILE             
*                                                                               
PUTDIFX  DS    0H                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
***********************************************************************         
* PUT RECORD AT 0(R3) TO OLD FILE (COMPARE MODE)                      *         
***********************************************************************         
PUTOLD   DS    0H                                                               
         ST    RE,SAVERE                                                        
         CP    CTROLDF,MAXCOMP                                                  
         JNL   PUTOLDX                                                          
*                                                                               
         AP    CTROLDF,=P'1'       COUNT OLDFILE RECORDS                        
         LARL  R1,OLDFILE                                                       
         PUT   (1),(3)             WRITE RECORD AT (R3) TO OLD FILE             
*                                                                               
PUTOLDX  DS    0H                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
COMPARE_FILES_X EQU *                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS                                            *         
***********************************************************************         
VALPARMS NTR1  BASE=(*,VALPARMS_X),LABEL=*                                      
*                                                                               
         XC    RUNVALS(RUNVALL),RUNVALS                                         
         MVC   MID1,SPACES                                                      
         MVC   MID1(14),=C'CONTROL CARDS:'                                      
         MVC   MID2,SPACES                                                      
         MVC   MID2(14),=C'--------------'                                      
*                                                                               
VALPAR02 DS    0H                                                               
         GOTOR VCARDS,DMCB,CARD,=C'RE10' (1=TREAT OPEN ERROR AS EOF)            
         MVC   P(L'CARD),CARD                                                   
         GOTOR VPRINTER                                                         
         CLI   CARD,C'*'           IGNORE COMMENTS                              
         JE    VALPAR02                                                         
         LA    R1,PARAMS           R1=A(PARAMETER TABLE)                        
         USING PARAMSD,R1                                                       
         SR    RE,RE               PREPARE FOR ICM                              
*                                                                               
VALPAR04 DS    0H                                                               
         ICM   RE,1,PARAMLEN       TEST FOR END OF TABLE                        
         JNZ   *+12                                                             
         BRAS  RE,VALPARIV         YES - NOT A VALID CONTROL CARD               
         J     VALPAR02                                                         
         EX    RE,*+8              MATCH CARD DATA TO TABLE ENTRY               
         JE    VALPAR06                                                         
         CLC   CARD(0),PARAMTXT                                                 
         AHI   R1,PARAMSLQ         BUMP TO NEXT TABLE ENTRY                     
         J     VALPAR04                                                         
*                                                                               
VALPAR06 DS    0H                                                               
         SR    RF,RF               PROCESS PARAMETER CARD                       
         ICM   RF,7,PARAMRTN       RF=A(PROCESS/VALIDATION ROUTINE)             
         DROP  R1                                                               
         LA    R1,CARD+1(RE)       R1=A(DATA VALUE)                             
         GOTOR (RF),(R1)           CALL PROCESS/VALIDATE ROUTINE                
         J     VALPAR02                                                         
*                                                                               
* ALL PARAMETER CARDS HAVE NOW BEEN VALIDATED: CHECK FOR CONSISTENCY            
VALPAR_CHECK_CONSISTENCY DS 0H                                                  
         CLI   MODE,0              MODE INPUT?                                  
         JNE   VALPAR20            YES                                          
         CLI   E35EXIT,YES         TEST RUNNING AS A DFSORT E35 EXIT            
         JNE   VALPAR12            NO, ERROR                                    
         MVI   MODE,COPY           DEFAULT COPY MODE                            
         J     VALPAR20                                                         
*                                                                               
VALPAR12 DS    0H                                                               
         LARL  R1,LTPMCM                                                        
         MVC   P+4(L'LTPMCM),0(R1)                                              
         BRAS  RE,VALPARER                                                      
         J     VALPARX                                                          
*                                                                               
VALPAR20 DS    0H                                                               
         CLI   MODE,COMPARE        COMPARE MODE?                                
         JNE   VALPAR22            NO                                           
         CLI   DTF#,0              FILE INPUT?                                  
         JNE   VALPAR22            YES                                          
         LARL  R1,LTPMRF                                                        
         MVC   P+4(L'LTPMRF),0(R1)                                              
         BRAS  RE,VALPARER                                                      
         J     VALPARX                                                          
*                                                                               
VALPAR22 DS    0H                                                               
         CLI   MAXCOMPF,0          MAXCOMP INPUT?                               
         JE    VALPAR24            NO                                           
         CLI   MODE,COMPARE        COMPARE MODE?                                
         JE    VALPAR24            YES                                          
         LARL  R1,LTPMVC                                                        
         MVC   P+4(L'LTPMVC),0(R1)                                              
         BRAS  RE,VALPARER                                                      
         J     VALPARX                                                          
*                                                                               
VALPAR24 DS    0H                                                               
*                                                                               
         IF (CLI,PURGE,NE,0),AND,                                               
            (CLI,MODE,EQ,UPDATE)                                                
           LARL  R1,LTPPMU         PURGE= INVALID WHEN MODE EQ UPDATE           
           MVC   P+4(L'LTPPMU),0(R1)                                            
           BRAS  RE,VALPARER                                                    
           J     VALPARX                                                        
         ENDIF ,                                                                
*                                                                               
         CLI   INPARM,0            INRECFORM= INPUT?                            
         JNE   *+8                 YES                                          
         MVI   INPARM,DANDX        DEFAULT INRECFORM=DANDX                      
*                                                                               
         CLI   COUNTFLG,0          COUNT= INPUT?                                
         JE    VALPAR26            NO                                           
         CLI   MODE,UPDATE         UPDATE MODE?                                 
         JNE   VALPAR26            NO                                           
         LARL  R1,LTPCIVU          YES: INVALID                                 
         MVC   P+4(L'LTPCIVU),0(R1)                                             
         BRAS  RE,VALPARER                                                      
         J     VALPARX                                                          
*                                                                               
VALPAR26 DS    0H                                                               
         CLI   SPLIT,YES           SPLIT=Y INPUT?                               
         JNE   VALPARX             NO                                           
         CLI   DTF#,DEMVSMN#       FILE=DEMVSMN INPUT?                          
         JE    VALPARX             YES                                          
         LARL  R1,LTPSDN                                                        
         MVC   P+4(L'LTPSDN),0(R1)                                              
         BRAS  RE,VALPARER                                                      
         J     VALPARX                                                          
*                                                                               
VALPARIV DS    0H                                                               
         LARL  R1,LTPICC           INVALID CONTROL CARD                         
         MVC   P+4(L'LTPICC),0(R1)                                              
         J     VALPARER                                                         
                                                                                
VALPARDP DS    0H                                                               
         LARL  R1,LTPDPC           DUPLICATE PARAMETER CARD                     
         MVC   P+4(L'LTPDPC),0(R1)                                              
                                                                                
VALPARER DS    0H                                                               
         MVC   P(3),=C'***'                                                     
         LR    R0,RE                                                            
         GOTOR VPRINTER                                                         
         LR    RE,R0                                                            
         MVI   ABEND,YES                                                        
         BR    RE                                                               
                                                                                
VALPARX  DS    0H                                                               
         CLI   ABEND,YES                                                        
         JE    VALPARAB                                                         
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         ZAP   LINE,=P'99'                                                      
         J     XIT1                                                             
*                                                                               
VALPARAB DS    0H                                                               
         GOTOR VPRINTER                                                         
         LARL  R1,LTPRAB                                                        
         MVC   P+4(L'LTPRAB),0(R1)                                              
         GOTOR VPRINTER                                                         
         ABEND 100                                                              
*                                                                               
PARAMS   DS    0H                  ** TABLE OF PARAMETER CARDS **               
         DC    AL1(04),AL3(PARMODE),CL20'MODE='                                 
         DC    AL1(05),AL3(PARINPT),CL20'INPUT='                                
         DC    AL1(09),AL3(PARINPT),CL20'INRECFORM='                            
         DC    AL1(07),AL3(PARMAXC),CL20'MAXCOMP='                              
         DC    AL1(05),AL3(PARSPLT),CL20'SPLIT='                                
         DC    AL1(08),AL3(PARTSTD),CL20'TESTDATA='                             
         DC    AL1(04),AL3(PARFILE),CL20'FILE='                                 
         DC    AL1(05),AL3(PARPRGE),CL20'PURGE='                                
         DC    AL1(05),AL3(PARCOUNT),CL20'COUNT='                               
         DC    AL1(04),AL3(PARDATE),CL20'DATE='                                 
         DC    AL1(01),AL3(VALPAR_CHECK_CONSISTENCY),CL20'/*'                   
         DC    AL1(0)              EOT                                          
*                                                                               
PARAMSD  DSECT                                                                  
PARAMLEN DS    AL1                 L'PARAMETER KEY TEXT                         
PARAMRTN DS    AL3                 A(VALIDATION ROUTINE)                        
PARAMTXT DS    CL20                PARAMETER KEY= (WITH EQUALS SIGN)            
PARAMSLQ EQU   *-PARAMLEN          L'TABLE ENTRY                                
*                                                                               
         EJECT                                                                  
DEM2VSAM CSECT                                                                  
*                                                                               
* PARAMETER VALIDATION ROUTINES R1=A(PARAMETER VALUE)                           
*                                                                               
PARMODE  DS    0H                                                               
         CLI   MODE,0              MODE=                                        
         JNE   VALPARDP                                                         
*                                                                               
         MVC   MODE,0(R1)                                                       
*                                                                               
         CLC   =C'COPY ',0(R1)     MODE=COPY                                    
         JNE   *+10                                                             
         MVI   MODE,COPY           CHANGE TO L AS C = COMPARE.                  
         BR    RE                                                               
*                                                                               
         CLC   =C'LOAD ',0(R1)     MODE=LOAD                                    
         BER   RE                                                               
         CLC   =C'COMPARE ',0(R1)  MODE=COMPARE                                 
         BER   RE                                                               
*                                                                               
         CLI   E35EXIT,YES         TEST RUNNING AS A DFSORT E35 EXIT            
         JNE   PARMODE2            NO, SKIP                                     
*                                                                               
         LARL  R1,LTPMLC                                                        
         MVC   P+4(L'LTPMLC),0(R1)                                              
         J     VALPARER            (RETURNS TO RE)                              
*                                                                               
PARMODE2 DS    0H                                                               
         CLC   =C'UPDATE ',0(R1)   MODE=UPDATE                                  
         BER   RE                                                               
*                                                                               
         LARL  R1,LTPMLCU                                                       
         MVC   P+4(L'LTPMLCU),0(R1)                                             
         J     VALPARER            (RETURNS TO RE)                              
*                                                                               
PARINPT  DS    0H                                                               
         CLI   INPARM,0            INRECFORM=                                   
         JNE   VALPARDP                                                         
         MVC   INPARM,0(R1)                                                     
         CLI   INPARM,DANDX        INRECFORM=D(ANDX)                            
         BER   RE                                                               
         CLI   INPARM,VSAM         INRECFORM=V(SAM)                             
         BER   RE                                                               
         J     VALPARIV            (RETURNS TO RE)                              
*                                                                               
PARDATE  DS    0H                                                               
         ST    RE,SAVERE                                                        
         ST    R1,DMCB             A(DATE) IS P1 FOR DATVAL                     
         GOTO1 =V(DATVAL),DMCB,,DUB  VALIDATE M/D/Y                             
         OC    0(4,R1),0(R1)       VALID DATE?                                  
         L     RE,SAVERE                                                        
         JZ    VALPARIV            NO (RETURNS TO RE)                           
         GOTO1 =V(DATCON),DMCB,DUB,(10,DATEVAL) FORCE MM/DD/YY FORMAT           
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PARCOUNT DS    0H                                                               
         CLI   COUNTFLG,0          COUNT=                                       
         JNE   VALPARDP                                                         
         CLC   =C'NO ',0(R1)                                                    
         BER   RE                                                               
         OI    COUNTFLG,COUNTYES+COUNTVSM  ASSUME COUNT=YES                     
         CLC   =C'YES ',0(R1)                                                   
         BER   RE                                                               
         OI    COUNTFLG,COUNTBKS   ASSUME COUNT=BOOKS                           
         CLC   =C'BOOKS ',0(R1)                                                 
         BER   RE                                                               
         J     VALPARIV            (RETURNS TO RE)                              
*                                                                               
PARMAXC  DS    0H                                                               
         CLI   MAXCOMPF,0          MAXCOMP=NNNNNNNNN                            
         JNE   VALPARDP                                                         
         MVI   MAXCOMPF,YES                                                     
         XR    R0,R0               DIGIT COUNTER                                
         LR    RF,R1                                                            
*                                                                               
PARMAXC2 DS    0H                                                               
         CLI   0(RF),C' '          END                                          
         JE    PARMAXC4                                                         
         CLI   0(RF),C'0'          MAKE SURE IT'S A DIGIT                       
         JL    VALPARIV            (RETURNS TO RE)                              
         CLI   0(RF),C'9'                                                       
         JH    VALPARIV            (RETURNS TO RE)                              
         AHI   R0,1                                                             
         LA    RF,1(,RF)                                                        
         J     PARMAXC2                                                         
*                                                                               
PARMAXC4 DS    0H                                                               
         LTR   RF,R0               # OF DIGITS MUST BE > 0, AND < 10            
         JZ    VALPARIV            (RETURNS TO RE)                              
         CHI   RF,9                                                             
         JH    VALPARIV            (RETURNS TO RE)                              
         BCTR  RF,0                                                             
         EXRL  RF,*+8                                                           
         BR    RE                                                               
         PACK  MAXCOMP,0(0,R1)     *EXECUTED* (WE DON'T FALL THROUGH)           
*                                                                               
PARSPLT  DS    0H                                                               
         CLI   SPLIT,0             SPLIT=                                       
         JNE   VALPARDP                                                         
         MVC   SPLIT,0(R1)                                                      
         CLI   SPLIT,YES           SPLIT=Y(ES)                                  
         BER   RE                                                               
         CLI   SPLIT,NO            SPLIT=N(O)                                   
         BER   RE                                                               
         J     VALPARIV            (RETURNS TO RE)                              
*                                                                               
PARPRGE  DS    0H                                                               
         CLI   PURGE,0             PURGE=                                       
         JNE   VALPARDP                                                         
         MVC   PURGE,0(R1)                                                      
         CLI   PURGE,YES           PURGE=Y(ES)                                  
         BER   RE                                                               
         CLI   PURGE,NO            PURGE=N(O)                                   
         BER   RE                                                               
         J     VALPARIV            (RETURNS TO RE)                              
*                                                                               
PARTSTD  DS    0H                                                               
         CLI   TESTDATA,0          TESTDATA=                                    
         JNE   VALPARDP                                                         
         MVC   TESTDATA,0(R1)                                                   
         CLI   TESTDATA,YES        TESTDATA=Y(ES)                               
         BER   RE                                                               
         CLI   TESTDATA,NO         TESTDATA=N(O)                                
         BER   RE                                                               
         J     VALPARIV            (RETURNS TO RE)                              
*                                                                               
PARFILE  DS    0H                                                               
         CLI   DTF#,0              FILE=                                        
         JNE   VALPARDP                                                         
         MVC   FILENAME,0(R1)      SAVE THE FILENAME                            
         MVI   DTF#,DEMVSMA#                                                    
         CLC   =C'DEMVSMA ',0(R1)                                               
         BER   RE                                                               
         MVI   DTF#,DEMVSMN#                                                    
         CLC   =C'DEMVSMN ',0(R1)                                               
         BER   RE                                                               
         MVI   DTF#,DEMVSMR#                                                    
         CLC   =C'DEMVSMR ',0(R1)                                               
         BER   RE                                                               
         MVI   DTF#,NTIVSM#                                                     
         CLC   =C'NTIVSM ',0(R1)                                                
         BER   RE                                                               
         MVI   DTF#,PAVVSM#                                                     
         CLC   =C'PAVVSM ',0(R1)                                                
         BER   RE                                                               
         J     VALPARIV            (RETURNS TO RE)                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
VALPARMS_X EQU *                                                                
         EJECT                                                                  
COMMON   DS    0D                                                               
***********************************************************************         
* STORAGE                                                             *         
***********************************************************************         
*              W/S REQUIREMENTS                                                 
*                                                                               
         DC    C'***W/S**'                                                      
LONG     DS    L                   QUADWORD                                     
         ORG   LONG                                                             
DUB1     DS    D                                                                
DUB2     DS    D                                                                
*                                                                               
DUB      DS    D                   (NOTE: USED BY EDIT MACRO)                   
DMCB     DS    6F                                                               
SAVERE   DS    F                                                                
*                                                                               
PL16     DS    0PL16                                                            
PL16HIGH DS    PL8                                                              
PL16LOW  DS    PL8                                                              
*                                                                               
CARD     DS    CL80                                                             
WORK     DS    CL17                (NOTE: USED BY EDIT MACRO)                   
*                                                                               
RETCODE  DC    H'0'                MVS RETURN CODE                              
*                                                                               
VCPRINT  DC    V(CPRINT)                                                        
VPRINTER DC    V(PRINTER)                                                       
VPRNTER  DC    V(PRNTER)           PRINT DCB WITHIN DDPRINT                     
VCARDS   DC    V(CARDS)                                                         
VLDPURGE DC    V(LDPURGE)                                                       
VLDCOUNT DC    V(LDCOUNT)                                                       
*                                                                               
PLPURGE  DS    0A                                                               
PURGEFLG DS    X                   MUST REMAIN INTACT BETWEEN CALLS!            
         DC    AL3(PRGAREA)                                                     
         DC    A(DTF#)                                                          
PLCOUNT  DC    A(CNTAREA),A(0),A(DTF#),A(COUNTFLG)                              
*                                                                               
DEMTABS  DC    A(0)                V(DEMTABS)                                   
DATEVAL  DC    CL8'????????'       MM/DD/YY (DEFAULT TO TODAY'S DATE)           
*                                                                               
DTF#     DC    X'00'               I/S INTERNAL FILE NUMBER                     
DEMVSMA# EQU   X'2F'                                                            
DEMVSMN# EQU   X'2D'                                                            
DEMVSMR# EQU   X'30'                                                            
NTIVSM#  EQU   X'38'                                                            
PAVVSM#  EQU   X'2C'                                                            
*                                                                               
FILENAME DC    CL8' '             DEMVSMA/DEMVSMN/DEMVSMR/NTIVSM/PAVVSM         
*                                                                               
TESTFIL# EQU   31                  ALL TEST FILES ARE LOGICAL FILE #31          
DANDXFL# EQU   B'00111111'         DANDX FILE NUMBER STATUS BITS                
PASSIVE# EQU   X'FF'               SEQUENCE # X'FF' = PASSIVE KEY               
*                                                                               
PRGLSTMJ DS    XL(L'DVKEYMAJ)'00'  LAST MAJOR KEY MARKED FOR PURGING            
         DS    0D                                                               
         DC    C'*PRGAREA'                                                      
PRGAREA  DS    0C                  USED FOR DELDPRGE (DIR REC)                  
PRGARKEY DS    CL(L'DVKEYMAJ)      KEY (= MAJOR KEY)                            
PRGARSTA DS    CL(L'DVSTAT)        STATUS                                       
PRGARDA  DS    XL4                 DISK ADDRESS (SET TO NULL)                   
*                                                                               
CNTLSTMJ DS    XL(L'DVKEYMAJ)'00'  LAST "COUNTED" MAJOR KEY                     
         DS    0D                                                               
         DC    C'*CNTAREA'                                                      
CNTAREA  DS    0C                  USED FOR DELDDCNT (DIR REC)                  
CNTARKEY DS    CL(L'DVKEYMAJ)      KEY (= MAJOR KEY)                            
CNTARSTA DS    CL(L'DVSTAT)        STATUS                                       
CNTARDA  DC    XL4'00'             DISK ADDRESS (SET TO NULL)                   
*                                                                               
         DS    0D                                                               
         DC    C'*VSMKEY*'                                                      
VSMKEY   DS    0XL21               KEY FOR VSAM READS                           
VSMKMAJ  DC    XL18'00'            MAJOR KEY FOR MERGE ROUTINE                  
         DC    XL3'00'             PLUS REST OF VSAM KEY                        
*                                                                               
SPLITKEY DS    XL21                                                             
*                                                                               
*----------------------------------------------------------------------         
RUNVALS  DS    0X                  ** RUN TIME PARAMETERS **                    
*                                                                               
MODE     DS    C                   L=COPY/LOAD,C=COMPARE,U=UPDATE               
COPY     EQU   C'L'                COPY MODE                                    
COMPARE  EQU   C'C'                COMPARE MODE                                 
UPDATE   EQU   C'U'                UPDATE MODE                                  
*                                                                               
MAXCOMPF DS    C                   MAXCOMP= CARD WAS READ                       
SPLIT    DS    C                   Y IF SPLIT=Y ELSE N OR NULL                  
PURGE    DS    C                   Y IF PURGE=Y ELSE N OR NULL                  
TESTDATA DS    C                   Y IF TESTDATA=Y ELSE N OR NULL               
*                                                                               
INPARM   DS    C                   V IF INRECFORM=VSAM ELSE D FOR DANDX         
VSAM     EQU   C'V'                                                             
DANDX    EQU   C'D'                                                             
*                                                                               
COUNTFLG DS    X                                                                
COUNTYES EQU   X'02'               COUNT=YES                                    
COUNTBKS EQU   X'04'               COUNT=BOOKS                                  
COUNTVSM EQU   X'10'               TELL DELDDCNT THAT WE ARE VSAM               
*                                                                               
RUNVALL  EQU   *-RUNVALS                                                        
*----------------------------------------------------------------------         
*                                                                               
ABEND    DS    C                   (FORCE TYPE TO "C" FOR IDF)                  
         ORG   ABEND                                                            
         DC    AL1(NO)                                                          
*                                                                               
VFILEID  DC    C' '                RECORD SOURCE                                
MAINFIL  EQU   C'M'                                                             
SPLITFIL EQU   C'S'                                                             
*                                                                               
E35EXIT  DS    C                   (FORCE TYPE TO "C" FOR IDF)                  
         ORG   E35EXIT                                                          
         DC    AL1(NO)             Y IF CALLED AS EXIT FROM DFSORT              
E35RC    DC    H'0'                RETURN CODE FOR DFSORT IF EXIT               
*                                                                               
*                                  ED PATTERN: ###,###,###,###,###              
ED_PATTERN1 DC X'402020206B2020206B2020206B2020206B202120'                      
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
         EJECT                                                                  
         DS    0D                                                               
         DC    C'COUNTERS'                                                      
CTRS     DS    0PL8                                                             
*        ALL MODES:                                                             
CTRIN    DC    PL8'0'              ALL RECORDS READ                             
CTRINFMS DC    PL8'0'              ALL RECORDS READ (BY FILE/MEDIA/SRC)         
CTRFMSTO DC    PL8'0'              RUNNING F/M/S TOTAL                          
*        ALL MODES FILE RECORDS (I.E. NOT DIRECTORY RECORDS):                   
CTRFIL   DC    PL8'0'              FILE RECORDS READ                            
CTRFILTD DC    PL8'0'              - FILE TEST DATA RECORDS READ                
CTRFILX  DC    PL8'0'              - FILE RECORDS FLGD EXTENDED PASSIVE         
CTRFILD  DC    PL8'0'              - DELETED FILE RECORDS READ                  
CTRMAJKY DC    PL8'0'              - MAJOR KEYS FOUND (NOT PASSIVES)            
CTRMINMX DC    PL8'0'              - MAXIMUM MINOR KEYS PER MAJOR KEY           
CTRMINAV DC    PL8'0'              - AVERAGE MINOR KEYS PER MAJOR KEY           
CTRDUPKY DC    PL8'0'              - RECORDS DUPLICATING PREVIOUS KEY           
CTRDUPMX DC    PL8'0'              - MAXIMUM DUPLICATES PER KEY                 
CTRDUPAV DC    PL8'0'              - AVERAGE DUPLICATES PER KEY                 
CTRFILPL DC    PL8'0'              - RECORDS IGNORED AS S/B PEELED              
*        ALL MODES DIRECTORY RECORDS:                                           
CTRDIR   DC    PL8'0'              DIRECTORY RECORDS READ                       
CTRDIRTD DC    PL8'0'              - DIRECTORY TEST DATA RECORDS READ           
CTRPASX  DC    PL8'0'              - EXTENDED PASSIVES READ                     
CTRPASXD DC    PL8'0'              - DELETED EXTENDED PASSIVES READ             
CTRDNX   DC    PL8'0'              - OTHER DIRECTORY RECORDS READ               
CTRDNXD  DC    PL8'0'              - OTHER DELETED DIRECTORY RECORDS            
CTRDIRPL DC    PL8'0'              - DIR. RECORDS IGNORED AS S/B PEELED         
*        OUTPUT COUNTERS, COPY OR UPDATE:                                       
CTROUT   DC    PL8'0'              ALL RECORDS WRITTEN OFILE                    
CTROU2   DC    PL8'0'              ALL RECORDS WRITTEN OFIL2                    
*        OUTPUT COUNTERS UPDATE ONLY:                                           
CTRBAK   DC    PL8'0'              BKFILE RECORDS WRITTEN                       
CTRBA2   DC    PL8'0'              BKFIL2 RECORDS WRITTEN                       
*        OUTPUT COUNTERS COMPARE ONLY:                                          
CTRNEWF  DC    PL8'0'              'NEW' (VSAM) FILE RECORDS                    
CTROLDF  DC    PL8'0'              'OLD' (DANDX) FILE RECORDS                   
CTRDIFF  DC    PL8'0'              'DIFF' FILE RECORDS (DATA DIFFERS)           
*        RECORD CHECK WARNINGS AND ERRORS:                                      
CTRRCE01 DC    PL8'0'              RDW LENGTH TOO SMALL                         
CTRRCE02 DC    PL8'0'              DDS LENGTH TOO SMALL                         
CTRRCE03 DC    PL8'0'              ZERO ELEMENT LENGTH                          
CTRRCW01 DC    PL8'0'              MISSING EOR (NO NULL TERMINATOR)             
CTRRCW02 DC    PL8'0'              EOR TOO SOON                                 
*        MERGE PROCESS COUNTERS, UPDATE ONLY:                                   
CTRINSRT DC    PL8'0'              RECORDS ADDED                                
CTRREPL  DC    PL8'0'              RECORDS REPLACED                             
CTRREPLD DC    PL8'0'              RECORDS REPLACED WITH DELETED RECORD         
CTRAOF   DC    PL8'0'              RECORDS ALREADY ON FILE, IGNORED             
CTRDTDF  DC    PL8'0'              RECORDS MATCH EXCEPT FOR CONV. DATE          
CTRDDF   DC    PL8'0'              NEW AND OLD BOTH DELETED, IGNORED            
CTRDOF   DC    PL8'0'              DELETED RECORD REPLACED                      
CTRDELET DC    PL8'0'              RECORDS DELETED                              
CTRDELWR DC    PL8'0'              'DELETE' RECORDS WRITTEN                     
CTRPTRD  DC    PL8'0'              DIRECTORY RECORD ASSUMED DELETE PTR          
*        MERGE PROCESS, ERROR COUNTERS, UPDATE ONLY:                            
CTRERP2F DC    PL8'0'              DIR REPLACING FILE, IGNORED                  
CTRERF2P DC    PL8'0'              FILE REPLACING DIR, IGNORED                  
CTRERDNF DC    PL8'0'              KEY TO DELETE NOT FOUND, IGNORE              
*        MERGE PROCESS, ADDED/REPLACED/DELETED COUNTERS, UPDATE ONLY:           
CTRNEWMJ DC    PL8'0'              NEW MAJOR KEYS ADDED                         
CTRUPDMJ DC    PL8'0'              MAJOR KEYS UPDATED                           
CTRDELMJ DC    PL8'0'              MAJOR KEYS DELETED                           
CTRNEWMN DC    PL8'0'              NEW MINOR KEYS ADDED                         
CTRUPDMN DC    PL8'0'              MINOR KEYS UPDATED                           
CTRDELMN DC    PL8'0'              MINOR KEYS DELETED                           
CTRNEWPV DC    PL8'0'              NEW PASSIVES ADDED                           
CTRUPDPV DC    PL8'0'              PASSIVES UPDATED                             
CTRDELPV DC    PL8'0'              PASSIVES DELETED                             
CTRNEWPX DC    PL8'0'              NEW EXTENDED PASSIVES ADDED                  
CTRUPDPX DC    PL8'0'              EXTENDED PASSIVES UPDATED                    
CTRDELPX DC    PL8'0'              EXTENDED PASSIVES DELETED                    
*        COMPARE PROCESS COUNTERS:                                              
CTRCMPDL DC    PL8'0'              DELETED VSAM RECORDS IGNORED                 
CTRCMPPL DC    PL8'0'              VSAM RECORDS IGNORED AS S/B PEELED           
CTRCMPEQ DC    PL8'0'              MATCHED RECORDS                              
CTRCMPUN DC    PL8'0'              UNMATCHED INPUT RECORDS                      
CTRCMPUV DC    PL8'0'              UNMATCHED VSAM RECORDS                       
CTRCMPNE DC    PL8'0'              MATCHED KEY BUT DATA DIFFERENT               
*        WORK COUNTERS:                                                         
IMINORS  DC    PL8'0'              NUMBER OF MINOR KEYS THIS MAJOR              
IDUPKEY  DC    PL8'0'              NUMBER OF TIMES CURRENT KEY DUPED            
IFBYTES  DC    D'0'                INPUT FILE BYTES                             
IDBYTES  DC    D'0'                INPUT DIRECTORY BYTES                        
FMSBYTES DC    D'0'                INPUT BYTE COUNT (FOR ONE F/M/S)             
OTBYTES  DC    D'0'                OFILE BYTES                                  
OTBYTE2  DC    D'0'                OFIL2 BYTES                                  
OMAXRLEN DC    H'0'                OUTPUT MAXIMUM RECORD LENGTH                 
*                                                                               
MAXCOMP  DC    PL8'5000'                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'*THISKEY'                                                      
THISKEY  DC    XL20'00'                                                         
         ORG   THISKEY                                                          
THISKFMS DS    0CL3                FILE/MEDIA/SOURCE                            
THISKMAJ DS    XL18                                                             
THISKMIN DS    XL2                                                              
SEQNO    DC    X'00'                                                            
*                                                                               
CTRRC    DS    C                   (FORCE TYPE TO "C" FOR IDF)                  
         ORG   CTRRC                                                            
         DC    AL1(NO)             'Y' = RECCHECK FLAGGED SOMETHING             
*                                                                               
LRECCHKY DC    XL3'00'                                                          
*                                                                               
* TO DELETE A RECORD, WE OVERWRITE IT WITH A VALID RECORD CONTAINING            
*  THE SAME KEY, BUT WITH NO ELEMENTS, AND WITH THE DELETE BIT SET ON.          
*  THE RECORD IS THEREFORE IRRETRIEVABLE AFTER THE IDCAMS UPDATE RUNS.          
DELREC   DS    0X                  DELETED RECORD AS LOADED TO VSAM             
         DC    AL2(DELRMLNQ)       MVS RECORD LENGTH                            
         DC    XL2'00'                                                          
DELRKEY  DC    XL21'00'            MAJOR KEY                                    
         DC    AL2(DELRDLNQ)       DDS RECORD LENGTH                            
         DC    AL1(DVSDEL)         DA STATUS BYTE - DELETED (X'80')             
         DC    X'00'               DUMMY FIRST ELEMENT                          
DELRDLNQ EQU   *-DELRKEY                                                        
DELRMLNQ EQU   *-DELREC                                                         
*                                                                               
         DS    0A                                                               
DSNXTRCT DC    X'87',AL3(0)        DCBRECFM,DCBEXLSA                            
*                                                                               
         EJECT                                                                  
*              DCBS, ETC.                                                       
*                                                                               
* MODE=UPDATE INPUT FILE:                                                       
IFILE    DCB   DDNAME=IFILE,DSORG=PS,MACRF=(GL),RECFM=VB,EODAD=EOFIN            
*                                                                               
* MODE=UPDATE OUTPUT FILE (FOR INPUT TO IDCAMS REPRO WITH REPLACE):             
OFILE    DCB   DDNAME=OFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192             
*                                                                               
* FOR SPLIT FILE:                                                               
OFIL2    DCB   DDNAME=OFIL2,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192             
*                                                                               
* BACKOUT FILE (TO REVERSE AN UPDATE):                                          
BKFILE   DCB   DDNAME=BKFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192            
*                                                                               
* FOR SPLIT FILE:                                                               
BKFIL2   DCB   DDNAME=BKFIL2,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192            
*                                                                               
* USED WHEN MODE=COMPARE:                                                       
NEWFILE  DCB   DDNAME=NEWFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192           
OLDFILE  DCB   DDNAME=OLDFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192           
DIFFILE  DCB   DDNAME=DIFFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192           
*                                                                               
* USED WHEN MODE=COMPARE OR MODE=UPDATE:                                        
DEMACB   ACB   AM=VSAM,DDNAME=DEMVSM,MACRF=(KEY,DIR,SKP,IN),           X        
               BUFNI=10,BUFND=16,RMODE31=ALL                                    
DEMRPL   RPL   ACB=DEMACB,AM=VSAM,                                     X        
               AREA=VSAREA,AREALEN=(L'VSAREA),ARG=VSMKEY,              X        
               OPTCD=(KEY,FWD,SYN,NSP,KGE)                                      
*                                                                               
* USED FOR SPLIT FILE:                                                          
DEMAC2   ACB   AM=VSAM,DDNAME=DEMVS2,MACRF=(KEY,DIR,SKP,IN),           X        
               BUFNI=10,BUFND=16,RMODE31=ALL                                    
DEMRP2   RPL   ACB=DEMAC2,AM=VSAM,                                     X        
               AREA=VSAREA,AREALEN=(L'VSAREA),ARG=VSMKEY,              X        
               OPTCD=(KEY,FWD,SYN,NSP,KGE)                                      
*                                                                               
*              I/O AREAS                                                        
*                                                                               
         DS    0L                                                               
VSRDW    DC    XL4'00'             AREA TO ADD RECORD LENGTH (RDW)              
VSAREA   DS    CL(8*1024)          VSAM INPUT AREA (SEE DEMRPL)                 
         DS    0L                                                               
OUTAREA  DS    CL(8*1024)                                                       
*                                                                               
         EJECT                                                                  
*              LITERALS USING LARL (ALL MUST BE HW ALIGNED)                     
*                                                                               
         DS    0H                                                               
*                                                                               
*              COUNTER DESCRIPTIONS. MAX 35 BYTES, EXCEPT WHERE MARKED          
*                                                                               
*                12345678901234567890123456789012345                            
*        ALL MODES:                                                             
LTCIN    DC    C'RECORDS READ',0H'0'                                            
LTCINB   DC    C'BYTES READ, INC RDW',0H'0'            MAX 30 BYTES             
*        ALL MODES FILE RECORDS (I.E. NOT DIRECTORY RECORDS):                   
LTCFIL   DC    C'DA RECORDS READ',0H'0'                                         
LTCFILB  DC    C'DA BYTES READ, INC RDW',0H'0'         MAX 30 BYTES             
LTCFILTD DC    C'TEST DATA RECORDS READ',0H'0'                                  
LTCFILX  DC    C'DA RECORDS MARKED AS EXTENDED PSV',0H'0'                       
LTCFILD  DC    C'DELETED DA RECORDS',0H'0'                                      
LTCFILM  DC    C'DA RECORDS MARKED DELETED',0H'0'                               
LTCMAJKY DC    C'MAJOR KEYS (EXCL. PASSIVES)',0H'0'                             
LTCMINMX DC    C'MAXIMUM MINOR KEYS PER MAJOR',0H'0'                            
LTCMINAV DC    C'AVERAGE MINOR KEYS PER MAJOR',0H'0'                            
LTCDUPKY DC    C'DUPLICATED KEYS',0H'0'                                         
LTCDUPMX DC    C'MAXIMUM DUPLICATES PER KEY',0H'0'                              
LTCDUPAV DC    C'AVERAGE DUPLICATES PER KEY',0H'0'                              
LTCFILPL DC    C'RECORDS IGNORED AS S/B PEELED',0H'0'                           
*        ALL MODES DIRECTORY RECORDS:                                           
LTCDIR   DC    C'DIRECTORY RECORDS READ',0H'0'                                  
LTCDIRB  DC    C'DIRECTORY BYTES READ, INC RDW',0H'0'  MAX 30 BYTES             
LTCDIRTD DC    C'TEST DATA RECORDS READ',0H'0'                                  
LTCPASX  DC    C'EXTENDED PASSIVES',0H'0'                                       
LTCPASXD DC    C'DELETED EXTENDED PASSIVES',0H'0'                               
LTCPASXM DC    C'EXTENDED PASSIVES MARKED DELETED',0H'0'                        
LTCDNX   DC    C'OTHER DIRECTORY RECORDS',0H'0'                                 
LTCDNXD  DC    C'DELETED DIRECTORY RECORDS',0H'0'                               
LTCDNXP  DC    C'DELETED PASSIVE DIRECTORY RECORDS',0H'0'                       
LTCDIRPL DC    C'DIR. RECS. IGNORED AS S/B PEELED',0H'0'                        
*        OUTPUT COUNTERS, COPY OR UPDATE:                                       
LTCOUT   DC    C'OFILE RECORDS WRITTEN',0H'0'                                   
LTCOUTB  DC    C'OFILE BYTES WRITTEN, INC RDW',0H'0'   MAX 30 BYTES             
LTCOU2   DC    C'OFIL2 RECORDS WRITTEN',0H'0'                                   
LTCOU2B  DC    C'OFIL2 BYTES WRITTEN, INC RDW',0H'0'   MAX 30 BYTES             
LTCMAXLN DC    C'MAX O/P RECORD LENGTH, INC RDW',0H'0'                          
LTCAVGLN DC    C'AVG O/P RECORD LENGTH, INC RDW',0H'0'                          
*        OUTPUT COUNTERS UPDATE ONLY:                                           
LTCBAK   DC    C'BKFILE RECORDS WRITTEN',0H'0'                                  
LTCBA2   DC    C'BKFIL2 RECORDS WRITTEN',0H'0'                                  
*        OUTPUT COUNTERS COMPARE ONLY:                                          
LTCNEWF  DC    C'NEWFILE RECORDS WRITTEN',0H'0'                                 
LTCOLDF  DC    C'OLDFILE RECORDS WRITTEN',0H'0'                                 
LTCDIFF  DC    C'DIFFILE RECORDS WRITTEN',0H'0'                                 
*        RECORD CHECK WARNINGS AND ERRORS:                                      
LTCRCE01 DC    C'RDW LENGTH TOO SMALL',0H'0'                                    
LTCRCE02 DC    C'DDS LENGTH TOO SMALL',0H'0'                                    
LTCRCE03 DC    C'ELEMENT LENGTH ZERO',0H'0'                                     
LTCRCW01 DC    C'MISSING EOR, ADDED',0H'0'                                      
LTCRCW02 DC    C'EOR BEFORE RECORD END, FIXED',0H'0'                            
*        MERGE PROCESS COUNTERS, UPDATE ONLY:                                   
LTCINSRT DC    C'NEW RECORDS ADDED TO FILE',0H'0'                               
LTCREPL  DC    C'RECORDS WITH SAME KEY AS FILE',0H'0'                           
LTCREPLD DC    C'DELETED RECORDS WITH SAME KEY',0H'0'                           
LTCAOF   DC    C'RECORDS IDENTICAL TO FILE, IGNORED',0H'0'                      
LTCDTDF  DC    C'RECORDS MATCH EXCEPT FOR CONV. DATE',0H'0'                     
LTCDDF   DC    C'DELETING DELETED RECORD, IGNORED',0H'0'                        
LTCDOF   DC    C'DELETED RECORDS ON FILE REPLACED',0H'0'                        
LTCDELET DC    C'UNMATCHED DA MINOR KEYS DELETED',0H'0'                         
LTCDELWR DC    C'"DELETE" RECORDS WRITTEN',0H'0'                                
LTCPTRD  DC    C'DIR REC ASSUMED DELETED POINTER',0H'0'                         
*        MERGE PROCESS, ERROR COUNTERS, UPDATE ONLY:                            
LTCERP2F DC    C'PASSIVE REPLACE MAJOR, IGNORED',0H'0'                          
LTCERF2P DC    C'MAJOR REPLACE PASSIVE, IGNORED',0H'0'                          
LTCERDNF DC    C'KEY TO DELETE NOT FOUND, IGNORE',0H'0'                         
*        MERGE PROCESS, ADDED/REPLACED/DELETED COUNTERS, UPDATE ONLY:           
LTCMJ    DC    C'MAJOR KEY SETS',0H'0'                                          
LTCMN    DC    C'MINOR KEYS',0H'0'                                              
LTCPV    DC    C'ORDINARY PASSIVES',0H'0'                                       
LTCPX    DC    C'EXTENDED PASSIVES',0H'0'                                       
*        COMPARE PROCESS COUNTERS:                                              
LTCCMPDL DC    C'DELETED VSAM RECORDS IGNORED',0H'0'                            
LTCCMPPL DC    C'VSAM RECORDS IGNORED AS S/B PEELED',0H'0'                      
LTCCMPEQ DC    C'MATCHED RECORDS',0H'0'                                         
LTCCMPUN DC    C'UNMATCHED INPUT RECORDS',0H'0'                                 
LTCCMPUV DC    C'UNMATCHED VSAM RECORDS',0H'0'                                  
LTCCMPNE DC    C'MATCHED KEY, DATA DIFFERENT',0H'0'                             
*                                                                               
*              ERROR AND WARNING MESSAGES                                       
*                                                                               
LTCWPDUP DC    C'*WARNING* - POSSIBLE DUPLICATE DATA RELOAD',0H'0'              
LTCMBZ   DC    C'*ERROR* - SHOULD NEVER BE NON-ZERO',0H'0'                      
LTCFLDUP DC    C'*IGNORED* - CAN''T DELETE SINGLE MINORS',0H'0'                 
LTGTDF1  DC    C'*ERROR* - TEST DATA FOUND. TERMINATING',0H'0'                  
LTGTDF2  DC    C'USE TESTDATA=Y PARAMETER IF REQUIRED',0H'0'                    
*                                                                               
LTPMCM   DC    C'MODE= CARD MISSING',0H'0'                                      
LTPMRF   DC    C'MODE=COMPARE REQUIRES FILE=',0H'0'                             
LTPMVC   DC    C'MAXCOMP= VALID ONLY WITH MODE=COMPARE',0H'0'                   
LTPCIVU  DC    C'COUNT= INVALID WITH MODE=UPDATE',0H'0'                         
LTPSDN   DC    C'SPLIT=Y VALID ONLY WITH FILE=DEMVSMN',0H'0'                    
LTPPMU   DC    C'PURGE= INVALID WITH MODE=UPDATE',0H'0'                         
LTPICC   DC    C'INVALID CONTROL CARD',0H'0'                                    
LTPDPC   DC    C'DUPLICATE CARD',0H'0'                                          
LTPRAB   DC    C'RUN ABORTED DUE TO ABOVE ERRORS',0H'0'                         
LTPMLC   DC    C'MODE= MUST BE COPY, LOAD OR COMPARE',0H'0'                     
LTPMLCU  DC    C'MODE= MUST BE COPY, LOAD, COMPARE OR UPDATE',0H'0'             
         EJECT                                                                  
***********************************************************************         
* ENTRY FROM AND RETURN TO DFSORT WHEN RUNNING AS A DFSORT E35 EXIT   *         
* FIRST TEST IF WE ARE ENTERED FROM DFSORT. IF NOT, NORMAL CODE       *         
***********************************************************************         
         USING E35,RF                                                           
E35      SAVE  (14,12),,DEM2VSAM   ENTRY GOOD FOR MVS AS WELL AS DFSORT         
*                                                                               
         CLC   1(3,RD),=C'SM1'     DFSORT ALWAYS CALLS WITH THIS                
         JNE   DEM2VSAM            USUAL NBASE IF NOT FROM DFSORT               
*                                                                               
* IF WE REACH THIS POINT, WE ARE RUNNING AS A DFSORT E35 EXIT.                  
* PREPARE TO ENTER THE MAIN PROGRAM CODE BY SETTING UP TO RUN UNDER             
* DFSORT.                                                                       
*                                                                               
         STMH  GR0,GRF,DFSORTHH                                                 
         LRL   RE,=V(REGSAVE)      GET OUR SAVE AREA CHAIN                      
         ST    RD,4(,RE)           SAVE FORWARD POINTER IN OURS                 
         ST    RE,8(,RD)           ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         L     R2,0(,R1)           GET ADDRESS OF RECORD                        
         LARL  RB,DEM2VSAM         USE THE SAME REGISTERS AS OUR NBASE          
         LAY   RA,4096(,RB)                                                     
         LARL  R7,COMMON                                                        
         MVIY  E35EXIT,YES                                                      
         SR    R0,R0                                                            
         STHRL R0,E35RC            PRESET RC=0                                  
         LARL  R3,OUTAREA                                                       
         LY    R9,VCPRINT                                                       
*                                                                               
* JUMP TO THE MAIN PROGRAM.                                                     
*                                                                               
         BRC   0,MAIN              GO BACK TO MAIN IF NOT FIRST TIME            
         OI    *-3,X'F0'           *** ONLY DO THIS ONCE   ***                  
         J     INIT                GO TO INIT FIRST TIME IN                     
*                                                                               
* WHEN WE ARE INVOKED AS A DFSORT E35 EXIT, THIS IS THE EXIT POINT              
* BACK TO DFSORT.                                                               
*                                                                               
E35GOBAK DS    0H                                                               
         LARL  RF,E35              RESET E35 BASE                               
         SGR   GR1,GR1                                                          
         LR    R1,R3               SET RECORD POINTER                           
         LMH   GR0,GR0,DFSORTHH    RESTORE HIGH HALVES OF DFSORT'S REGS         
         LMH   GR2,GRE,DFSORTHH+8                                               
         LGHRL GRF,E35RC           SET RC (WAS OUR BASE)                        
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE DFSORT'S REGS                        
         BSM   0,RE                RETURN TO DFSORT                             
*                                                                               
DFSORTHH DS    8D                  HIGH HALVES OF DFSORT'S REGISTERS            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
*              RECORD DSECTS                                                    
*                                                                               
INREC    DSECT ,                   RECORD FROM DELDXMOD OUTPUT                  
INRDW    DS    0XL4                RDW                                          
INLENMVS DS    XL2                 MVS RECORD LENGTH                            
         DS    XL2                                                              
INKEY    DS    0CL20                                                            
INKEYFMS DS    0CL3                FILE/MEDIA/SOURCE                            
INKEYMAJ DS    CL18                MAJOR KEY                                    
INKEYMIN DS    CL2                 DA MINOR KEY                                 
INLENDDS DS    XL2                 DA DDS RECORD LENGTH                         
INSTAT   DS    X                   DA STATUS BYTE                               
         ORG   INKEYMIN            REDEFINE FOR IS-ONLY PASSIVE                 
INPDATA  DS    XL4                 IS DATA (IN DISK ADDRESS LOCATION)           
INPSTAT  DS    X                   PASSIVE STATUS (NOTE WRONG PLACE!)           
INPLENQ  EQU   *-INREC             LENGTH OF A PASSIVE RECORD                   
*                                                                               
OTREC    DSECT ,                   OUTPUT RECORD                                
OTLENMVS DS    0XL2                MVS RECORD LENGTH                            
OTRDW    DS    XL4                 MVS RDW                                      
OTVSREC  DS    0C                  VSAM RECORD                                  
*                                                                               
       ++INCLUDE DEVSMFILE                                                      
*                                                                               
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         IFGACB AM=VSAM                                                         
         IFGRPL AM=VSAM                                                         
* DDDPRINT                                                                      
* DEDEMFILE                                                                     
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021DEMTOVSAM 12/03/20'                                      
         END                                                                    
