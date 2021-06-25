*          DATA SET DEVSUTIL   AT LEVEL 001 AS OF 12/04/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEVSUTA                                                                  
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
*INCLUDE SQUASHER                                                               
*INCLUDE REGSAVE                                                                
*ENTRY   E35                       *** PROGRAM ENTRY POINT                      
*                                                                               
         TITLE 'DEMO VSAM UTILITY (FORMERLY DEMTOVSAM)'                         
***********************************************************************         
* TWO MODES (VIA MODE=XXX PARAMETER CARD):                                      
*                                                                               
* MODE=COPY COPIES A DEMO FILE. OUTPUT IS ALWAYS VSAM FORMAT, BUT               
*        INPUT CAN BE DANDX OR VSAM FORMAT (SEE INPUT=). THIS IS                
*        THE DEFAULT MODE WHEN RUNNING AS AN E35 EXIT TO DFSORT.                
* MODE=LOAD IS A SYNONYM FOR MODE=COPY                                          
* MODE=COMPARE WORKS EXACTLY LIKE MODE=COPY EXCEPT THAT IT SIMPLY               
*        COMPARES THE INPUT FILE WITH THE TARGET VSAM FILE, IGNORING            
*        ALL RECORDS MARKED AS DELETED, AND REPORTS ANY DIFFERENCES.            
* UPDATE MODE WAS IN DEM2VSM BUT HAS BEEN SPLIT OUT INTO A NEW DEVSUPD          
*                                                                               
* INPUT=VSAM IMPLIES THAT THE INPUT FILE RECORDS ARE IN VSAM                    
*       FORMAT (DEFAULT).                                                       
* INPUT=DANDX IMPLIES THAT THE INPUT FILE RECORDS ARE IN DANDX                  
*       FORMAT. THE RECORDS WILL BE CONVERTED TO VSAM FORMAT ON                 
*       OUTPUT.                                                                 
*                                                                               
* PROGRAM CAN RUN AS A DFSORT E35 EXIT, IN WHICH CASE PARAMETER CARDS           
* ARE READ FROM DVUIN RATHER THAN SYSIN. IF DVUIN CAN'T BE OPENED, WE           
* ASSUME COPY MODE. ONLY COPY AND COMPARE MODES VALID IN DFSORT EXIT.           
*                                                                               
* SINCE THE PROGRAM CAN RUN FOR EXTENDED PERIODS, IT SUPPORTS A SIMPLE          
* OPERATOR INTERFACE. AN MVS MODIFY COMMAND CAN BE USED TO ENQUIRE ON           
* PROGRESS. THE RESPONSE WILL SHOW THE NUMBER OF RECORDS READ, NUMBER           
* OF KILOBYTES READ AND THE CURRENT MAJOR KEY.                                  
*                                                                               
* E.G. F JOBNAME,STATUS OR F JOBNAME,?                                          
*                                                                               
* AN MVS STOP COMMAND (P JOBNAME) CAN BE USED TO FORCE EOF.                     
*                                                                               
***********************************************************************         
*                                                                               
* THE PROGRAM DOES NOT DIRECTLY SUPPORT THE SPLIT DEMVSMN FILE BUT              
* WHEN RUNNING AS A SORT EXIT, THE FOUR PARTS OF THE SPLIT FILE CAN             
* FORM FOUR INPUTS TO A SORT MERGE WITH EITHER A SINGLE SORTOUT FILE            
* TO RECOMBINE THE SPLIT FILE OR MULTIPLE SORT OUTFIL STATEMENTS CAN            
* BE USED TO REARRANGE THE WAY THE DATA IS SPLIT ACCOSS THE FILES.              
*                                                                               
* MAIN REGISTER'S THROUGHOUT:                                                   
* R2 - VSAM OR DANDX FORMAT INPUT (FROM IFILE LOCATE MODE OR FROM SORT)         
* R3 - VSAM FORMAT OUTPUT IN OUTAREA (TO OFILE MOVE MODE OR SORT)               
* R4 - VSAM DEMO INPUT RECORD WHEN MODE=COMPARE (MOVE MODE)                     
* R7 - COMMON STORAGE                                                           
* R9 - DPRINT                                                                   
*                                                                               
***********************************************************************         
         EJECT                                                                  
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
*  SO TO DEBUG AN "A" VERSION OF DEVSUT: IF "INIT" IS LOCATED AT                
*  OFFSET X'7E' FROM THE START OF THE LOAD MODULE, ISSUE THE FOLLOWING          
*  IDF COMMANDS BEFORE ISSUING A "RUN" COMMAND:                                 
*                                                                               
*  ==> DBREAK (DEVSUTA.)+X'7E'                                                  
*  ==> LAN LOAD DEVSUT                                                          
*                                                                               
***********************************************************************         
* E35 ENTRY POINT JUMPS TO HERE ON ENTRY IF *NOT* RUNNING AS SORT EXIT          
***********************************************************************         
         PRINT NOGEN                                                            
DEVSUT   CSECT                                                                  
*                                                                               
         ENTRY E35                 PROGRAM ENTRY POINT                          
         ENTRY DATEVAL             "TODAY'S" DATE: FORMAT MM/DD/YY              
         ENTRY DEMTABS             V(DEMTABS)                                   
*                                                                               
         NBASE 0,**DVUT**,WORK=V(REGSAVE)                                       
         LARL  R7,COMMON                                                        
         USING COMMON,R7                                                        
         L     R9,VCPRINT                                                       
         USING DPRINT,R9           R9=A(PRINT CSECT)                            
*                                                                               
         USING INREC,R2            R2=INPUT (LOCATE MODE OR FROM SORT)          
*                                                                               
         LARL  R3,OUTAREA                                                       
OUTREC   USING RECDSECT,R3         R3=OUTPUT AREA (MOVE MODE)                   
OTDATA   USING DVREC,OUTREC.RECDATA DEMO VSAM RECORD OUTPUT                     
*                                                                               
         MVI   E35EXIT,NO          INDICATE WE ARE NOT A SORT EXIT              
*                                                                               
***********************************************************************         
* INITIALISATION                                                                
* E35 ENTRY POINT JUMPS HERE ON FIRST CALL IF RUNNING AS SORT EXIT              
***********************************************************************         
INIT     DS    0H                                                               
         EXTRACT ATIOT,'S',FIELDS=(TIOT)                                        
         L     RF,ATIOT                                                         
         MVC   JOBNAME,0(RF)       GET MY JOBNAME                               
         MVC   STEPNAME,8(RF)      GET MY STEPNAME                              
                                                                                
         EXTRACT AASID,'S',FIELDS=(ASID)                                        
         L     R4,AASID            MVS JOB ACCOUNTING INFORMATION               
         LOCASCB ASID=(4)          PUTS A(ASCB) INTO R1                         
         ST    R1,AASCB            SAVE A(ASCB)                                 
*                                                                               
         L     R1,ASCBASSB-ASCB(,R1) R1=A(ASSB)                                 
         SAM31 ,                                                                
         L     R1,ASSBJSAB-ASSB(,R1) R1=A(JSAB)                                 
         MVC   JOBID,JSABJBID-JSAB(R1) GET JOBID (E.G. JOB12345)                
         SAM24 ,                                                                
*                                                                               
         EXTRACT ACOMM,FIELDS=(COMM)                                            
         L     RF,ACOMM            SET UP OPERATOR COMMUNICATIONS               
         USING COMLIST,RF                                                       
         MVC   AOPERECB,COMECBPT                                                
         L     R4,COMCIBPT         GET A(CIB)                                   
         LA    R5,COMCIBPT         GET A(A(CIB))                                
         DROP  RF                                                               
                                                                                
         CLI   CIBVERB-CIBNEXT(R4),CIBSTART                                     
         JNE   INIT02                                                           
         QEDIT ORIGIN=(5),BLOCK=(4)                                             
                                                                                
INIT02   QEDIT ORIGIN=(5),CIBCTR=1 ALLOW OPERATOR MODIFY COMMANDS               
         LAM   AR0,ARF,ARZERO                                                   
*                                                                               
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'99'                                                      
*                                                                               
         MVC   TITLE(3),=C'JOB'                                                 
         MVC   TITLE+4(8),JOBNAME                                               
         MVI   TITLE+13,C'('                                                    
         MVC   TITLE+14(1),JOBID                                                
         MVC   TITLE+15(5),JOBID+3                                              
         MVI   TITLE+20,C')'                                                    
         MVC   TITLE+22(17),=C'VSAM DEMO UTILITY' ASSUME NOT E35                
*                                                                               
         GOTO1 =V(LOADER),DMCB,=C'T00AD1  ',0                                   
         ICM   RE,15,4(R1)         GET A(DEMTABS)                               
         JZ    *+2                 CAN'T GET ADDRESS ?!?                        
         ST    RE,DEMTABS          PUBLISH VIA ENTRY POINT                      
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(10,DATEVAL)  TODAY: MM/DD/YY              
*                                                                               
         IF (CLI,E35EXIT,EQ,YES)                                                
                                                                                
           MVC   TITLE+22+17+1(5),=C'(E35)'                                     
           L     RE,=V(SYSIN)        PICK UP SYSIN DCB IN CARDS                 
           MVC   DCBDDNAM-IHADCB(,RE),=CL8'DVUIN' CHANGE DDNAME                 
           BRAS  RE,VALPARMS         VALIDATE PARAMETER CARD(S)                 
                                                                                
         ELSE  ,                                                                
                                                                                
           BRAS  RE,VALPARMS         VALIDATE PARAMETER CARD(S)                 
           LARL  RF,IFILE            NOT SORT EXIT SO NEEDS INPUT FILE          
           BRAS  R8,GETDSN                                                      
           MVC   INDSN,0(R3)                                                    
           OPEN  (IFILE,INPUT)                                                  
           IF (CLI,MODE,EQ,COPY)     COPY MODE?                                 
             LARL  RF,OFILE                                                     
             BRAS  R8,GETDSN                                                    
             MVC   OUTDSN,0(R3)                                                 
             OPEN  (OFILE,OUTPUT)    COPY NEEDS OUTPUT FILE IF NOT SORT         
           ENDIF ,                                                              
                                                                                
         ENDIF ,                                                                
*                                                                               
         IF (CLI,MODE,EQ,COMPARE)  COMPARE MODE?                                
           OPEN  (NEWFILE,OUTPUT,OLDFILE,OUTPUT,DIFFILE,OUTPUT)                 
           OPEN  (DEMACB)            OPEN VSAM FILE TO COMPARE                  
           LTR   RF,RF               RF HOLDS ERROR RETURN CODE                 
           JNZ   *+2                                                            
         ENDIF ,                                                                
*                                                                               
INITX    J     MAIN                                                             
*                                                                               
***********************************************************************         
*        GET DSN FOR THE DCB AT RF INTO IOAREA AT R3                            
***********************************************************************         
GETDSN   DS    0H                                                               
         STCM  R3,7,DSNXTRCT+1     SET EXTRACT AREA TO IOAREA AT R3             
         LA    R1,DSNXTRCT                                                      
         STCM  R1,7,DCBEXLSA-IHADCB(RF) SET EXLST ADDRESS IN DCB AT RF          
         RDJFCB ((RF))             GET DSN                                      
         BR    R8                                                               
         EJECT                                                                  
***********************************************************************         
* MAIN LOOP                                                                     
* E35 ENTRY POINT JUMPS HERE ON ALL BUT 1ST CALL IF RUNNING AS SORT XIT         
***********************************************************************         
MAIN     DS    0H                                                               
         L     RF,AOPERECB         GET A(OPERATOR ECB)                          
         TM    0(RF),X'40'         TEST MESSAGE/COMMAND PENDING                 
         JZ    MAIN10              NO, CARRY ON                                 
                                                                                
         BRAS  RE,OPSINP           YES, SEE WHAT IT WAS                         
         CLI   OPERSTOP,YES        TEST STOPPED BY OPERATOR                     
         JE    EOFIN               YES, FORCE EOF                               
                                                                                
MAIN10   DS    0H                                                               
         CLI   E35EXIT,YES         TEST RUNNING AS A DFSORT E35 EXIT            
         JNE   MAIN12                                                           
         LTR   R2,R2               YES, R2 IS ALREADY A(RECORD)                 
         JNZ   MAIN20                                                           
         J     EOFIN               OR R2 IS ZERO IF NO MORE RECORDS             
*                                                                               
MAIN12   DS    0H                                                               
         LARL  R1,IFILE                                                         
         GET   (1)                 GET NEXT RECORD                              
         LR    R2,R1                                                            
*                                                                               
MAIN20   DS    0H                                                               
*                                                                               
         AP    CTRIN,=P'1'         COUNT RECORDS IN                             
*                                                                               
         OC    INKEYMAJ,INKEYMAJ   TEST FOR NULL MAJOR KEY                      
         JZ    *+2                 ABSOLUTELY SHOULD NOT HAPPEN                 
*                                                                               
         IF (TM,COUNTFLG,COUNTYES,O),AND, IF COUNT= CARD IS PRESENT...          
            (CLC,CNTARKEY,NE,INKEYMAJ)    ..AND MAJOR KEY CHANGED               
           IF (OC,CNTARKEY,CNTARKEY,NZ)     IF NOT FIRST TIME                   
             MVI   PLCOUNT,1                TELL LDCOUNT TO "COUNT"             
             GOTO1 VLDCOUNT,PLCOUNT         ...LAST MAJOR                       
           ENDIF ,                                                              
*                                      DELDDCNT REQUIRES DANDX-STYLE            
           MVC   CNTARKEY,INKEYMAJ     MAJOR KEY. SET UP FOR NEW MAJOR          
           MVI   CNTARSTA,DVSDEL       AND POSIT NEW MAJOR IS DELETED           
         ENDIF ,                                                                
*                                                                               
         IF (CLC,THISKFMS,NE,INKEYFMS),AND, WHEN F/M/S CHANGES..                
            (OC,THISKFMS,THISKFMS,NZ) ,     ..AND NOT FIRST TIME                
           BAS RE,PRNTFMS              PRINT TOTS. FOR LAST F/M/S               
         ENDIF ,                       JUST SETS HEADING IF F/M/S NULL          
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,B'0011',INLENMVS GET INPUT MVS BYTES                          
         LG    GRF,FMSBYTES        BYTE COUNT PER FILE/MEDIA/SOURCE             
         AGFR  GRF,R0                                                           
         STG   GRF,FMSBYTES        (KEEP R0 FOR IFBYTES/IDBYTES)                
         AP    CTRINFMS,=P'1'      COUNT RECORDS IN (FOR THIS F/M/S)            
*                                                                               
         CLI   INPARM,VSAM         INPUT=VSAM?                                  
         JE    MAINV20             YES, SKIP TO VSAM CODE                       
*                                                                               
*        INPUT IS DANDX. HANDLE CONVERSION TO VSAM FORMAT                       
*        NOTE R0 = RECORD LENGTH (TO ACCUMULATE IFBYTES OR IDBYTES)             
*                                                                               
MAIND20  DS    0H                                                               
         CLC   INLENMVS,=Y(INPLENQ) IS THIS A DIRECTORY RECORD?                 
         JE    MAIND50             YES, WHOLE DIFFERENT REFORMAT                
*                                                                               
*        'NORMAL' DEMO RECORD (I.E. NOT PASSIVE)                                
*                                                                               
         AP    CTRFIL,=P'1'                                                     
*                                                                               
         LG    GRF,IFBYTES                                                      
         AGFR  GRF,R0                                                           
         STG   GRF,IFBYTES                                                      
*                                                                               
         LLC   R0,INSTAT           STATUS BYTE                                  
         NILF  GR0,DANDXFL#        ISOLATE LOGICAL FILE NUMBER                  
         CHI   R0,TESTFIL#         IS THIS A TEST FILE RECORD?                  
         JNE   MAIND22             NO                                           
         AP    CTRFILTD,=P'1'      COUNT FILE TEST DATA                         
         CLI   TESTDATA,YES        ARE WE EXPECTING TEST DATA?                  
         JNE   MAINETST            NO, ABORT THIS RUN                           
*                                                                               
MAIND22  DS    0H                                                               
         TM    INSTAT,DVSDEL       IS IT DELETED?                               
         JZ    MAIND23                                                          
         AP    CTRFILD,=P'1'       COUNT DELETED FILE RECORDS                   
         J     MAINDEL             AND DROP THE RECORD                          
*                                                                               
MAIND23  DS    0H                                                               
         LA    R1,INKEYMAJ                                                      
         BRAS  R8,MAINPRGE         SHOULD RECORD BE PURGED?                     
         JNE   MAIND24                                                          
         AP    CTRFILPL,=P'1'      COUNT FILE RECORDS IGNORED AS PURGED         
         J     MAINDEL             AND DROP THE RECORD                          
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
MAIND26  DS    0H                  NEW MAJOR KEY                                
         AP    CTRMAJKY,=P'1'      COUNT MAJOR KEYS                             
*                                                                               
         CP    CTRMINMX,IMINORS    WAS LAST MAXIMUM MINORS SO FAR               
         JNL   *+10                                                             
         MVC   CTRMINMX,IMINORS    YES, SAVE IT                                 
         ZAP   IMINORS,=P'1'                                                    
*                                                                               
MAIND28  DS    0H                  SAME MAJOR KEY AS LAST, NEW MINOR            
         CP    CTRDUPMX,IDUPKEY    WAS LAST MAXIMUM DUPS SO FAR                 
         JNL   *+10                                                             
         MVC   CTRDUPMX,IDUPKEY    YES, SAVE IT                                 
         ZAP   IDUPKEY,=P'0'                                                    
         MVC   THISKEY,INKEY       SAVE NEW KEY                                 
         MVI   SEQNO,0             RESET SEQUENCE NUMBER                        
         J     MAIND40                                                          
*                                                                               
MAIND30  DS    0H                  SAME MAJOR AND SAME MINOR AS LAST            
         AP    IDUPKEY,=P'1'       COUNT DUPLICATES THIS KEY                    
         AP    CTRDUPAV,=P'1'      COUNT FOR AVERAGE CALC LATER                 
         LLC   R1,SEQNO            GET SEQUENCE NUMBER                          
         LTR   R1,R1               IS THIS FIRST DUPLICATE FOR THIS KEY         
         JNZ   *+10                                                             
         AP    CTRDUPKY,=P'1'      YES, COUNT NUMBER OF KEYS DUPLICATED         
         LA    R1,1(,R1)                                                        
         STC   R1,SEQNO            INCREMENT SEQUENCE NUMBER                    
         CLI   SEQNO,250                                                        
         JH    *+2                 DIE IF SEQNO > 250                           
*                                                                               
MAIND40  DS    0H                  CONVERT RECORD TO VSAM FORMAT                
         XR    RE,RE                                                            
         ICM   RE,B'0011',INLENMVS MVS LENGTH                                   
         LR    R1,RE               SAVE FOR MVCL                                
         LA    RE,1(,RE)           INCREASE MVS LENGTH BY 1                     
         SLL   RE,16                                                            
         ST    RE,OUTREC.RECRDW    SET O/P MVS LEN (RDW)                        
         MVC   OTDATA.DVKEY(L'INKEY),INKEY COPY MAJOR/MINOR KEY                 
         MVC   OTDATA.DVKEYSEQ,SEQNO      INSERT SEQUENCE NUMBER                
         ICM   RE,B'0011',INLENDDS DDS LENGTH (HI 2 BYTES IRRELEVANT)           
         LA    RE,1(,RE)           INCREASE DDS LENGTH BY 1                     
         STCM  RE,B'0011',OTDATA.DVLENDDS SET O/P DDS LEN                       
         LA    R0,OTDATA.DVKEY+L'DVKEY+L'DVLENDDS 1ST BYTE FOR REMAINDR         
         AHI   R1,-(L'INRDW+L'INKEY+L'INLENDDS) LESS LEN DONE SO FAR            
         LA    RE,INKEY+L'INKEY+L'INLENDDS FIRST BYTE TO MOVE REST FROM         
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE REST OF RECORD                          
         JO    *+2                 DESTRUCTIVE MOVE!                            
         NI    OTDATA.DVSTAT,X'FF'-(DVSEXTP+DANDXFL#) 0 UNWANTED STATUS         
         J     MAINOUT             KEEP RECORD                                  
*                                                                               
*        DEMO DIRECTORY RECORD (HAS NO MINOR KEY & SHOULD BE A PASSIVE)         
*        NOTE R0 = RECORD LENGTH (TO ACCUMULATE IFBYTES OR IDBYTES)             
*                                                                               
MAIND50  DS    0H                                                               
         AP    CTRDIR,=P'1'        COUNT DIRECTORY RECORDS                      
*                                                                               
         LG    GRF,IDBYTES                                                      
         AGFR  GRF,R0                                                           
         STG   GRF,IDBYTES                                                      
*                                                                               
         LLC   R0,INPSTAT          STATUS BYTE                                  
         NILF  GR0,DANDXFL#        ISOLATE LOGICAL FILE NUMBER                  
         CHI   R0,TESTFIL#         IS THIS A TEST FILE RECORD?                  
         JNE   MAIND52             NO                                           
         AP    CTRDIRTD,=P'1'      COUNT TEST DATA POINTERS                     
         CLI   TESTDATA,YES        ARE WE EXPECTING TEST DATA?                  
         JNE   MAINETST            NO, ABORT THIS RUN                           
*                                                                               
MAIND52  DS    0H                                                               
         TM    INPSTAT,DVSEXTP     TEST EXTENDED                                
         JO    MAIND54                                                          
*                                                                               
         AP    CTRDNX,=P'1'        COUNT NOT EXTENDED                           
         TM    INPSTAT,DVSDEL      IS IT DELETED?                               
         JZ    MAIND58             NO, PROCESS                                  
         AP    CTRDNXD,=P'1'       COUNT NOT EXTENDED AND DELETED               
         J     MAINDEL             DROP DELETED PASSIVES                        
*                                                                               
MAIND54  DS    0H                                                               
         AP    CTRPASX,=P'1'       COUNT EXTENDED PASSIVES                      
         TM    INPSTAT,DVSDEL      IS IT DELETED?                               
         JZ    MAIND58             NO, PROCESS                                  
         AP    CTRPASXD,=P'1'      COUNT EXTENDED AND DELETED                   
         J     MAINDEL             DROP DELETED PASSIVES                        
*                                                                               
MAIND58  DS    0H                                                               
         XC    THISKMIN,THISKMIN   CLEAR MINOR PART                             
         CLC   THISKMAJ,INKEYMAJ   SAME KEY AS THE LAST RECORD                  
         JE    *+2                 REALLY SHOULD NOT HAPPEN                     
*                                                                               
         MVC   THISKMAJ,INKEYMAJ   SAVE KEY                                     
*                                                                               
         LA    R1,INKEYMAJ                                                      
         BRAS  R8,MAINPRGE         SHOULD DIRECTORY RECORD BE PURGED?           
         JNE   MAIND60             NO, KEEP IT                                  
         AP    CTRDIRPL,=P'1'      COUNT DIR RECORDS IGNORED AS PURGED          
         J     MAINDEL             DROP RECORD                                  
*                                                                               
MAIND60  DS    0H                  CONVERT RECORD TO VSAM FORMAT                
         MVC   OTDATA.DVKEYMAJ,INKEYMAJ       COPY MAJOR KEY                    
         XC    OTDATA.DVKEYMIN,OTDATA.DVKEYMIN CLEAR MINOR KEY                  
         MVI   OTDATA.DVKEYSEQ,PASSIVE# INDICATE PASSIVE IN KEY SEQ FLD         
         MVC   OTDATA.DVSTAT,INPSTAT     COPY STATUS                            
         NI    OTDATA.DVSTAT,X'FF'-DANDXFL# ZERO UNWANTED STATUS                
         MVI   OTDATA.DVPEOR,0           NON-EXTENDED END OF RECORD             
         LA    R1,DVPRLNQ                NON-EXTENDED RECORD LENGTH             
         TM    OTDATA.DVSTAT,DVSEXTP     EXTENDED PASSIVE?                      
         JZ    MAIND64                   NO, SKIP                               
         MVC   OTDATA.DVPEL(2),=AL1(X'01',DVPLENQ) DUMMY ELCODE/LENGTH          
         MVC   OTDATA.DVPDATA,INPDATA    COPY DATA TO ELEMENT                   
         MVI   OTDATA.DVPEEOR,0          EXTENDED END OF RECORD                 
         LA    R1,DVPERLNQ         EXTENDED RECORD LENGTH                       
*                                                                               
MAIND64  DS    0H                                                               
         STCM  R1,B'0011',OTDATA.DVLENDDS SET DDS LENGTH                        
         LA    R1,4(,R1)                                                        
         SLL   R1,16                                                            
         ST    R1,OUTREC.RECRDW    SET MVS LENGTH                               
         J     MAINOUT                                                          
         EJECT                                                                  
*                                                                               
*        INPUT IS VSAM FORMAT. NO CONVERSION NEEDED                             
*                                                                               
MAINV20  DS    0H                                                               
         LR    R0,R3               OUTPUT AREA                                  
         LR    RE,R2               INPUT RECORD                                 
         LH    R1,INLENMVS         LENGTH                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY INPUT TO OUTPUT AREA                    
         JO    *+2                 DESTRUCTIVE MOVE!                            
*                                                                               
         CLI   OTDATA.DVKEYSEQ,PASSIVE#  PASSIVE?                               
         JE    MAINV50             YES, SKIP                                    
*                                                                               
*        'NORMAL' VSAM RECORD (I.E. NOT PASSIVE)                                
*                                                                               
         AP    CTRFIL,=P'1'                                                     
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,B'0011',OUTREC.RECMVSLN ACCUMULATE INPUT MVS BYTES            
         LG    GRF,IFBYTES                                                      
         AGFR  GRF,R0                                                           
         STG   GRF,IFBYTES                                                      
*                                                                               
         TM    OTDATA.DVSTAT,DVSDEL IS IT DELETED?                              
         JZ    MAINV22                                                          
         AP    CTRFILD,=P'1'       COUNT DELETED FILE RECORDS                   
         J     MAINDEL             AND DROP RECORD                              
*                                                                               
MAINV22  DS    0H                                                               
         LA    R1,OTDATA.DVKEYMAJ                                               
         BRAS  R8,MAINPRGE         SHOULD RECORD BE PURGED?                     
         JNE   MAINV24             NO, KEEP IT                                  
         AP    CTRFILPL,=P'1'      COUNT DIR RECORDS IGNORED AS PURGED          
         J     MAINDEL             AND DROP RECORD                              
*                                                                               
MAINV24  DS    0H                                                               
         AP    CTRMINAV,=P'1'      COUNT TOTAL MINOR KEYS FOR AVG CALC          
         CLC   THISKMAJ,OTDATA.DVKEYMAJ  SAME MAJOR KEY AS LAST RECORD?         
         JNE   MAINV26             NO, SKIP                                     
         AP    IMINORS,=P'1'       COUNT MINORS THIS MAJOR                      
         CLC   THISKMIN,OTDATA.DVKEYMIN  SAME MINOR KEY AS LAST RECORD?         
         JNE   MAINV28             NO,                                          
         J     MAINV30             YES, SKIP                                    
*                                                                               
MAINV26  DS    0H                  NEW MAJOR KEY                                
         AP    CTRMAJKY,=P'1'      COUNT MAJOR KEYS                             
*                                                                               
         CP    CTRMINMX,IMINORS    WAS LAST MAXIMUM MINORS SO FAR               
         JNL   *+10                                                             
         MVC   CTRMINMX,IMINORS    YES, SAVE IT                                 
*                                                                               
         ZAP   IMINORS,=P'1'                                                    
*                                                                               
MAINV28  DS    0H                  SAME MAJOR KEY, NEW MINOR                    
         CP    CTRDUPMX,IDUPKEY    WAS LAST MAXIMUM DUPS SO FAR                 
         JNL   *+10                                                             
         MVC   CTRDUPMX,IDUPKEY    YES, SAVE IT                                 
         ZAP   IDUPKEY,=P'0'                                                    
         MVC   THISKEY,OTDATA.DVKEY SAVE NEW KEY                                
         MVI   SEQNO,0             RESET SEQUENCE NUMBER                        
         J     MAINV40                                                          
*                                                                               
MAINV30  DS    0H                  SAME MAJOR AND MINOR KEYS                    
         AP    IDUPKEY,=P'1'       COUNT DUPLICATES THIS KEY                    
         AP    CTRDUPAV,=P'1'      COUNT FOR AVERAGE CALC LATER                 
         LLC   R1,SEQNO            GET SEQUENCE NUMBER                          
         LTR   R1,R1               IS THIS FIRST FOR THIS KEY                   
         JNZ   *+10                                                             
         AP    CTRDUPKY,=P'1'      YES, COUNT NUMBER OF KEYS DUPLICATED         
         LA    R1,1(,R1)           INCREMENT SEQUENCE NUMBER                    
         STC   R1,SEQNO                                                         
         CLI   SEQNO,250                                                        
         JH    *+2                 DIE IF SEQNO > 250                           
*                                                                               
MAINV40  DS    0H                                                               
         MVC   OTDATA.DVKEYSEQ,SEQNO RESET SEQUENCE NUMBER IN RECORD            
         J     MAINOUT                                                          
*                                                                               
*        PASSIVE VSAM RECORD                                                    
*                                                                               
MAINV50  DS    0H                                                               
         AP    CTRDIR,=P'1'        COUNT DIRECTORY RECORDS                      
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,B'0011',OUTREC.RECMVSLN ACCUMULATE INPUT MVS BYTES            
         LG    GRF,IDBYTES                                                      
         AGFR  GRF,R0                                                           
         STG   GRF,IDBYTES                                                      
*                                                                               
         TM    OTDATA.DVSTAT,DVSEXTP EXTENDED PASSIVE?                          
         JO    MAINV54                                                          
*                                                                               
         AP    CTRDNX,=P'1'        COUNT NOT EXTENDED                           
         TM    OTDATA.DVSTAT,DVSDEL IS IT DELETED?                              
         JZ    MAINV58             NO, PROCESS                                  
         AP    CTRDNXD,=P'1'       COUNT NOT EXTENDED AND DELETED               
         J     MAINDEL             DROP DELETED PASSIVES                        
*                                                                               
MAINV54  DS    0H                                                               
         AP    CTRPASX,=P'1'       COUNT EXTENDED PASSIVES                      
         TM    OTDATA.DVSTAT,DVSDEL IS IT DELETED?                              
         JZ    MAINV58             NO, PROCESS                                  
         AP    CTRPASXD,=P'1'      COUNT EXTENDED AND DELETED                   
         J     MAINDEL             DROP DELETED PASSIVES                        
*                                                                               
MAINV58  DS    0H                                                               
         MVC   THISKEY,OTDATA.DVKEY SAVE NEW KEY                                
*                                                                               
         LA    R1,OTDATA.DVKEYMAJ                                               
         BRAS  R8,MAINPRGE         SEE IF REC SHOULD HAVE BEEN PURGED           
         JNE   MAINOUT             NO, KEEP IT                                  
         AP    CTRDIRPL,=P'1'      COUNT DIR RECORDS IGNORED AS PURGED          
*                                                                               
MAINDEL  DS    0H                                                               
         MVC   E35RC,=H'4'         SET RC=4 - DELETE (IF DFSORT EXIT)           
         J     MAINNEXT            IGNORE RECORD                                
         EJECT                                                                  
*                                                                               
* OUTPUT VSAM FORMAT RECORD                                                     
*                                                                               
MAINOUT  DS    0H                                                               
         BRAS  RE,RECCHECK         CHECK FORMAT OF OUTPUT RECORD                
*                                                                               
         MVI   CNTARSTA,0          INDICATE MAJOR NOT DELETED AFTER ALL         
*                                                                               
         CLI   MODE,COPY           IF COPY MODE,                                
         JNE   MAINO20                                                          
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
*        WE FOUND AN UNEXPECTED TEST DATA RECORD. ABORT THE RUN                 
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
*        DELDPRGE ONLY CALLED ON CHANGE OF MAJOR KEY. IF MAJOR KEY IS           
*        SAME AS LAST CALL, RESULT OF LAST CALL IS USED.                        
*        NOTE DELDPRGE REQUIRES A DANDX DIRECTORY RECORD                        
*                                                                               
MAINPRGE DS    0H                                                               
         CLI   DTF#,1              WAS FILE SPECIFIED                           
         JL    MAINPRGX            NO, CC NEQ TO KEEP RECORD                    
         CLI   PURGE,YES           ARE WE PURGING                               
         JNE   MAINPRGX            NO, CC NEQ TO KEEP RECORD                    
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
                                                                                
         IF (CLC,THISKFMS,EQ,=X'FFFFFF')  IF EOF DIR. RECORD                    
           MVC THISKFMS,=C'*FF'           MAKE IT PRINTABLE                     
         ENDIF ,                                                                
         BAS RE,PRNTFMS                    PRINT TOTS. FOR LAST F/M/S           
         MVC   P(30),=C'-----          ---------------'                         
         GOTO1 VPRINTER                                                         
         MVC   P(5),=C'TOTAL'                                                   
         EDIT  CTRFMSTO,(15,P+15),COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 VPRINTER                                                         
                                                                                
         IF (TM,COUNTFLG,COUNTYES,O) IF COUNT= CARD IS PRESENT:                 
           IF (OC,CNTARKEY,CNTARKEY,NZ) IF NOT FIRST TIME                       
             MVI   PLCOUNT,1         TELL LDCOUNT TO "COUNT" LAST MAJOR         
             GOTO1 VLDCOUNT,PLCOUNT                                             
           ENDIF ,                                                              
           MVI   PLCOUNT,X'FF'       NOW TELL LDCOUNT TO PRINT TOTALS           
           GOTO1 VLDCOUNT,PLCOUNT                                               
         ENDIF ,                                                                
                                                                                
         SELECT CLI,MODE,EQ                                                     
                                                                                
           WHEN (COPY)             MODE=COPY:                                   
             IF (CLI,E35EXIT,EQ,NO) IF NOT RUNNING AS A DFSORT E35 EXIT         
               CLOSE (IFILE,,OFILE) CLOSE FILES                                 
             ENDIF ,                                                            
                                                                                
           WHEN (COMPARE)          MODE=COMPARE:                                
             XC    OTDATA.DVKEY,OTDATA.DVKEY                                    
             BRAS  RE,COMPARE_FILES    COMPLETE COMPARE                         
             CLOSE (DEMACB,,NEWFILE,,OLDFILE,,DIFFILE)                          
             IF (CLI,E35EXIT,EQ,NO) IF NOT RUNNING AS A DFSORT E35 EXIT         
               CLOSE (IFILE)        CLOSE INPUT                                 
             ENDIF ,                                                            
                                                                                
           OTHRWISE ,                                                           
             J *+2                 INVALID MODE ?!?                             
                                                                                
         ENDSEL ,                                                               
                                                                                
         BAS   RE,TOTALS           PRINT TOTALS                                 
                                                                                
         CLI   E35EXIT,YES         IF NOT RUNNING AS A DFSORT EXIT              
         JNE   XBASE               USE NORMAL RETURN TO MVS.                    
                                                                                
         IF (CLC,RETCODE,GE,=H'16'),OR, IF WE'VE SET A SEVERE RC,               
            (CLI,OPERSTOP,EQ,YES) ,     OR STOPPED BY OPERATOR:                 
           MVC   E35RC,=H'16'       TELL DFSORT TO TERMINATE WITH RC=16         
         ELSE ,                                                                 
           MVC   E35RC,=H'8'        NORMALLY SET DFSORT RC=8 AT EOF             
         ENDIF ,                                                                
                                                                                
         J     E35GOBAK             RETURN TO DFSORT                            
*                                                                               
XBASE    DS    0H                                                               
         XBASE RC=RETCODE,RL=2     RETURN BACK TO MVS                           
                                                                                
         EJECT                                                                  
***********************************************************************         
* CHECK FORMAT OF RECORD AT R3. COUNT IF BAD. FIX MISSING TERMINATOR  *         
*        COUNT ERRORS BY TYPE.                                        *         
*        FIX MISSING TERMINATOR.                                      *         
*        PRINT FIRST THREE BYTES                                      *         
***********************************************************************         
RECCHECK DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
         LH    RE,OUTREC.RECMVSLN  GET MVS LENGTH                               
         AHI   RE,-4               LESS RDW LENGTH                              
         CLM   RE,B'0011',OTDATA.DVLENDDS SHOULD BE >= DDS LENGTH               
         JL    RECCHE1             RDW LENGTH INCORRECT                         
         ICM   RE,B'0011',OTDATA.DVLENDDS GET DDS LENGTH                        
         LA    RE,OTDATA.DVREC(RE) POINT PAST END OF DDS RECORD                 
         LA    R1,OTDATA.DVFRSTEL  POINT TO FIRST ELEMENT                       
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
         LA    RE,OTDATA.DVREC     POINT TO BEGINNING OF DDS REC                
         SR    R1,RE               GET NEW LENGTH                               
         STCM  R1,B'0011',OTDATA.DVLENDDS SET DDS LENGTH                        
         LA    R1,4(,R1)                                                        
         STH   R1,OUTREC.RECMVSLN  SET MVS LENGTH                               
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
         CLC   LRECCHKY,OTDATA.DVREC HAVE WE PRINTED THIS ONE ALREADY?          
         JNE   *+14                NO, SKIP                                     
         MVC   P+12(30),P+11       RESET TO SPACES                              
         J     RECCHXIT                                                         
*                                                                               
         MVC   LRECCHKY,OTDATA.DVREC                                            
         MVC   P(7),=C'KEY(3)='                                                 
         MVC   P+7(3),LRECCHKY                                                  
         GOTOR VPRINTER                                                         
*                                                                               
RECCHXIT DS    0H                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS FOR THE FILE/MEDIA/SOURCE JUST PROCESSED               *         
***********************************************************************         
PRNTFMS  DS    0H                                                               
         IF (CP,CTRFMSTO,EQ,=P'0') SET MIDS IF 1ST FMS (TOTAL STILL 0)          
           MVC   MID1,SPACES                                                    
           MVC   MID1(50),=C'FMS                   #RECORDS            +        
                #KBYTES'                                                        
           MVC   MID2,SPACES                                                    
           MVC   MID2(50),=C'------------------------------------------+        
               --------'                                                        
         ENDIF ,                                                                
         ST    RE,SAVERE                                                        
         MVC   P(L'THISKFMS),THISKFMS     PRINT THIS F/M/S KEY                  
         EDIT  CTRINFMS,(15,P+15),COMMAS=YES,ZERO=NOBLANK                       
         LG    GR1,FMSBYTES               # BYTES                               
         AGHI  GR1,512                    +512                                  
         SRAG  GR1,GR1,10                 /1024                                 
         CVDG  GR1,LONG                   VALUE IS IN DUB1+DUB2                 
         EDIT  (P8,DUB2),(15,P+35),COMMAS=YES,ZERO=NOBLANK                      
         MVC   P+50(2),=C'KB'                                                   
         GOTO1 VPRINTER                                                         
         AP    CTRFMSTO,CTRINFMS          KEEP A RUNNING F/M/S TOTAL            
         ZAP   CTRINFMS,=P'0'             RESET RECORD COUNTER                  
         XC    FMSBYTES,FMSBYTES          AND BYTE COUNTER.                     
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PUT OUTPUT RECORD AT 0(R0)                                                    
* IF RUNNING AS A SORT EXIT, THE PUT IS SKIPPED AND LEFT FOR SORT TO DO         
* IF UNDER SORT, O/P CAN BE SPLIT VIA DFSORT OUTFIL STATEMENTS                  
***********************************************************************         
PUTOUT   NTR1  BASE=(*,PUTOUT_X),LABEL=*                                        
         LR    RF,R0                                                            
         AP    CTROUT,=P'1'        COUNT OFILE RECORDS OUT                      
         XR    RE,RE                                                            
         ICM   RE,B'0011',0(RF)                                                 
         LG    GR1,OTBYTES         ACCUMULATE OUTPUT BYTES                      
         AGFR  GR1,RE                                                           
         STG   GR1,OTBYTES                                                      
         LARL  R1,OFILE                                                         
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
         DROP  RB                                                               
*                                                                               
PUTOUT_X EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* DEAL WITH OPERATOR INPUT                                            *         
***********************************************************************         
*                                                                               
OPSINP   NTR1  BASE=(*,OPSINP_X),LABEL=*                                        
                                                                                
         L     R5,ACOMM            POINT TO OPERATOR COMMS BLOCK                
         LA    R5,COMCIBPT-COMLIST(,R5)                                         
         L     R6,0(R5)                                                         
         USING CIBNEXT,R6                                                       
                                                                                
         CLI   CIBVERB,CIBSTOP     TEST OPERATOR ENTERED STOP COMMAND           
         JNE   OPSINP01                                                         
         MVI   OPERSTOP,YES        YES, MARK STOPPED BY OPERATOR                
         WTO   'STOPPED BY OPERATOR'                                            
         J     OPSINP08                                                         
                                                                                
OPSINP01 CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER MODIFY COMMAND            
         JNE   *+2                 WHAT THE HELL DID HE DO THEN?                
         CLC   CIBDATLN,=H'1'      ONE CHARACTER SHORT FORMS                    
         JL    OPSINP06            IGNORE NULL COMMAND                          
         JH    OPSINP02                                                         
         CLI   CIBDATA,C'?'        ?=STATUS                                     
         JE    OPSINP04                                                         
         J     OPSINP06            ELSE INVALID                                 
OPSINP02 DS    0H                                                               
         CLC   CIBDATLN,=H'6'                                                   
         JNE   OPSINP06                                                         
         CLC   CIBDATA(6),=C'STATUS'                                            
         JNE   OPSINP06            INVALID IF NOT STATUS COMMAND                
                                                                                
OPSINP04 DS    0H                  BUILD STATUS MESSAGE                         
         EDIT  CTRIN,(15,OPCOUNT),COMMAS=YES,ZERO=NOBLANK                       
         LG    GR1,IFBYTES                                                      
         AG    GR1,IDBYTES                                                      
         AGHI  GR1,512             +512                                         
         SRAG  GR1,GR1,10          /1024                                        
         CVDG  GR1,LONG            VALUE IS IN DUB1+DUB2                        
         MVC   OPBYTES,ED_PATTERN1 ###,###,###,###,###                          
         ED    OPBYTES,DUB2                                                     
         MVC   P+2(OPSQLNQ),OPSTATUS COPY TO P AND WRITE FROM THERE             
         GOTOR VSQUASHR,DMCB,P+2,OPSQLNQ                                        
         L     R1,DMCB+4           PICK UP SQUASHED LENGTH                      
         LA    RE,P+2(R1)          POINT TO END                                 
         MVC   0(L'INKEYMAJ,RE),4(R2) MAJOR KEY FROM INPUT                      
         LARL  RF,PRTCHARS                                                      
         TR    0(L'INKEYMAJ,RE),0(RF)                                           
         LA    R1,L'INKEYMAJ(,R1)                                               
         STH   R1,P                SET LENGTH                                   
         WTO   TEXT=P                                                           
         MVC   P,SPACES                                                         
         J     OPSINP08                                                         
                                                                                
OPSINP06 DS    0H                                                               
         WTO   'INVALID OPERATOR COMMAND'                                       
                                                                                
OPSINP08 DS    0H                                                               
         QEDIT ORIGIN=(R5),BLOCK=(R6) RESET OPERATOR COMMANDS                   
         LAM   AR0,ARF,ARZERO                                                   
         DROP  R6                                                               
         J     XIT1                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
OPSINP_X EQU   *                                                                
*                                                                               
***********************************************************************         
* PRINT RECORD TOTALS ETC.                                            *         
***********************************************************************         
*                                                                               
TOTALS NTR1  BASE=(*,TOTALS_X),LABEL=*                                          
                                                                                
       ZAP   LINE,=P'99'                                                        
       MVC   MID1,SPACES                                                        
       MVC   MID1(7),=C'SUMMARY'                                                
       MVC   MID2,SPACES                                                        
       MVC   MID2(7),=C'-------'                                                
                                                                                
       IF (CLI,INDSN,NE,C' ')                                                   
         MVC   P(10),=C'IFILE DSN:'                                             
         MVC   P+11(L'INDSN),INDSN                                              
         GOTOR VPRINTER                                                         
       ENDIF ,                                                                  
                                                                                
       IF (CLI,OUTDSN,NE,C' ')                                                  
         MVC   P(10),=C'OFILE DSN:'                                             
         MVC   P+11(L'OUTDSN),OUTDSN                                            
         GOTOR VPRINTER                                                         
       ENDIF ,                                                                  
                                                                                
       IF (CLI,INDSN,NE,C' '),OR,                                               
          (CLI,OUTDSN,NE,C' ')                                                  
         GOTOR VPRINTER                                                         
       ENDIF ,                                                                  
                                                                                
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
                                                                                
       LARL  R1,LTCFILD          RECORDS DELETED                                
       MVC   P+2(L'LTCFILD),0(R1)                                               
                                                                                
       EDIT  CTRFILD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                          
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
                                                                                
       IF (CLI,DTF#,NE,0)        IF WE CHECKED FOR PEELED:                      
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
                                                                                
       LARL  R1,LTCPASXD         EXT PASSIVES DELETED                           
       MVC   P+2(L'LTCPASXD),0(R1)                                              
       EDIT  CTRPASXD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCDNX           OTHER DIRECTORY RECORDS                        
       MVC   P+2(L'LTCDNX),0(R1)                                                
       EDIT  CTRDNX,(15,P+35),COMMAS=YES,ZERO=NOBLANK                           
       GOTOR VPRINTER                                                           
                                                                                
       LARL  R1,LTCDNXP          ORDINARY PASSIVES DELETED                      
       MVC   P+2(L'LTCDNXP),0(R1)                                               
       EDIT  CTRDNXD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                          
       GOTOR VPRINTER                                                           
                                                                                
       IF (CLI,DTF#,NE,0)        IF WE CHECKED FOR PEELED:                      
         LARL  R1,LTCDIRPL         DIRECTORY RECORDS PEELED                     
         MVC   P+2(L'LTCDIRPL),0(R1)                                            
         EDIT  CTRDIRPL,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
       ENDIF ,                                                                  
       GOTOR VPRINTER                                                           
                                                                                
       IF (CLI,MODE,NE,COMPARE)  IF MODE=COPY                                   
                                                                                
         LARL  R1,LTCOUT           OFILE RECORDS WRITTEN                        
         MVC   P(L'LTCOUT),0(R1)                                                
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
         IF (NZ)                                                                
           CVBG  GRE,LONG                                                       
           XGR   GR0,GR0           CALC. AVERAGE LRECL. GR0/1=BYTES             
           LG    GR1,OTBYTES                                                    
           DLGR  GR0,GRE           DON'T CARE ABOUT ROUNDING                    
           CVDG  GR1,LONG                                                       
         ENDIF ,                                                                
         EDIT  (P8,DUB2),(15,P+35),COMMAS=YES,ZERO=NOBLANK                      
         GOTOR VPRINTER                                                         
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
                                                                                
       IF (CLI,MODE,EQ,COMPARE)  IF COMPARE MODE:                               
                                                                                
         LARL  R1,LTCCMPVR         VSAM RECORDS READ                            
         MVC   P(L'LTCCMPVR),0(R1)                                              
         EDIT  CTRCMPVR,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
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
                                                                                
         LARL  R1,LTCCMPEQ         MATCHED RECORDS INC DATE                     
         MVC   P(L'LTCCMPEQ),0(R1)                                              
         EDIT  CTRCMPEQ,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCCMPDQ         MATCHED RECORDS EXCEPT DATE                  
         MVC   P(L'LTCCMPDQ),0(R1)                                              
         EDIT  CTRCMPDQ,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCCMPBQ         TOTAL MATCHED RECORDS                        
         MVC   P(L'LTCCMPBQ),0(R1)                                              
         ZAP   DUB2,CTRCMPEQ                                                    
         AP    DUB2,CTRCMPDQ                                                    
         EDIT  (P8,DUB2),(15,P+35),COMMAS=YES,ZERO=NOBLANK                      
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
       DROP  RB                                                                 
*                                                                               
TOTALS_X EQU   *                                                                
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
         LARL  R4,VSRDW                                                         
VSMREC   USING RECDSECT,R4         R4=VSAM INPUT AREA (MOVE MODE)               
VSDATA   USING DVREC,VSMREC.RECDATA DEMO VSAM RECORD INPUT                      
*                                                                               
         CLI   OPERSTOP,YES        STOPPED BY OPERATOR?                         
         JE    CPX                 YES, IGNORE REST                             
*                                                                               
         OC    VSMREC.RECRDW,VSMREC.RECRDW HAVE WE READ FIRST VSAM REC          
         JNZ   CP10                YES, SKIP                                    
         XC    VSMKEY,VSMKEY       CLEAR KEY                                    
         BRAS  RE,GETVFI           GET FIRST VSAM RECORD                        
*                                                                               
CP10     DS    0H                                                               
         TM    VSDATA.DVSTAT,DVSDEL TEST VSAM RECORD DELETED?                   
         JZ    CP12                NO, SKIP                                     
         AP    CTRCMPDL,=P'1'      COUNT DELETED VSAM RECORDS IGNORED           
         BRAS  RE,GETVNX           GET NEXT VSAM RECORD                         
         J     CP10                                                             
*                                                                               
CP12     DS    0H                                                               
         BRAS  R8,CPTPRGE          WOULD DELDPRGE PURGE VSAM RECORD?            
         JNE   CP20                NO, SKIP                                     
         AP    CTRCMPPL,=P'1'      COUNT VSAM RECORDS IGNORED AS PURGED         
         BRAS  RE,GETVNX           GET NEXT VSAM RECORD                         
         J     CP10                                                             
*                                                                               
CP20     DS    0H                                                               
         OC    OTDATA.DVKEYMAJ,OTDATA.DVKEYMAJ IS THIS THE EOF CALL             
         JZ    CP22                YES, SEE IF ANY MORE VSAM RECORDS            
         CLI   OTDATA.DVKEY,X'FF'  IS THIS TRAILER                              
         JNE   CP30                NO, SKIP                                     
*                                                                               
CP22     DS    0H                  END OF INPUT OR TRAILER READ                 
         CLI   VSDATA.DVKEY,X'FF'  IS THIS END OF VSAM FILE                     
         JE    CPX                 YES, EXIT                                    
         J     CP40                GO COUNT UNMATCHED VSAM                      
*                                                                               
CP30     DS    0H                                                               
         CLC   OTDATA.DVKEY,VSDATA.DVKEY COMPARE KEYS                           
         JH    CP40                INPUT HIGH, GO COUNT UNMATCHED VSAM          
         JE    CP50                KEYS EQUAL, SEE IF DATA EQUAL                
         AP    CTRCMPUN,=P'1'      COUNT UNMATCHED INPUT                        
         BRAS  RE,PUTOLD           COPY TO OLD FILE                             
         J     CPX                                                              
*                                                                               
CP40     DS    0H                                                               
         AP    CTRCMPUV,=P'1'      COUNT UNMATCHED VSAM                         
         BRAS  RE,PUTNEW           COPY TO NEW FILE                             
         BRAS  RE,GETVNX           GET NEXT VSAM RECORD                         
         J     CP10                RETRY COMPARE                                
*                                                                               
CP50     DS    0H                  KEYS EQUAL                                   
*                                                                               
* WE NOW COMPARE THE INPUT RECORD WITH THE VSAM RECORD, TO SEE IF THE           
* DATA DIFFERS.                                                                 
*                                                                               
* HOWEVER: MANY OF OUR DEMOS CONVERSION PROGRAMS STORE THE CONVERSION           
* EXECUTION *DATE* INTO AN ELEMENT WITHIN THE GENERATED RECORD (E.G.,           
* SEE DSECT "MARELEM" IN DEDEMFILE). THERE REALLY ISN'T ANY GOOD REASON         
* TO CONSIDER THE RECORDS DIFFERENT IF THE *ONLY* DIFFERENCE BETWEEN            
* THEM IS THE CONVERSION RUN DATE.                                              
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
         SR  R2,R2                 R2 = CONVERSION DATE FROM INPUT              
         SR  R5,R5                 R5 = CONVERSION DATE FROM VSAM               
         IF (CLC,=C'RTN',EQ,OTDATA.DVKEYMAJ),OR, LOCAL MONTHLIES                
            (CLC,=C'RON',EQ,OTDATA.DVKEYMAJ),ANDIF, LOCAL DAILIES               
            (CLI,OTDATA.DVFRSTEL,EQ,MARCODEQ),AND, CONFIRM X'01' ELEMS.         
            (CLI,VSDATA.DVFRSTEL,EQ,MARCODEQ)                                   
           USING MARELEM,R1                                                     
           LA    R1,OTDATA.DVFRSTEL R1 = A(1ST ELEM. IN REPLACEMENT)            
           ICM   R2,3,MARDATE      SAVE THE CONVERSION DATE                     
           XC    MARDATE,MARDATE   CLEAR DATE IN I/O AREA                       
           LA    R1,VSDATA.DVFRSTEL R1 = A(1ST ELEM. IN ORIGINAL)               
           ICM   R5,3,MARDATE      SAVE THE CONVERSION DATE                     
           XC    MARDATE,MARDATE   CLEAR DATE IN I/O AREA                       
           DROP  R1                                                             
         ENDIF ,                                                                
*                                                                               
         LR    R0,R3               COMPARE INPUT                                
         LH    R1,0(R3)                                                         
         LR    RE,R4               WITH VSAM                                    
         LH    RF,0(R4)                                                         
         CLCL  R0,RE                                                            
         JNE   CP52                THE RECORDS DIFFER                           
*                                                                               
*                                  RECORDS MATCH: CONSIDER THEM EQUAL           
         IF  (CR,R2,EQ,R5)         BUT IF THE CONVERSION DATES MATCH:           
           AP  CTRCMPEQ,=P'1'      COUNT KEYS AND DATA MATCH                    
         ELSE ,                    O/W:                                         
           AP  CTRCMPDQ,=P'1'      COUNT MATCH EXCEPT FOR DATE                  
         ENDIF ,                                                                
         J     CP58                                                             
*                                                                               
CP52     DS    0H                                                               
         USING MARELEM,R1                                                       
         IF (CHI,R2,NE,0)          IF WE SAVED DATES RESTORE THEM               
           LA    R1,OTDATA.DVFRSTEL  R1 = A(1ST ELEM. IN REPLACEMENT)           
           STCM  R2,3,MARDATE        RESTORE THE CONVERSION DATE                
           LA    R1,VSDATA.DVFRSTEL  R1 = A(1ST ELEM. IN ORIGINAL)              
           STCM  R5,3,MARDATE        RESTORE THE CONVERSION DATE                
         ENDIF ,                                                                
         DROP  R1                                                               
*                                                                               
         AP    CTRCMPNE,=P'1'      COUNT KEYS MATCH BUT NOT DATA                
         BRAS  RE,PUTDIF           COPY BOTH TO DIF FILE                        
*                                                                               
CP58     DS    0H                                                               
         BRAS  RE,GETVNX           GET NEXT VSAM RECORD                         
*                                                                               
CPX      DS    0H                                                               
         J     XIT1                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*        TEST IF INPUT RECORD WOULD HAVE BEEN PURGED BY DELDPRGE AND            
*        SET CC EQUAL IF IT WOULD.                                              
*        DELDPRGE ONLY CALLED ON CHANGE OF MAJOR KEY. IF MAJOR KEY IS           
*        SAME AS LAST CALL, RESULT OF LAST CALL IS USED.                        
*        NOTE DELDPRGE REQUIRES A DANDX DIRECTORY RECORD                        
*                                                                               
CPTPRGE  DS    0H                                                               
         CLI   DTF#,1              WAS FILE SPECIFIED                           
         BLR   R8                  NO, CC NEQ TO KEEP RECORD                    
         CLI   PURGE,YES           ARE WE PURGING                               
         BNER  R8                  NO, CC NEQ TO KEEP RECORD                    
         IF (CLC,PRGLSTMJ,NE,VSDATA.DVKEY) IF MAJOR KEY HAS CHANGED:            
           MVC   PRGARKEY,VSDATA.DVKEY COPY MAJOR KEY                           
           MVC   PRGLSTMJ,PRGARKEY   REMEMBER LAST MAJOR KEY WE CHECKED         
           MVC   PRGARSTA,VSDATA.DVSTAT COPY STATUS                             
           XC    PRGARDA,PRGARDA     CLEAR D/A                                  
           MVI   PURGEFLG,0          CLEAR PURGE FLAG                           
           GOTOR VLDPURGE,PLPURGE    CALL DELDPRGE                              
         ENDIF ,                                                                
         CLI   PURGEFLG,X'FF'      RECORD TO BE PURGED?                         
         BR    R8                  CC EQ IF SO, ELSE CC NE                      
         EJECT                                                                  
***********************************************************************         
* VSAM READ ROUTINES. KEY IN VSMKEY, VSAM RECORD RETURNED AT 0(R4)    *         
***********************************************************************         
         USING IFGRPL,R2                                                        
*                                                                               
GETVFI   NTR1  ,                   GET FIRST RECORD FOR MAJOR KEY               
*                                                                               
         LARL  R2,DEMRPL                                                        
         NI    RPLOPT1,255-RPLSEQ                                               
         OI    RPLOPT1,RPLDIR+RPLKGE                                            
         J     GETVSM                                                           
*                                                                               
GETVNX   NTR1  ,                   GET NEXT VSAM RECORD                         
*                                                                               
         LARL  R2,DEMRPL                                                        
         NI    RPLOPT1,255-(RPLDIR+RPLKGE)                                      
         OI    RPLOPT1,RPLSEQ      SET OPTCD=SEQ                                
*                                                                               
GETVSM   DS    0H                                                               
         GET   RPL=(R2)            READ VSAM RECORD                             
         LTR   RF,RF                                                            
         JNZ   GETVSMER            VSAM ERROR BEFORE WAIT                       
         CHECK RPL=(R2)                                                         
         OC    RPLFDBK,RPLFDBK                                                  
         JNZ   GETVSMER            VSAM ERROR                                   
         L     R1,RPLRLEN          RETURNED RECORD LENGTH                       
         LA    R1,4(,R1)           ADD 4 FOR RDW LENGTH                         
         SLL   R1,16               AND SHIFT TO CORRECT POSITION                
         ST    R1,VSMREC.RECRDW                                                 
*                                                                               
         AP    CTRCMPVR,=P'1'      COUNT VSAM RECORDS READ                      
*                                                                               
         J     XIT1                                                             
*                                                                               
GETVSMER CLC   RPLFDBK,=X'080004'  TEST EOF                                     
         JNE   *+2                 NO, UNACCEPTABLE VSAM ERROR                  
         MVC   VSMREC.RECRDW(DVPRLNQ+4),VSEOFREC                                
         J     XIT1                                                             
*                                                                               
VSEOFREC DC    AL2(DVPRLNQ+4,0),21X'FF',AL2(DVPRLNQ),2X'00'                     
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
         CLI   INPARM,0            INPUT= INPUT?                                
         JNE   *+8                 YES                                          
         MVI   INPARM,VSAM         DEFAULT INPUT=VSAM                           
*                                                                               
         CLI   PURGE,0             PURGE= INPUT?                                
         JNE   *+8                 YES                                          
         MVI   PURGE,YES           DEFAULT PURGE=YESM                           
*                                                                               
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
         DC    AL1(09),AL3(PARINPT),CL20'INRECFORM=' SYNONYM FOR INPUT=         
         DC    AL1(07),AL3(PARMAXC),CL20'MAXCOMP='                              
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
DEVSUT   CSECT                                                                  
*                                                                               
* PARAMETER VALIDATION ROUTINES R1=A(PARAMETER VALUE)                           
*                                                                               
PARMODE  DS    0H                                                               
         CLI   MODE,0              MODE=                                        
         JNE   VALPARDP                                                         
*                                                                               
         MVI   MODE,COPY           POSIT MODE=COPY                              
         CLC   =C'COPY ',0(R1)     MODE=COPY                                    
         BER   RE                                                               
         CLC   =C'LOAD ',0(R1)     MODE=LOAD IS A SYNONYM FOR COPY              
         BER   RE                                                               
*                                                                               
         MVI   MODE,COMPARE        POSIT MODE=COMPARE                           
         CLC   =C'COMPARE ',0(R1)  MODE=COMPARE                                 
         BER   RE                                                               
*                                                                               
         MVI   MODE,X'FF'          INVALID MODE                                 
         LARL  R1,LTPMLC                                                        
         MVC   P+4(L'LTPMLC),0(R1)                                              
         J     VALPARER            (RETURNS TO RE)                              
*                                                                               
PARINPT  DS    0H                                                               
         CLI   INPARM,0            INPUT=                                       
         JNE   VALPARDP                                                         
         MVC   INPARM,0(R1)                                                     
         CLI   INPARM,DANDX        INPUT=D(ANDX)                                
         BER   RE                                                               
         CLI   INPARM,VSAM         INPUT=V(SAM)                                 
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
         CLC   =C'DEMA ',0(R1)                                                  
         BER   RE                                                               
         CLC   =C'DEMVSMA ',0(R1)                                               
         BER   RE                                                               
         MVI   DTF#,DEMVSMN#                                                    
         CLC   =C'DEMN ',0(R1)                                                  
         BER   RE                                                               
         CLC   =C'DEMVSMN ',0(R1)                                               
         BER   RE                                                               
         MVI   DTF#,DEMVSMR#                                                    
         CLC   =C'DEMR ',0(R1)                                                  
         BER   RE                                                               
         CLC   =C'DEMVSMR ',0(R1)                                               
         BER   RE                                                               
         MVI   DTF#,NTIVSM#                                                     
         CLC   =C'NTI ',0(R1)                                                   
         BER   RE                                                               
         CLC   =C'NTIVSM ',0(R1)                                                
         BER   RE                                                               
         MVI   DTF#,PAVVSM#                                                     
         CLC   =C'PAV ',0(R1)                                                   
         BER   RE                                                               
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
ATIOT    DS    A                                                                
AASID    DS    A                                                                
AASCB    DS    A                                                                
ACOMM    DS    A                                                                
AOPERECB DS    A                   A(ECB OF OPERATOR INTERRUPT)                 
*                                                                               
STEPNAME DS    CL8                                                              
JOBNAME  DS    CL8                                                              
JOBID    DS    CL8                                                              
*                                                                               
PL16     DS    0PL16                                                            
PL16HIGH DS    PL8                                                              
PL16LOW  DS    PL8                                                              
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
RETCODE  DC    H'0'                MVS RETURN CODE                              
*                                                                               
VCPRINT  DC    V(CPRINT)                                                        
VPRINTER DC    V(PRINTER)                                                       
VPRNTER  DC    V(PRNTER)           PRINT DCB WITHIN DDPRINT                     
VCARDS   DC    V(CARDS)                                                         
VLDPURGE DC    V(LDPURGE)                                                       
VLDCOUNT DC    V(LDCOUNT)                                                       
VSQUASHR DC    V(SQUASHER)                                                      
*                                                                               
ARZERO   DC    4A(0)                                                            
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
TESTFIL# EQU   31                  ALL TEST FILES ARE LOGICAL FILE #31          
DANDXFL# EQU   B'00111111'         DANDX FILE NUMBER STATUS BITS                
PASSIVE# EQU   X'FF'               SEQUENCE # X'FF' = PASSIVE KEY               
*                                                                               
WORK     DS    CL17                (NOTE: USED BY EDIT MACRO)                   
*                                                                               
OPERSTOP DS    C'N'                OPERATOR ENTERED STOP                        
*                                                                               
OPSTATUS DC    C'STATUS:'          LENGTH SET AFTER SQUASHER                    
OPCOUNT  DC    CL15' '                                                          
         DC    C' RECORDS READ,'                                                
OPBYTES  DC    CL20' '                                                          
         DC    C' KBYTES READ,'                                                 
         DC    C' LAST MAJ KEY:'                                                
OPSQLNQ  EQU   *-OPSTATUS          LENGTH TO SQUASH                             
*                                                                               
PRGLSTMJ DS    XL(L'DVKEYMAJ)'00'  LAST MAJOR KEY MARKED FOR PURGING            
         DS    0D                                                               
         DC    C'*PRGAREA'                                                      
PRGAREA  DS    0C                  USED FOR DELDPRGE (DIR REC)                  
PRGARKEY DS    CL(L'DVKEYMAJ)      KEY (= MAJOR KEY)                            
PRGARSTA DS    CL(L'DVSTAT)        STATUS                                       
PRGARDA  DS    XL4                 DISK ADDRESS (SET TO NULL)                   
*                                                                               
         DC    C'*CNTAREA'                                                      
CNTAREA  DS    0C                  USED FOR DELDDCNT (DIR REC)                  
CNTARKEY DS    XL(L'DVKEYMAJ)'00'  KEY (= MAJOR KEY)                            
CNTARSTA DS    XL(L'DVSTAT)'00'    STATUS                                       
CNTARDA  DC    XL4'00'             DISK ADDRESS (SET TO NULL)                   
*                                                                               
         DS    0D                                                               
         DC    C'*VSMKEY*'                                                      
VSMKEY   DS    0XL21               KEY FOR VSAM READS                           
VSMKMAJ  DC    XL18'00'            MAJOR KEY FOR MERGE ROUTINE                  
         DC    XL3'00'             PLUS REST OF VSAM KEY                        
*                                                                               
*----------------------------------------------------------------------         
RUNVALS  DS    0X                  ** RUN TIME PARAMETERS **                    
*                                                                               
MODE     DS    C                   L=COPY/LOAD,C=COMPARE                        
COPY     EQU   C'C'                COPY MODE                                    
COMPARE  EQU   C'P'                COMPARE MODE                                 
*                                                                               
MAXCOMPF DS    C                   MAXCOMP= CARD WAS READ                       
PURGE    DS    C                   Y IF PURGE=Y ELSE N OR NULL                  
TESTDATA DS    C                   Y IF TESTDATA=Y ELSE N OR NULL               
*                                                                               
INPARM   DS    C                   V IF INPUT=VSAM ELSE D FOR DANDX             
VSAM     EQU   C'V'                                                             
DANDX    EQU   C'D'                                                             
*                                                                               
COUNTFLG DS    X                                                                
COUNTYES EQU   X'02'               COUNT=YES                                    
COUNTBKS EQU   X'04'               COUNT=BOOKS                                  
COUNTVSM EQU   X'10'               TELL DELDDCNT THAT WE ARE VSAM               
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
RUNVALL  EQU   *-RUNVALS                                                        
*----------------------------------------------------------------------         
*                                                                               
ABEND    DS    C                   (FORCE TYPE TO "C" FOR IDF)                  
         ORG   ABEND                                                            
         DC    AL1(NO)                                                          
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
*                                                                               
INDSN    DC    CL44' '                                                          
OUTDSN   DC    CL44' '                                                          
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
*        OUTPUT COUNTERS, COPY MODE:                                            
CTROUT   DC    PL8'0'              ALL RECORDS WRITTEN OFILE                    
*        OUTPUT COUNTERS, COMPARE MODE:                                         
CTRNEWF  DC    PL8'0'              'NEW' (VSAM) FILE RECORDS                    
CTROLDF  DC    PL8'0'              'OLD' (DANDX) FILE RECORDS                   
CTRDIFF  DC    PL8'0'              'DIFF' FILE RECORDS (DATA DIFFERS)           
*        RECORD CHECK WARNINGS AND ERRORS:                                      
CTRRCE01 DC    PL8'0'              RDW LENGTH TOO SMALL                         
CTRRCE02 DC    PL8'0'              DDS LENGTH TOO SMALL                         
CTRRCE03 DC    PL8'0'              ZERO ELEMENT LENGTH                          
CTRRCW01 DC    PL8'0'              MISSING EOR (NO NULL TERMINATOR)             
CTRRCW02 DC    PL8'0'              EOR TOO SOON                                 
*        COMPARE PROCESS COUNTERS:                                              
CTRCMPVR DC    PL8'0'              VSAM RECORDS READ                            
CTRCMPDL DC    PL8'0'              DELETED VSAM RECORDS IGNORED                 
CTRCMPPL DC    PL8'0'              VSAM RECORDS IGNORED AS S/B PEELED           
CTRCMPEQ DC    PL8'0'              MATCHED RECORDS INCLUDING DATE               
CTRCMPDQ DC    PL8'0'              MATCHED RECORDS EXCEPT FOR DATE              
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
         DS    0A                                                               
DSNXTRCT DC    X'87',AL3(0)        DCBRECFM,DCBEXLSA                            
         EJECT                                                                  
*              DCBS, ETC.                                                       
*                                                                               
* INPUT FILE:                                                                   
IFILE    DCB   DDNAME=IFILE,DSORG=PS,MACRF=(GL),RECFM=VB,EODAD=EOFIN            
*                                                                               
* OUTPUT FILE (FOR INPUT TO IDCAMS REPRO WITH REPLACE):                         
OFILE    DCB   DDNAME=OFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192             
*                                                                               
* USED WHEN MODE=COMPARE:                                                       
NEWFILE  DCB   DDNAME=NEWFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192           
OLDFILE  DCB   DDNAME=OLDFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192           
DIFFILE  DCB   DDNAME=DIFFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192           
*                                                                               
DEMACB   ACB   AM=VSAM,DDNAME=DEMVSM,MACRF=(KEY,DIR,SKP,IN),           X        
               BUFNI=10,BUFND=16,RMODE31=ALL                                    
DEMRPL   RPL   ACB=DEMACB,AM=VSAM,                                     X        
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
*        OUTPUT COUNTERS, COPY MODE:                                            
LTCOUT   DC    C'OFILE RECORDS WRITTEN',0H'0'                                   
LTCOUTB  DC    C'OFILE BYTES WRITTEN, INC RDW',0H'0'   MAX 30 BYTES             
LTCMAXLN DC    C'MAX O/P RECORD LENGTH, INC RDW',0H'0'                          
LTCAVGLN DC    C'AVG O/P RECORD LENGTH, INC RDW',0H'0'                          
*        OUTPUT COUNTERS COMPARE MODE:                                          
LTCNEWF  DC    C'NEWFILE RECORDS WRITTEN',0H'0'                                 
LTCOLDF  DC    C'OLDFILE RECORDS WRITTEN',0H'0'                                 
LTCDIFF  DC    C'DIFFILE RECORDS WRITTEN',0H'0'                                 
*        RECORD CHECK WARNINGS AND ERRORS:                                      
LTCRCE01 DC    C'RDW LENGTH TOO SMALL',0H'0'                                    
LTCRCE02 DC    C'DDS LENGTH TOO SMALL',0H'0'                                    
LTCRCE03 DC    C'ELEMENT LENGTH ZERO',0H'0'                                     
LTCRCW01 DC    C'MISSING EOR, ADDED',0H'0'                                      
LTCRCW02 DC    C'EOR BEFORE RECORD END, FIXED',0H'0'                            
*        COMPARE PROCESS COUNTERS:                                              
LTCCMPVR DC    C'VSAM RECORDS READ',0H'0'                                       
LTCCMPDL DC    C'DELETED VSAM RECORDS IGNORED',0H'0'                            
LTCCMPPL DC    C'VSAM RECORDS IGNORED AS S/B PEELED',0H'0'                      
LTCCMPEQ DC    C'MATCHED RECORDS INCL. CONVERSN DATE',0H'0'                     
LTCCMPDQ DC    C'MATCHED RECORDS EXCL. CONVERSN DATE',0H'0'                     
LTCCMPBQ DC    C'MATCHED RECORDS TOTAL',0H'0'                                   
LTCCMPUN DC    C'UNMATCHED INPUT RECORDS',0H'0'                                 
LTCCMPUV DC    C'UNMATCHED VSAM RECORDS',0H'0'                                  
LTCCMPNE DC    C'MATCHED KEY, DATA DIFFERENT',0H'0'                             
*                                                                               
*              ERROR AND WARNING MESSAGES                                       
*                                                                               
LTCMBZ   DC    C'*ERROR* - SHOULD NEVER BE NON-ZERO',0H'0'                      
LTGTDF1  DC    C'*ERROR* - TEST DATA FOUND. TERMINATING',0H'0'                  
LTGTDF2  DC    C'USE TESTDATA=Y PARAMETER IF REQUIRED',0H'0'                    
*                                                                               
LTPMCM   DC    C'MODE= CARD MISSING',0H'0'                                      
LTPMRF   DC    C'MODE=COMPARE REQUIRES FILE=',0H'0'                             
LTPMVC   DC    C'MAXCOMP= VALID ONLY WITH MODE=COMPARE',0H'0'                   
LTPICC   DC    C'INVALID CONTROL CARD',0H'0'                                    
LTPDPC   DC    C'DUPLICATE CARD',0H'0'                                          
LTPRAB   DC    C'RUN ABORTED DUE TO ABOVE ERRORS',0H'0'                         
LTPMLC   DC    C'MODE= MUST BE COPY, LOAD OR COMPARE',0H'0'                     
*                                                                               
*              TRANSLATE TABLE FOR PRINTABLE CHARS                              
*                                                                               
PRTCHARS DC    64C'.'              EVERYTHING BELOW BLANK IS A DOT.             
         DC    (256-64)AL1(*-PRTCHARS) REST IS AS IS                            
         EJECT                                                                  
***********************************************************************         
* ENTRY FROM AND RETURN TO DFSORT WHEN RUNNING AS A DFSORT E35 EXIT   *         
* FIRST TEST IF WE ARE ENTERED FROM DFSORT. IF NOT, NORMAL CODE       *         
***********************************************************************         
         USING E35,RF                                                           
E35      SAVE  (14,12),,DEVSUT     ENTRY GOOD FOR MVS AS WELL AS DFSORT         
*                                                                               
         CLC   1(3,RD),=C'SM1'     DFSORT ALWAYS CALLS WITH THIS                
         JNE   DEVSUT              USUAL NBASE IF NOT FROM DFSORT               
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
         LARL  RB,DEVSUT           USE THE SAME REGISTERS AS OUR NBASE          
         LARL  R7,COMMON                                                        
         MVIY  E35EXIT,YES                                                      
         SR    R0,R0                                                            
         STHRL R0,E35RC            PRESET RC=0                                  
         LARL  R3,OUTAREA          MUST RESET R4/R9 WHEN NOT 1ST TIME           
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
*        RECORD DSECTS                                                          
*                                                                               
*        THIS DSECT COVERS THE DEMO DANDX FORMAT                                
*        NOTE THAT THE MAJOR KEY OCCUPIES THE FIRST 18 BYTES OF A DANDX         
*        RECORD AND ALSO THE FIRST 18 BYTES OF A VSAM RECORD SO THIS            
*        DSECT CAN ALSO BE USED FOR VSAM UP TO THE END OF INKEYMAJ.             
*                                                                               
INREC    DSECT ,                   DANDX DEMO RECORD                            
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
RECDSECT DSECT ,                   VARIABLE RECORD DSECT                        
RECMVSLN DS    0XL2                MVS RECORD LENGTH                            
RECRDW   DS    XL4                 MVS RDW                                      
RECDATA  DS    0C                  RECORD DATA (KEY)                            
*                                                                               
       ++INCLUDE DEVSMFILE                                                      
*                                                                               
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         IFGACB AM=VSAM                                                         
         IFGRPL AM=VSAM                                                         
* IEZCIB                                                                        
* IHAASCB                                                                       
* IHAASSB                                                                       
* IAZJSAB                                                                       
         PRINT OFF                                                              
         DSECT ,                                                                
         IEZCIB                                                                 
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         IEZCOM                                                                 
         PRINT ON                                                               
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
**PAN#1  DC    CL21'001DEVSUTIL  12/04/20'                                      
         END                                                                    
