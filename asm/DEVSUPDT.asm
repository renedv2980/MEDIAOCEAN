*          DATA SET DEVSUPDT   AT LEVEL 001 AS OF 01/06/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEVSUPDA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE REGSAVE                                                                
*                                                                               
         TITLE 'PREPARE UPDATES TO VSAM FORMAT DEMO FILES'                      
***********************************************************************         
*                                                                               
* PROGRAM TAKES AN UPDATE FILE (THAT WOULD HAVE GONE INTO A DELD/PVLD           
* UPDATE STEP) AND PERFORMS THE FOLLOWING ACTIONS:                              
* 1. REFORMATS THE 'DANDX' UPDATE RECORDS TO THE VSAM FORMAT.                   
* 2. COMPARES THE UPDATE FILE TO THE VSAM FILE AND GENERATES A FILE OF          
*    UPDATE DATA THAT WILL BE MERGED INTO THE VSAM FILE USING IDCAMS.           
* 3. CREATES A BACKOUT FILE WHICH, IF MERGED WITH THE UPDATED FILE              
*    USING THE SAME IDCAMS PROCESS, WILL BACK OUT THE UPDATE.                   
*                                                                               
***********************************************************************         
*                                                                               
* PROGRAM SUPPORTS THE DEMVSMN FILE SPLIT. IN THAT CASE, THE FILE OF            
* UPDATE DATA IN POINT 2 ABOVE, AND THE BACKOUT FILE IN POINT 3 BOTH            
* BECOME FOUR FILES, ONE FOR EACH OF THE FILES THAT MAKE UP DEMVSMN.            
*                                                                               
* MAIN REGISTER'S THROUGHOUT:                                                   
* R2 - INPUT  RECORD (LOCATE MODE)                                              
* R3 - OUTPUT RECORD IN OUTAREA (MOVE MODE)                                     
* R4 - VSAM DEMO INPUT RECORD IN VSRDW/VSAREA (MOVE MODE)                       
* R7 - COMMON STORAGE                                                           
* R9 - DPRINT                                                                   
*                                                                               
***********************************************************************         
*                                                                               
DEVSUPD  CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,**DVUP**,WORK=V(REGSAVE)                                       
         LARL  R7,COMMON                                                        
         USING COMMON,R7                                                        
         L     R9,VCPRINT                                                       
         USING DPRINT,R9           R9=A(PRINT CSECT)                            
*                                                                               
         USING INREC,R2            R2=DANDX UPDATE INPUT (LOCATE MODE)          
*                                                                               
         LARL  R3,OUTAREA                                                       
OUTREC   USING RECDSECT,R3         R3=OUTPUT AREA (MOVE MODE)                   
OTDATA   USING DVREC,OUTREC.RECDATA DEMO VSAM RECORD OUTPUT                     
*                                                                               
         LARL  R4,VSRDW                                                         
VSMREC   USING RECDSECT,R4         R4=VSAM INPUT AREA (MOVE MODE)               
VSDATA   USING DVREC,VSMREC.RECDATA DEMO VSAM RECORD INPUT                      
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     DS    0H                                                               
         EXTRACT ATIOT,'S',FIELDS=(TIOT)                                        
         L     RF,ATIOT                                                         
         MVC   JOBNAME,0(RF)       GET MY JOBNAME                               
         MVC   STEPNAME,8(RF)      GET MY STEPNAME                              
                                                                                
         EXTRACT AASID,'S',FIELDS=(ASID)                                        
         L     R5,AASID            MVS JOB ACCOUNTING INFORMATION               
         LOCASCB ASID=(5)          PUTS A(ASCB) INTO R1                         
         ST    R1,AASCB            SAVE A(ASCB)                                 
*                                                                               
         L     R1,ASCBASSB-ASCB(,R1) R1=A(ASSB)                                 
         SAM31 ,                                                                
         L     R1,ASSBJSAB-ASSB(,R1) R1=A(JSAB)                                 
         MVC   JOBID,JSABJBID-JSAB(R1) GET JOBID (E.G. JOB12345)                
         SAM24 ,                                                                
*                                                                               
         STCM  R3,7,DSNXTRCT+1     EXTRACT INPUT DSN                            
         LA    R1,DSNXTRCT         USING OUTAREA AT R3 FOR WORK AREA            
         LARL  RF,IFILE                                                         
         STCM  R1,7,DCBEXLSA-IHADCB(RF) SET EXLST ADDRESS IN DCB                
         RDJFCB ((RF))                                                          
         MVC   INDSN,0(R3)                                                      
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
         MVC   TITLE+22(17),=C'VSAM DEMO UPDATE.'                               
*                                                                               
         BRAS  RE,VALPARMS         VALIDATE PARAMETER CARD(S)                   
*                                                                               
*        OPEN ALL FILES                                                         
*                                                                               
*        NOTE DEMVSMN IS THE ONLY FILE WE SUPPORT AS A SPLIT FILE AND           
*        THE SPLIT DEMVSMN FILE HAS FOUR SETS OF FILES                          
*                                                                               
*        FOR SPLIT DEMVSMN, WE MUST FIND THE FIRST 'RTN' MAJOR GROUP            
*        ON FILE 3 (FILE2/3 SPLIT) AND FILE 4 (FILE3/4 SPLIT).                  
*                                                                               
         OPEN  (IFILE,INPUT,OFILE,OUTPUT)                                       
         OPEN  (BKFILE,OUTPUT)     OPEN BACKOUT FILES                           
         OPEN  (DEMACB)            OPEN VSAM FILE                               
         LTR   RF,RF               RF HOLDS ERROR RETURN CODE                   
         JNZ   *+2                                                              
*                                                                               
         IF (CLI,SPLIT,EQ,YES)     SPLIT FILE?                                  
           OPEN  (OFIL2,OUTPUT,OFIL3,OUTPUT,OFIL4,OUTPUT)                       
           OPEN  (BKFIL2,OUTPUT,BKFIL3,OUTPUT,BKFIL4,OUTPUT)                    
           OPEN  (DEMAC2,,DEMAC3,,DEMAC4)                                       
           LTR   RF,RF                                                          
           JNZ   *+2                                                            
                                                                                
           XC    VSMKEY,VSMKEY     SET KEY TO READ FIRST MAJOR GROUP            
           MVC   VSMKMIN,=X'FFFFFF' SKIP OVER ANY POSSIBLE HEADER               
           MVI   VFILEID,SPLIT3    FILE 3                                       
           BRAS  RE,GETVSHI                                                     
           MVC   F3MAJ1ST,VSDATA.DVKEY FIRST MAJOR KEY ON FILE 3                
           MVI   VFILEID,SPLIT4    FILE 4                                       
           BRAS  RE,GETVSHI                                                     
           MVC   F4MAJ1ST,VSDATA.DVKEY FIRST MAJOR KEY ON FILE 4                
         ENDIF ,                                                                
*                                                                               
INITX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* MAIN LOOP                                                           *         
***********************************************************************         
MAININ   DS    0H                                                               
         LARL  R1,IFILE                                                         
         GET   (1)                 GET NEXT DANDX FORMAT INPUT RECORD           
         LR    R2,R1               (USING INREC)                                
*                                                                               
         AP    CTRIN,=P'1'         COUNT RECORDS IN                             
*                                                                               
         OC    INKEYMAJ,INKEYMAJ   TEST FOR NULL MAJOR KEY                      
         JZ    *+2                 ABSOLUTELY SHOULD NOT HAPPEN                 
*                                                                               
         IF (CLC,THISKFMS,NE,INKEYFMS) WHEN F/M/S CHANGES                       
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
*        CONVERT FROM DANDX TO VSAM FORMAT                                      
*        NOTE R0 = RECORD LENGTH (TO ACCUMULATE IFBYTES OR IDBYTES)             
*                                                                               
         CLC   INLENMVS,=Y(INPLENQ) IS THIS A DIRECTORY RECORD?                 
         JE    MAINI40             YES, WHOLE DIFFERENT REFORMAT                
*                                                                               
*        'NORMAL' DEMO RECORD (I.E. NOT A DIRECTORY RECORD)                     
*                                                                               
         AP    CTRFIL,=P'1'        COUNT 'FILE' (DA) RECORDS                    
*                                                                               
         LG    GRF,IFBYTES         ACCUMULATE 'FILE' BYTES                      
         AGFR  GRF,R0                                                           
         STG   GRF,IFBYTES                                                      
*                                                                               
         TM    INSTAT,DVSDEL       IS IT DELETED?                               
         JZ    MAINI10             NO, SKIP                                     
         AP    CTRFILD,=P'1'       COUNT DELETED FILE RECORD AND IGNORE         
         J     MAINNEXT            NOT SUPPORTED FOR UPDATE PROCESS             
*                                                                               
MAINI10  DS    0H                                                               
         TM    INSTAT,DVSEXTP      TEST EXTENDED FLAG (SHOULD BE ZERO)          
         JZ    *+10                                                             
         AP    CTRFILX,=P'1'       COUNT (INCORRECT) EXTENDED FLAG              
*                                                                               
         AP    CTRMINAV,=P'1'      COUNT TOTAL MINOR KEYS FOR AVG CALC          
         CLC   THISKMAJ,INKEYMAJ   SAME MAJOR KEY AS THE LAST RECORD            
         JNE   MAINI12             NO, SKIP                                     
         AP    IMINORS,=P'1'       COUNT MINORS THIS MAJOR                      
         CLC   THISKMIN,INKEYMIN   SAME MINOR KEY AS LAST RECORD?               
         JNE   MAINI14             NO,                                          
         J     MAINI16             YES, SKIP IT'S A DUPLICATE                   
*                                                                               
MAINI12  DS    0H                                                               
         AP    CTRMAJKY,=P'1'      COUNT MAJOR KEYS                             
*                                                                               
         CP    CTRMINMX,IMINORS    WAS LAST MAXIMUM MINORS SO FAR               
         JNL   *+10                                                             
         MVC   CTRMINMX,IMINORS    YES, SAVE IT                                 
         ZAP   IMINORS,=P'1'                                                    
*                                                                               
MAINI14  DS    0H                                                               
         CP    CTRDUPMX,IDUPKEY    WAS LAST MAXIMUM DUPS SO FAR                 
         JNL   *+10                                                             
         MVC   CTRDUPMX,IDUPKEY    YES, SAVE IT                                 
         ZAP   IDUPKEY,=P'0'                                                    
         MVC   THISKEY,INKEY       SAVE NEW KEY                                 
         MVI   SEQNO,0             RESET SEQUENCE NUMBER                        
         J     MAINI20                                                          
*                                                                               
MAINI16  DS    0H                                                               
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
MAINI20  DS    0H                                                               
         XR    RE,RE                                                            
         ICM   RE,B'0011',INLENMVS MVS LENGTH                                   
         LR    R1,RE               SAVE FOR MVCL                                
         LA    RE,1(,RE)           INCREASE MVS LENGTH BY 1                     
         SLL   RE,16                                                            
         ST    RE,OUTREC.RECRDW    SET O/P MVS LEN                              
         MVC   OTDATA.DVKEY(L'INKEY),INKEY COPY MAJOR/MINOR KEY                 
         MVC   OTDATA.DVKEYSEQ,SEQNO INSERT SEQUENCE NUMBER                     
         ICM   RE,B'0011',INLENDDS DDS LENGTH (HI 2 BYTES IRRELEVANT)           
         LA    RE,1(,RE)           INCREASE DDS LENGTH BY 1                     
         STCM  RE,B'0011',OTDATA.DVLENDDS SET O/P DDS LEN                       
         LA    R0,OTDATA.DVKEY+L'DVKEY+L'DVLENDDS 1ST BYTE FOR REMAIN           
         AHI   R1,-(L'INRDW+L'INKEY+L'INLENDDS) LESS LEN DONE SO FAR            
         LA    RE,INKEY+L'INKEY+L'INLENDDS FIRST BYTE TO MOVE REST FROM         
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE REST OF RECORD                          
         JO    *+2                 DESTRUCTIVE MOVE!                            
         NI    OTDATA.DVSTAT,X'FF'-(DVSEXTP+DANDXFL#) 0 UNWANTED STAT           
         J     MAINOUT             KEEP RECORD                                  
*                                                                               
*        DIRECTORY RECORD ('PASSIVE' OR DELETED POINTER)                        
*                                                                               
MAINI40  DS    0H                                                               
         AP    CTRDIR,=P'1'        COUNT DIRECTORY RECORDS                      
*                                                                               
         LG    GRF,IDBYTES         ACCUMULATE DIRECTORY BYTES                   
         AGFR  GRF,R0                                                           
         STG   GRF,IDBYTES                                                      
*                                                                               
         TM    INPSTAT,DVSEXTP     TEST EXTENDED                                
         JO    MAINI42                                                          
*                                                                               
         AP    CTRDNX,=P'1'        NO, COUNT NON-EXTENDED PASSIVES              
         TM    INPSTAT,DVSDEL      IS IT DELETED?                               
         JZ    MAINI44                                                          
         AP    CTRDNXD,=P'1'       YES, COUNT NON-EXTENDED AND DELETED          
         J     MAINI44                                                          
*                                                                               
MAINI42  DS    0H                                                               
         AP    CTRPASX,=P'1'       COUNT EXTENDED PASSIVES                      
         TM    INPSTAT,DVSDEL      IS IT DELETED                                
         JZ    MAINI44                                                          
         AP    CTRPASXD,=P'1'      YES, COUNT EXTENDED AND DELETED              
*                                                                               
MAINI44  DS    0H                                                               
         XC    THISKMIN,THISKMIN   CLEAR MINOR PART                             
         CLC   THISKMAJ,INKEYMAJ   SAME KEY AS THE LAST RECORD                  
         JE    *+2                 SHOULD NOT HAPPEN FOR DIRECTORY              
*                                                                               
         MVC   THISKMAJ,INKEYMAJ                                                
*                                                                               
         MVC   OTDATA.DVKEYMAJ,INKEYMAJ        COPY MAJOR KEY                   
         XC    OTDATA.DVKEYMIN,OTDATA.DVKEYMIN CLEAR MINOR KEY                  
         MVI   OTDATA.DVKEYSEQ,PASSIVE#  INDICATE PASSIVE IN KEY SEQ            
         MVC   OTDATA.DVSTAT,INPSTAT     COPY STATUS                            
         NI    OTDATA.DVSTAT,X'FF'-DANDXFL# ZERO UNWANTED STATUS                
         MVI   OTDATA.DVPEOR,0           NON-EXTENDED END OF RECORD             
         LA    R1,DVPRLNQ                NON-EXTENDED RECORD LENGTH             
         TM    OTDATA.DVSTAT,DVSEXTP     EXTENDED PASSIVE?                      
         JZ    MAINI50                   NO, SKIP                               
         MVC   OTDATA.DVPEL(2),=AL1(X'01',DVPLENQ) DUMMY ELCODE/LEN             
         MVC   OTDATA.DVPDATA,INPDATA    COPY DATA TO ELEMENT                   
         MVI   OTDATA.DVPEEOR,0          EXTENDED END OF RECORD                 
         LA    R1,DVPERLNQ         EXTENDED RECORD LENGTH                       
*                                                                               
MAINI50  DS    0H                                                               
         STCM  R1,B'0011',OTDATA.DVLENDDS SET DDS LENGTH                        
         LA    R1,4(,R1)                                                        
         SLL   R1,16                                                            
         ST    R1,OUTREC.RECRDW    SET MVS LENGTH                               
         EJECT                                                                  
* OUTPUT VSAM FORMAT RECORD                                                     
*                                                                               
MAINOUT  DS    0H                                                               
         BRAS  RE,RECCHECK         CHECK FORMAT OF OUTPUT RECORD                
*                                                                               
         BRAS  RE,MERGEREC         DEAL WITH EXISTING RECS FOR MAJOR            
*                                                                               
         OC    OUTREC.RECRDW,OUTREC.RECRDW ZEROED BY MERGEREC IF RECORD         
         JZ    MAINNEXT            MUST BE DROPPED                              
*                                                                               
         LR    R0,R3               WRITE RECORD                                 
         BRAS  RE,PUTOUT                                                        
         J     MAINNEXT                                                         
*                                                                               
MAINNEXT EQU   MAININ              GO GET NEXT UPDATE RECORD                    
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
                                                                                
         XC    OTDATA.DVKEY,OTDATA.DVKEY                                        
         BRAS  RE,MERGEREC         COMPLETE EXISTING RECS FOR MAJOR             
                                                                                
         CLOSE (IFILE,,OFILE,,BKFILE,,DEMACB)                                   
                                                                                
         IF (CLI,SPLIT,EQ,YES)     SPLIT FILE?                                  
           CLOSE (OFIL2,,OFIL3,,OFIL4)                                          
           CLOSE (BKFIL2,,BKFIL3,,BKFIL4)                                       
           CLOSE (DEMAC2,,DEMAC3,,DEMAC4)                                       
         ENDIF ,                                                                
                                                                                
         BAS   RE,TOTALS           PRINT TOTALS                                 
*                                                                               
XBASE    DS    0H                                                               
         XBASE RC=RETCODE,RL=2     RETURN BACK TO MVS                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FORMAT OF RECORD AT R3.                                       *         
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
         CLC   LRECCHKY,OTDATA.DVREC HAVE WE PRINTED THIS ONE ALREADY           
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
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS FOR THE FILE/MEDIA/SOURCE JUST PROCESSED               *         
* R2 = A(CURRENTLY PROCESSED RECORD)                                  *         
***********************************************************************         
PRNTFMS  DS    0H                                                               
         OC    THISKFMS,THISKFMS   IF FIRST CALL, JUST SET MIDS                 
         JZ    PRNTFMSM                                                         
*                                                                               
         ST    RE,SAVERE                                                        
         MVC   P(L'THISKFMS),THISKFMS     PRINT THIS F/M/S KEY                  
         EDIT  CTRINFMS,(15,P+15),COMMAS=YES,ZERO=NOBLANK                       
         LG    GR1,FMSBYTES               # BYTES                               
         CVDG  GR1,LONG                   VALUE IS IN DUB1+DUB2                 
         EDIT  (P8,DUB2),(15,P+35),COMMAS=YES,ZERO=NOBLANK                      
         GOTO1 VPRINTER                                                         
         AP    CTRFMSTO,CTRINFMS          KEEP A RUNNING F/M/S TOTAL            
         ZAP   CTRINFMS,=P'0'             RESET RECORD COUNTER                  
         XC    FMSBYTES,FMSBYTES          AND BYTE COUNTER.                     
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRNTFMSM DS    0H                  FIRST TIME, JUST SET MIDS                    
         MVC   MID1,SPACES                                                      
         MVC   MID1(50),=C'FMS                   #RECORDS              +        
               #BYTES'                                                          
         MVC   MID2,SPACES                                                      
         MVC   MID2(50),=C'--------------------------------------------+        
               ------'                                                          
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PUT OUTPUT RECORD AT 0(R0)                                          *         
***********************************************************************         
PUTOUT   NTR1  BASE=(*,PUTOUT_X),LABEL=*                                        
*                                                                               
         LR    RF,R0                                                            
         XR    R5,R5                                                            
         ICM   R5,B'0011',0(RF)    R5=DDS RECORD LENGTH                         
         CH    R5,OMAXRLEN         SET MAXIMUM                                  
         JNH   *+8                                                              
         STH   R5,OMAXRLEN                                                      
*                                                                               
         SELECT CLI,VFILEID,EQ                                                  
           WHEN (SPLIT1)           FOR MAIN OR 1ST SPLIT FILE                   
             LARL  R1,OFILE          POINT TO OFILE DCB                         
             PUT   (1),(0)           WRITE RECORD AT (R0) TO OFILE              
             AP    CTROUT,=P'1'      COUNT OFILE RECORDS OUT                    
             LG    GRE,OTBYTES       ACCUMULATE OUTPUT BYTES                    
             AGFR  GRE,R5                                                       
             STG   GRE,OTBYTES                                                  
           WHEN (SPLIT2)           FOR 2ND SPLIT FILE                           
             LARL  R1,OFIL2          POINT TO OFIL2 DCB                         
             PUT   (1),(0)           WRITE RECORD AT (R0) TO OFIL2              
             AP    CTROU2,=P'1'      COUNT OFIL2 RECORDS OUT                    
             LG    GRE,OTBYTE2       ACCUMULATE OUTPUT BYTES                    
             AGFR  GRE,R5                                                       
             STG   GRE,OTBYTE2                                                  
           WHEN (SPLIT3)           FOR 3RD SPLIT FILE                           
             LARL  R1,OFIL3          POINT TO OFIL3 DCB                         
             PUT   (1),(0)           WRITE RECORD AT (R0) TO OFIL3              
             AP    CTROU3,=P'1'      COUNT OFIL3 RECORDS OUT                    
             LG    GRE,OTBYTE3       ACCUMULATE OUTPUT BYTES                    
             AGFR  GRE,R5                                                       
             STG   GRE,OTBYTE3                                                  
           WHEN (SPLIT4)           FOR 4TH SPLIT FILE                           
             LARL  R1,OFIL4          POINT TO OFIL4 DCB                         
             PUT   (1),(0)           WRITE RECORD AT (R0) TO OFIL4              
             AP    CTROU4,=P'1'      COUNT OFIL4 RECORDS OUT                    
             LG    GRE,OTBYTE4       ACCUMULATE OUTPUT BYTES                    
             AGFR  GRE,R5                                                       
             STG   GRE,OTBYTE4                                                  
           OTHRWISE ,                                                           
             J *+2                 INVALID FILE ?!?                             
         ENDSEL ,                                                               
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
*                                                                               
         SELECT CLI,VFILEID,EQ                                                  
           WHEN (SPLIT1)           FOR MAIN OR 1ST SPLIT FILE                   
             LARL  R1,BKFILE         POINT TO BKFILE DCB                        
             PUT   (1),(0)           WRITE RECORD AT (R0) TO BKFILE             
             AP    CTRBAK,=P'1'      COUNT BKFILE RECORDS OUT                   
           WHEN (SPLIT2)           FOR 2ND SPLIT FILE                           
             LARL  R1,BKFIL2         POINT TO BKFIL2 DCB                        
             PUT   (1),(0)           WRITE RECORD AT (R0) TO BKFIL2             
             AP    CTRBA2,=P'1'      COUNT BKFIL2 RECORDS OUT                   
           WHEN (SPLIT3)           FOR 3RD SPLIT FILE                           
             LARL  R1,BKFIL3         POINT TO BKFIL3 DCB                        
             PUT   (1),(0)           WRITE RECORD AT (R0) TO BKFIL3             
             AP    CTRBA3,=P'1'      COUNT BKFIL3 RECORDS OUT                   
           WHEN (SPLIT4)           FOR 4TH SPLIT FILE                           
             LARL  R1,BKFIL4         POINT TO BKFIL4 DCB                        
             PUT   (1),(0)           WRITE RECORD AT (R0) TO BKFIL4             
             AP    CTRBA4,=P'1'      COUNT BKFIL4 RECORDS OUT                   
           OTHRWISE ,                                                           
             J *+2                 INVALID FILE ?!?                             
         ENDSEL ,                                                               
*                                                                               
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
TOTALS   NTR1  BASE=(*,TOTALS_X),LABEL=*                                        
*                                                                               
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
         IF (CP,CTRIN,EQ,=P'0')                                                 
           LARL  R1,LTCWNOIP       WARNING: INPUT FILE EMPTY                    
           MVC   P+55(L'LTCWNOIP),0(R1)                                         
           LA    R1,4              SET RC TO 4: NOTHING TO UPDATE               
           BRAS  RE,SETRC                                                       
         ELSE ,                                                                 
           ZAP   DUB,CTRAOF        DUB= #RECS. ALREADY ON FILE +                
           AP    DUB,CTRDTDF            #RECS. THAT MATCH EXCEPT DATE           
           IF (CP,CTRIN,EQ,DUB)                                                 
             LARL  R1,LTCWPDUP       WARNING: POSSIBLE ERROR                    
             MVC   P+55(L'LTCWPDUP),0(R1)                                       
             LA    R1,4              SET RC TO 4: NOTHING TO UPDATE             
             BRAS  RE,SETRC                                                     
           ENDIF ,                                                              
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
                                                                                
         LARL  R1,LTCFILM          DA RECORDS MARKED DELETED                    
         MVC   P+2(L'LTCFILM),0(R1)                                             
                                                                                
         EDIT  CTRFILD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                        
         IF (CP,CTRFILD,NE,=P'0')  CHECK FOR DELETED MINOR KEYS                 
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
                                                                                
         LARL  R1,LTCPASXM         EXTENDED PASSIVES *MARKED* DELETED           
         MVC   P+2(L'LTCPASXM),0(R1)                                            
         EDIT  CTRPASXD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCDNX           OTHER DIRECTORY RECORDS                      
         MVC   P+2(L'LTCDNX),0(R1)                                              
         EDIT  CTRDNX,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCDNXD          THIS COULD BE "ACTIVE" OR PASSIVE            
         MVC   P+2(L'LTCDNXD),0(R1)                                             
         EDIT  CTRDNXD,(15,P+35),COMMAS=YES,ZERO=NOBLANK                        
         GOTOR VPRINTER                                                         
         GOTOR VPRINTER                                                         
                                                                                
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
                                                                                
         IF (CLI,SPLIT,EQ,YES)                                                  
           LARL  R1,LTCOU2           OFIL2 RECORDS WRITTEN                      
           MVC   P(L'LTCOU2),0(R1)                                              
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
           LARL  R1,LTCOU3           OFIL3 RECORDS WRITTEN                      
           MVC   P(L'LTCOU3),0(R1)                                              
           EDIT  CTROU3,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
           GOTOR VPRINTER                                                       
           LG    GR1,OTBYTE3         BYTES                                      
           IF (CGFI,GR1,H,999999)                                               
             AGHI  GR1,512             +512                                     
             SRAG  GR1,GR1,10          /1024                                    
             MVC   P+50(2),=C'KB'                                               
           ENDIF ,                                                              
           CVDG  GR1,LONG            VALUE IS IN DUB1+DUB2                      
           LARL  R1,LTCOU3B          OFIL3 BYTES WRITTEN, INC RDW               
           MVC   P(L'LTCOU3B),0(R1)                                             
           MVC   P+30(20),ED_PATTERN1 ###,###,###,###,###                       
           ED    P+30(20),DUB2                                                  
           GOTOR VPRINTER                                                       
           LARL  R1,LTCOU4           OFIL4 RECORDS WRITTEN                      
           MVC   P(L'LTCOU4),0(R1)                                              
           EDIT  CTROU4,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
           GOTOR VPRINTER                                                       
           LG    GR1,OTBYTE4         BYTES                                      
           IF (CGFI,GR1,H,999999)                                               
             AGHI  GR1,512             +512                                     
             SRAG  GR1,GR1,10          /1024                                    
             MVC   P+50(2),=C'KB'                                               
           ENDIF ,                                                              
           CVDG  GR1,LONG            VALUE IS IN DUB1+DUB2                      
           LARL  R1,LTCOU4B          OFIL4 BYTES WRITTEN, INC RDW               
           MVC   P(L'LTCOU4B),0(R1)                                             
           MVC   P+30(20),ED_PATTERN1 ###,###,###,###,###                       
           ED    P+30(20),DUB2                                                  
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
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
         AP    DUB2,CTROU2                                                      
         AP    DUB2,CTROU3                                                      
         AP    DUB2,CTROU4                                                      
         IF (NZ)                                                                
           CVBG  GRE,LONG                                                       
           XGR   GR0,GR0           CALC. AVERAGE LRECL. GR0/1=BYTES             
           LG    GR1,OTBYTES                                                    
           AG    GR1,OTBYTE2                                                    
           AG    GR1,OTBYTE3                                                    
           AG    GR1,OTBYTE4                                                    
           DLGR  GR0,GRE           DON'T CARE ABOUT ROUNDING                    
           CVDG  GR1,LONG                                                       
         ENDIF ,                                                                
         EDIT  (P8,DUB2),(15,P+35),COMMAS=YES,ZERO=NOBLANK                      
         GOTOR VPRINTER                                                         
         GOTOR VPRINTER                                                         
                                                                                
         LARL  R1,LTCBAK           BKFILE RECORDS WRITTEN                       
         MVC   P(L'LTCBAK),0(R1)                                                
         EDIT  CTRBAK,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
         GOTOR VPRINTER                                                         
         IF (CLI,SPLIT,EQ,YES)                                                  
           LARL  R1,LTCBA2           BKFIL2 RECORDS WRITTEN                     
           MVC   P(L'LTCBA2),0(R1)                                              
           EDIT  CTRBA2,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
           GOTOR VPRINTER                                                       
           LARL  R1,LTCBA3           BKFIL3 RECORDS WRITTEN                     
           MVC   P(L'LTCBA3),0(R1)                                              
           EDIT  CTRBA3,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
           GOTOR VPRINTER                                                       
           LARL  R1,LTCBA4           BKFIL4 RECORDS WRITTEN                     
           MVC   P(L'LTCBA4),0(R1)                                              
           EDIT  CTRBA4,(15,P+35),COMMAS=YES,ZERO=NOBLANK                       
           GOTOR VPRINTER                                                       
         ENDIF ,                                                                
         GOTOR VPRINTER                                                         
                                                                                
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
                                                                                
         ZAP   PL8,MAXLINE         MAKE SURE ROOM ON PAGE                       
         SP    PL8,LINE                                                         
         IF (CP,PL8,LT,=P'6')                                                   
           ZAP   LINE,=P'99'       NEW PAGE IF LESS THAN 6 LINES                
         ELSE ,                                                                 
           GOTOR VPRINTER          ELSE JUST A BLANK LINE                       
         ENDIF ,                                                                
                                                                                
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
                                                                                
         L     R2,VPRNTER                                                       
         CLOSE ((R2))                                                           
*                                                                               
         J     XIT1                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
TOTALS_X EQU   *                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE BACKOUT AND EXTRA UPDATE RECORDS FOR THE CURRENT MAJOR KEY.   *         
* THIS ROUTINE MERGES THE UPDATE FILE WITH THE EXISTING RECORDS ON    *         
* THE VSAM FILE. THE RULES ARE AS FOLLOWS:                            *         
*                                                                     *         
* 1. A PASSIVE RECORD WILL REPLACE (OR DELETE) A PASSIVE ON FILE WITH *         
*    THE SAME KEY. IF NONE, IT IS ADDED. IF IT HAS ITS DELETE BIT ON, *         
*    IT SHOULD MATCH AN EXISTING KEY AND DELETES IT.                  *         
* 2. A NON-PASSIVE DIRECTORY RECORD WITH ITS DELETE BIT SET INDICATES *         
*    THAT ALL RECORDS FOR THE MATCHING MAJOR GROUP ARE TO BE DELETED. *         
*    SUCH A RECORD WITHOUT THE DELETE BIT SET IS AN ERROR.            *         
* 3. ALL FILE RECORDS (I.E. NOT PASSIVE OR DIRECTORY RECORDS) HAVING  *         
*    A GIVEN MAJOR KEY MUST *REPLACE* ALL RECORDS HAVING THE SAME     *         
*    MAJOR KEY. INPUT RECORDS MUST NOT THEREFORE HAVE DELETE BITS ON. *         
*    WE DO NOT ATTEMPT TO MERGE INPUT RECORDS FOR A GIVEN MAJOR KEY   *         
*    WITH THOSE ALREADY ON THE FILE. THE WHOLE MAJOR KEY IS REPLACED. *         
*                                                                     *         
* OUTREC.RECDSECT 0(R3) POINTS TO CURRENT OUTPUT RECORD RDW.          *         
* OTDATA.DVREC    4(R3) IS THE DDS VSAM RECORD. KEY NULL IF EOF CALL  *         
*                                                                     *         
* VSMREC.RECDSECT 0(R4) POINTS TO CURRENT VSAM INPUT RECORD RDW       *         
* VSDATA.DVREC    4(R4) IS THE DDS VSAM INPUT RECORD.                 *         
*                                                                     *         
* VSMKMAJ CONTAINS THE CURRENT MAJOR KEY.                             *         
*                                                                     *         
* USE OF THE BACKOUT FILE(S):                                         *         
* -AN UPDATE RECORD WHOSE KEY IS NOT PRESENT IN THE VSAM FILE WILL BE *         
*  INSERTED INTO THE VSAM FILE. THEREFORE, AN EMPTY DELETED COPY OF   *         
*  THE NEW RECORD MUST BE WRITTEN TO THE BACKOUT FILE.                *         
* -AN UPDATE RECORD WITH THE SAME KEY AS A VSAM RECORD WILL REPLACE   *         
*  THAT RECORD AND THE ORIGINAL RECORD MUST BE WRITTEN TO THE BACKOUT *         
*  FILE.                                                              *         
* -AN UPDATE RECORD THAT IS DELETED AND NOT A PASSIVE MUST BE FROM A  *         
*  DELETED POINTER AND INDICATES A WHOLE MAJOR GROUP MUST BE DELETED. *         
*  SO WRITE A DELETE FOR EACH RECORD IN THE MAJOR GROUP AND WRITE THE *         
*  ORIGINAL RECORDS TO THE BACKOUT FILE.                              *         
* THE EFFECT OF THIS IS THAT IF THE BACKOUT FILE IS ADDED TO THE VSAM *         
* FILE IN THE SAME WAY AS THE UPDATE FILE WAS, IT WILL EFFECTIVELY    *         
* REVERSE THAT UPDATE.                                                *         
***********************************************************************         
MERGEREC NTR1  BASE=(*,MERGEREC_X),LABEL=*                                      
*                                                                               
         OC    VSMKMAJ,VSMKMAJ     FIRST TIME?                                  
         JZ    MR20                YES, SKIP                                    
*                                                                               
         CLC   OTDATA.DVKEYMAJ,VSMKMAJ HAVE WE CHANGED MAJOR KEY?               
         JE    MR30                NO, SKIP                                     
*                                                                               
*        COMPLETE PREVIOUS MAJOR KEY                                            
*                                                                               
MR10     DS    0H                                                               
         CLC   VSDATA.DVKEYMAJ,VSMKMAJ MORE FOR THIS MAJOR ON FILE?             
         JNE   MR20                NO, SKIP                                     
         TM    VSDATA.DVSTAT,DVSDEL IS OLD RECORD DELETED                       
         JNZ   MR12                YES, DON'T DELETE AGAIN                      
         AP    CTRDELET,=P'1'                                                   
         CLI   VSDATA.DVKEYSEQ,PASSIVE# IS OLD RECORD A PASSIVE?                
         JE    *+10                                                             
         AP    CTRDELMN,=P'1'      NO, IT'S A DELETED MINOR                     
*                                                                               
         AP    CTRDELWR,=P'1'                                                   
         MVC   DELRKEY,VSDATA.DVKEY BUILD A DELETE FROM OLD VSAM REC            
         LA    R0,DELREC                                                        
         BRAS  RE,PUTOUT           WRITE DELETE TO UPDATE                       
         LR    R0,R4                                                            
         BRAS  RE,PUTBAK           WRITE ORIGINAL RECORD TO BACKOUT             
*                                                                               
MR12     DS    0H                                                               
         BRAS  RE,GETVSSEQ         GET NEXT VSAM RECORD                         
         J     MR10                                                             
*                                                                               
*        BEGIN A NEW MAJOR KEY (UNLESS EOF)                                     
*                                                                               
MR20     DS    0H                                                               
         OC    OTDATA.DVKEYMAJ,OTDATA.DVKEYMAJ IS THIS THE EOF CALL             
         JZ    MRX                 YES, ALL DONE                                
*                                                                               
         MVC   VSMKMAJ,OTDATA.DVKEYMAJ SET NEW MAJOR KEY AND                    
         XC    VSMKMIN,VSMKMIN     ENSURE WE GET FIRST FOR MAJOR                
*                                                                               
         MVI   VFILEID,MAINFIL     ALL RECS IN FILE 1 IF NOT SPLIT              
*                                  ELSE ONLY NON-'RTN' RECS IF SPLIT            
         IF (CLI,SPLIT,EQ,YES),AND,        IF SPLIT, 'RTN' RECORDS ARE          
            (CLC,=C'RTN',EQ,VSMKMAJ)     , SPREAD OVER FILES 2, 3 AND 4         
           MVI   VFILEID,SPLIT2            POSIT KEY LIVES ON FILE 2            
           IF (CLC,VSMKMAJ,GE,F4MAJ1ST)    IF KEY >= 1ST KEY ON FILE 4,         
             MVI   VFILEID,SPLIT4             IT LIVES ON FILE 4                
           ELSEIF (CLC,VSMKMAJ,GE,F3MAJ1ST)   KEY >= 1ST KEY ON FILE 3          
             MVI   VFILEID,SPLIT3             IT LIVES ON FILE 3                
           ENDIF ,                                                              
         ENDIF ,                                                                
*                                                                               
         BRAS  RE,GETVSHI          GET FIRST VSAM RECORD FOR NEW MAJOR          
*                                                                               
         CLC   VSDATA.DVKEYMAJ,VSMKMAJ IS MAJOR KEY ALREADY ON FILE?            
         JNE   MR28                NO, SKIP, IT'S A NEW ONE                     
*                                                                               
*        MAJOR KEY ALREADY EXISTS:                                              
*                                                                               
         CLI   VSDATA.DVKEYSEQ,PASSIVE# IS EXISTING MAJOR A PASSIVE?            
         JNE   MR25                NO, SKIP                                     
*                                                                               
*        EXISTING MAJOR KEY IS A PASSIVE:                                       
*                                                                               
         CLI   OTDATA.DVKEYSEQ,PASSIVE# NEW RECORD MUST ALSO                    
         JE    MR24                BE A PASSIVE                                 
         AP    CTRERP2F,=P'1'      CAN'T CHANGE PASSIVE TO FILE RECORD          
         XC    VSMKMAJ,VSMKMAJ     CLEAR SO WON'T PURGE EXISTING PASSVE         
         J     MRDROP              IGNORE THIS RECORD                           
*                                                                               
MR24     DS    0H                                                               
         LA    R1,CTRUPDPV         POSIT UPDATED PASSIVE                        
         TM    OTDATA.DVSTAT,DVSEXTP IS EXTENDED FLAG ON?                       
         JZ    *+8                                                              
         LA    R1,CTRUPDPX         POSIT UPDATED EXTENDED PASSIVE               
         TM    OTDATA.DVSTAT,DVSDEL TEST UPDATE RECORD IS DELETED               
         JZ    *+8                                                              
         LA    R1,CTRDELPV         YES, SO DELETING PASSIVE                     
         TM    OTDATA.DVSTAT,DVSEXTP+DVSDEL TEST DELETED AND EXTENDED           
         JNO   *+8                                                              
         LA    R1,CTRDELPX         YES, SO DELETING EXTENDED PASSIVE            
         AP    0(L'CTRS,R1),=P'1'                                               
         J     MR30                                                             
*                                                                               
*        EXISTING MAJOR KEY IS A FILE (NOT PASSIVE) RECORD:                     
*                                                                               
MR25     DS    0H                                                               
         CLI   OTDATA.DVKEYSEQ,PASSIVE# IS UPDATE A DIRECTORY RECORD?           
         JE    MR26                YES, BUT NOT REALLY A PASSIVE                
         TM    OTDATA.DVSTAT,DVSDEL TEST UPDATE RECORD IS DELETED               
         JO    *+2                 YES, SHOULD'VE BEEN DROPPED AT MAIN          
         AP    CTRUPDMJ,=P'1'                                                   
         J     MR30                UPDATE MAJOR GROUP                           
*                                                                               
MR26     DS    0H                                                               
         TM    OTDATA.DVSTAT,DVSDEL TEST UPDATE RECORD IS DELETED               
         JO    MR27                YES, SKIP, IT'S A DELETED POINTER            
*                                  ELSE IT MUST BE A PASSIVE AND WE             
         AP    CTRERF2P,=P'1'      CAN'T CHANGE FILE RECORD TO PASSIVE          
         XC    VSMKMAJ,VSMKMAJ     CLEAR SO WON'T PURGE MAJOR                   
         J     MRDROP              IGNORE THIS RECORD                           
*                                                                               
MR27     DS    0H                                                               
         AP    CTRPTRD,=P'1'       IT'S A DELETED POINTER                       
         AP    CTRDELMJ,=P'1'      SO WE'RE DELETING A WHOLE MAJOR.             
         J     MRDROP              DROP THIS RECORD  AND NEXT CALL WILL         
*                                  DELETE WHOLE MAJOR GROUP.                    
*        MAJOR KEY DOES NOT YET EXIST:                                          
*                                                                               
MR28     DS    0H                                                               
         TM    OTDATA.DVSTAT,DVSDEL IS UPDATE RECORD DELETED?                   
         JZ    *+14                                                             
         AP    CTRERDNF,=P'1'      YES, BUT NO MAJOR KEY TO DELETE              
         J     MRDROP              IGNORE THIS RECORD                           
*                                                                               
         LA    R1,CTRNEWPV         POSIT NEW PASSIVE                            
         TM    OTDATA.DVSTAT,DVSEXTP IS EXTENDED FLAG ON?                       
         JZ    *+8                                                              
         LA    R1,CTRNEWPX         POSIT NEW EXTENDED PASSIVE                   
         CLI   OTDATA.DVKEYSEQ,PASSIVE# IS NEW RECORD A PASSIVE?                
         JE    *+8                                                              
         LA    R1,CTRNEWMJ         IT'S A NEW MAJOR                             
         AP    0(L'CTRS,R1),=P'1'                                               
*                                                                               
*        UPDATE RECORD HAS SAME MAJOR KEY AS LAST OR IS A NEW MAJOR             
*                                                                               
MR30     DS    0H                                                               
         CLC   OTDATA.DVKEY,VSDATA.DVKEY COMPARE NEW KEY WITH FILE              
         JH    MR40                UPDATE HIGHER THAN FILE                      
         JL    MR50                UPDATE LOWER THAN FILE                       
*                                  UPDATE EQUAL TO FILE                         
         AP    CTRREPL,=P'1'       COUNT REPLACEMENT RECORDS                    
         CLI   OTDATA.DVKEYSEQ,PASSIVE# IS NEW RECORD A PASSIVE?                
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
         LR    R0,R3               COMPARE REPLACEMENT                          
         LH    R1,0(R3)                                                         
         LR    RE,R4               WITH ORIGINAL                                
         LH    RF,0(R4)                                                         
         CLCL  R0,RE                                                            
         JNE   MR32                THE RECORDS DIFFER                           
*                                                                               
*                                  RECORDS MATCH: NO NEED TO OVERWRITE          
         IF  (CR,R2,EQ,R5)         IF THE CONVERSION DATES MATCH:               
           AP  CTRAOF,=P'1'         INCREMENT "ALREADY ON FILE" COUNTER         
         ELSE ,                    O/W:                                         
           AP  CTRDTDF,=P'1'        INCREMENT "MATCH EXCEPT FOR DATE"           
         ENDIF ,                                                                
         XC    OUTREC.RECRDW,OUTREC.RECRDW ZERO RDW SO DROPPED ON RETRN         
         J     MR36                                                             
*                                                                               
MR32     DS    0H                                                               
         USING MARELEM,R1                                                       
         IF (CHI,R2,NE,0)          IF WE SAVED DATES RESTORE THEM               
           LA    R1,OTDATA.DVFRSTEL  R1 = A(1ST ELEM. IN REPLACEMENT)           
           STCM  R2,3,MARDATE        RESTORE THE CONVERSION DATE                
           LA    R1,VSDATA.DVFRSTEL  R1 = A(1ST ELEM. IN ORIGINAL)              
           STCM  R5,3,MARDATE        RESTORE THE CONVERSION DATE                
         ENDIF ,                                                                
         DROP  R1                                                               
*                                                                               
         TM    VSDATA.DVSTAT,DVSDEL IS OLD RECORD DELETED                       
         JZ    MR34                                                             
         TM    OTDATA.DVSTAT,DVSDEL YES, IS NEW RECORD DELETED                  
         JO    *+14                                                             
         AP    CTRDOF,=P'1'        NO, COUNT ALREADY ON FILE BUT DELTD          
         J     MR34                                                             
*                                                                               
         AP    CTRDDF,=P'1'        BOTH DELETED SO NO POINT IN OUTPUT           
         XC    OUTREC.RECRDW,OUTREC.RECRDW ZERO RDW SO DROPPED ON RETRN         
         J     MR36                                                             
*                                                                               
MR34     DS    0H                                                               
         LR    R0,R4                                                            
         BRAS  RE,PUTBAK           WRITE ORIGINAL RECORD TO BACKOUT             
*                                                                               
         TM    OTDATA.DVSTAT,DVSDEL IS NEW RECORD DELETED                       
         JZ    MR36                                                             
         AP    CTRREPLD,=P'1'      YES, COUNT AND                               
         AP    CTRDELWR,=P'1'      REPLACE WITH A DELETE RECORD                 
         MVC   DELRKEY,OTDATA.DVKEY (SHOULD BE A PASSIVE)                       
         MVC   OUTREC.RECDSECT(DELRMLNQ),DELREC                                 
*                                                                               
MR36     DS    0H                                                               
         BRAS  RE,GETVSSEQ         GET NEXT VSAM RECORD                         
         J     MRX                                                              
*                                  FILE IS LOW: NEEDS TO BE DELETED             
MR40     DS    0H                                                               
         TM    VSDATA.DVSTAT,DVSDEL IS OLD RECORD DELETED                       
         JNZ   MR42                YES, DON'T DELETE AGAIN                      
*                                                                               
         AP    CTRDELET,=P'1'                                                   
         CLI   VSDATA.DVKEYSEQ,PASSIVE# IS OLD RECORD A PASSIVE?                
         JE    *+10                                                             
         AP    CTRDELMN,=P'1'      NO, IT'S A DELETED MINOR                     
         AP    CTRDELWR,=P'1'                                                   
         MVC   DELRKEY,VSDATA.DVKEY BUILD A DELETE FROM VSAM REC                
         LA    R0,DELREC                                                        
         BRAS  RE,PUTOUT           WRITE DELETE TO UPDATE                       
         LR    R0,R4                                                            
         BRAS  RE,PUTBAK           WRITE ORIGINAL RECORD TO BACKOUT             
*                                                                               
MR42     DS    0H                                                               
         BRAS  RE,GETVSSEQ         GET NEXT VSAM RECORD                         
         J     MR30                RETRY COMPARE                                
*                                                                               
*                                  FILE IS HIGH: UPDATE IS AN INSERT            
MR50     DS    0H                                                               
         AP    CTRINSRT,=P'1'      COUNT INSERTS                                
         CLI   OTDATA.DVKEYSEQ,PASSIVE# IS OLD RECORD A PASSIVE?                
         JE    *+10                                                             
         AP    CTRNEWMN,=P'1'      NO, IT'S A NEW MINOR                         
         MVC   DELRKEY,OTDATA.DVKEY BUILD A DELETE FROM UPDATE RECORD           
         LA    R0,DELREC                                                        
         BRAS  RE,PUTBAK           WRITE DELETE TO BACKOUT                      
         J     MRX                                                              
*                                                                               
MRDROP   DS    0H                                                               
         XC    OUTREC.RECRDW,OUTREC.RECRDW ZERO RDW SO DROPPED ON RETRN         
*                                                                               
MRX      DS    0H                                                               
         J     XIT1                DROP THIS RECORD                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
MERGEREC_X EQU *                                                                
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
*                                                                               
VALPAR_CHECK_CONSISTENCY DS 0H                                                  
         CLI   SPLIT,0             SPLIT= INPUT?                                
         JNE   VALPAR10            YES                                          
         CLI   DTF#,DEMVSMN#       NO, FILE=DEMVSMN INPUT?                      
         JNE   VALPARX             NO                                           
         MVI   SPLIT,YES           DEFAULT TO SPLIT=Y FOR DEMVSMN               
         J     VALPARX                                                          
VALPAR10 CLI   DTF#,DEMVSMN#       FILE=DEMVSMN INPUT?                          
         JE    VALPARX             YES SPLIT= ONLY VALID FOR DEMVSMN            
         LARL  R1,LTPSDN           ELSE ERROR                                   
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
         DC    AL1(04),AL3(PARFILE),CL20'FILE='                                 
         DC    AL1(05),AL3(PARSPLT),CL20'SPLIT='                                
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
DEVSUPD  CSECT                                                                  
*                                                                               
* PARAMETER VALIDATION ROUTINES R1=A(PARAMETER VALUE)                           
*                                                                               
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
PARSPLT  DS    0H                                                               
         CLI   SPLIT,0             SPLIT= (DEFAULT=Y)                           
         JNE   VALPARDP                                                         
         MVC   SPLIT,0(R1)                                                      
         CLI   SPLIT,YES           SPLIT=Y(ES)                                  
         BER   RE                                                               
         CLI   SPLIT,NO            SPLIT=N(O)                                   
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
* VSAM READ ROUTINES. KEY IN VSMKEY, VSAM RECORD RETURNED AT 0(R4)    *         
* PLACED IN COMMON AREA SO WILL USE COMMON BASE                       *         
***********************************************************************         
         USING IFGRPL,R5                                                        
*                                                                               
GETVSHI  NTR1  ,                   GET FIRST RECORD FOR MAJOR KEY               
         SELECT CLI,VFILEID,EQ                                                  
           WHEN (SPLIT1)           FROM MAIN OR 1ST SPLIT FILE                  
             LARL  R5,DEMRPL                                                    
           WHEN (SPLIT2)           FROM 2ND SPLIT FILE                          
             LARL  R5,DEMRP2                                                    
           WHEN (SPLIT3)           FROM 3RD SPLIT FILE                          
             LARL  R5,DEMRP3                                                    
           WHEN (SPLIT4)           FROM 4TH SPLIT FILE                          
             LARL  R5,DEMRP4                                                    
           OTHRWISE ,                                                           
             J *+2                 INVALID FILE ?!?                             
         ENDSEL ,                                                               
*                                                                               
         NI    RPLOPT1,255-RPLSEQ                                               
         OI    RPLOPT1,RPLDIR+RPLKGE                                            
         J     GETVSAM                                                          
*                                                                               
GETVSSEQ NTR1  ,                   GET NEXT VSAM RECORD                         
*                                                                               
         SELECT CLI,VFILEID,EQ                                                  
           WHEN (SPLIT1)           FROM MAIN OR 1ST SPLIT FILE                  
             LARL  R5,DEMRPL                                                    
           WHEN (SPLIT2)           FROM 2ND SPLIT FILE                          
             LARL  R5,DEMRP2                                                    
           WHEN (SPLIT3)           FROM 3RD SPLIT FILE                          
             LARL  R5,DEMRP3                                                    
           WHEN (SPLIT4)           FROM 4TH SPLIT FILE                          
             LARL  R5,DEMRP4                                                    
           OTHRWISE ,                                                           
             J *+2                 INVALID FILE ?!?                             
         ENDSEL ,                                                               
*                                                                               
         NI    RPLOPT1,255-(RPLDIR+RPLKGE)                                      
         OI    RPLOPT1,RPLSEQ      SET OPTCD=SEQ                                
*                                                                               
GETVSAM  DS    0H                                                               
         GET   RPL=(R5)            READ VSAM RECORD                             
         LTR   RF,RF                                                            
         JNZ   *+2                 VSAM ERROR BEFORE WAIT                       
         CHECK RPL=(R5)                                                         
         OC    RPLFDBK,RPLFDBK                                                  
         JNZ   *+2                 VSAM ERROR                                   
         L     R1,RPLRLEN          RETURNED RECORD LENGTH                       
         LA    R1,4(,R1)           ADD 4 FOR RDW LENGTH                         
         SLL   R1,16               AND SHIFT TO CORRECT POSITION                
         ST    R1,VSMREC.RECRDW                                                 
*                                                                               
         J     XIT1                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5                  IFGRPL                                       
         EJECT                                                                  
***********************************************************************         
* STORAGE                                                             *         
***********************************************************************         
*              W/S REQUIREMENTS                                                 
*                                                                               
         DS    0L                                                               
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
ATIOT    DS    F                                                                
AASID    DS    F                                                                
AASCB    DS    F                                                                
*                                                                               
STEPNAME DS    CL8                                                              
JOBNAME  DS    CL8                                                              
JOBID    DS    CL8                                                              
*                                                                               
PL16     DS    0PL16                                                            
PL8      DS    0PL8                                                             
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
*                                                                               
TESTFIL# EQU   31                  ALL TEST FILES ARE LOGICAL FILE #31          
DANDXFL# EQU   B'00111111'         DANDX FILE NUMBER STATUS BITS                
PASSIVE# EQU   X'FF'               SEQUENCE # X'FF' = PASSIVE KEY               
*                                                                               
         DS    0D                                                               
         DC    C'*VSMKEY*'                                                      
VSMKEY   DS    0XL21               KEY FOR VSAM READS                           
VSMKMAJ  DC    XL18'00'            MAJOR KEY FOR MERGE ROUTINE                  
VSMKMIN  DC    XL3'00'             PLUS REST OF VSAM KEY                        
*                                                                               
F3MAJ1ST DS    XL18                FIRST MAJOR KEY ON FILE 3                    
F4MAJ1ST DS    XL18                FIRST MAJOR KEY ON FILE 4                    
*                                                                               
*----------------------------------------------------------------------         
RUNVALS  DS    0X                  ** RUN TIME PARAMETERS **                    
*                                                                               
SPLIT    DS    C                   Y IF SPLIT=Y ELSE N OR NULL                  
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
VFILEID  DC    C' '                RECORD SOURCE                                
MAINFIL  EQU   C'1'                                                             
SPLIT1   EQU   MAINFIL                                                          
SPLIT2   EQU   C'2'                                                             
SPLIT3   EQU   C'3'                                                             
SPLIT4   EQU   C'4'                                                             
*                                                                               
*                                  ED PATTERN: ###,###,###,###,###              
ED_PATTERN1 DC X'402020206B2020206B2020206B2020206B202120'                      
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
INDSN    DS    CL44                                                             
         EJECT                                                                  
         DS    0D                                                               
         DC    C'COUNTERS'                                                      
CTRS     DS    0PL8                                                             
*                                                                               
CTRIN    DC    PL8'0'              ALL RECORDS READ                             
CTRINFMS DC    PL8'0'              ALL RECORDS READ (BY FILE/MEDIA/SRC)         
CTRFMSTO DC    PL8'0'              RUNNING F/M/S TOTAL                          
*        FILE RECORDS (I.E. NOT DIRECTORY RECORDS):                             
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
*        DIRECTORY RECORDS:                                                     
CTRDIR   DC    PL8'0'              DIRECTORY RECORDS READ                       
CTRDIRTD DC    PL8'0'              - DIRECTORY TEST DATA RECORDS READ           
CTRPASX  DC    PL8'0'              - EXTENDED PASSIVES READ                     
CTRPASXD DC    PL8'0'              - DELETED EXTENDED PASSIVES READ             
CTRDNX   DC    PL8'0'              - OTHER DIRECTORY RECORDS READ               
CTRDNXD  DC    PL8'0'              - OTHER DELETED DIRECTORY RECORDS            
CTRDIRPL DC    PL8'0'              - DIR. RECORDS IGNORED AS S/B PEELED         
*        OUTPUT COUNTERS:                                                       
CTROUT   DC    PL8'0'              ALL RECORDS WRITTEN OFILE                    
CTROU2   DC    PL8'0'              ALL RECORDS WRITTEN OFIL2                    
CTROU3   DC    PL8'0'              ALL RECORDS WRITTEN OFIL3                    
CTROU4   DC    PL8'0'              ALL RECORDS WRITTEN OFIL4                    
CTRBAK   DC    PL8'0'              BKFILE RECORDS WRITTEN                       
CTRBA2   DC    PL8'0'              BKFIL2 RECORDS WRITTEN                       
CTRBA3   DC    PL8'0'              BKFIL3 RECORDS WRITTEN                       
CTRBA4   DC    PL8'0'              BKFIL4 RECORDS WRITTEN                       
*        RECORD CHECK WARNINGS AND ERRORS:                                      
CTRRCE01 DC    PL8'0'              RDW LENGTH TOO SMALL                         
CTRRCE02 DC    PL8'0'              DDS LENGTH TOO SMALL                         
CTRRCE03 DC    PL8'0'              ZERO ELEMENT LENGTH                          
CTRRCW01 DC    PL8'0'              MISSING EOR (NO NULL TERMINATOR)             
CTRRCW02 DC    PL8'0'              EOR TOO SOON                                 
*        MERGE PROCESS COUNTERS:                                                
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
*        MERGE PROCESS, ERROR COUNTERS:                                         
CTRERP2F DC    PL8'0'              DIR REPLACING FILE, IGNORED                  
CTRERF2P DC    PL8'0'              FILE REPLACING DIR, IGNORED                  
CTRERDNF DC    PL8'0'              KEY TO DELETE NOT FOUND, IGNORE              
*        MERGE PROCESS, ADDED/REPLACED/DELETED COUNTERS:                        
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
*        WORK COUNTERS:                                                         
IMINORS  DC    PL8'0'              NUMBER OF MINOR KEYS THIS MAJOR              
IDUPKEY  DC    PL8'0'              NUMBER OF TIMES CURRENT KEY DUPED            
IFBYTES  DC    D'0'                INPUT FILE BYTES                             
IDBYTES  DC    D'0'                INPUT DIRECTORY BYTES                        
FMSBYTES DC    D'0'                INPUT BYTE COUNT (FOR ONE F/M/S)             
OTBYTES  DC    D'0'                OFILE BYTES                                  
OTBYTE2  DC    D'0'                OFIL2 BYTES                                  
OTBYTE3  DC    D'0'                OFIL3 BYTES                                  
OTBYTE4  DC    D'0'                OFIL4 BYTES                                  
OMAXRLEN DC    H'0'                OUTPUT MAXIMUM RECORD LENGTH                 
*                                                                               
         EJECT                                                                  
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
*              DCBS, ACBS AND RPLS                                              
*                                                                               
* INPUT FILE OF UPDATE RECORDS IN DANDX FORMAT                                  
IFILE    DCB   DDNAME=IFILE,DSORG=PS,MACRF=(GL),RECFM=VB,EODAD=EOFIN            
*                                                                               
* OUTPUT FILE (FOR INPUT TO IDCAMS REPRO WITH REPLACE):                         
OFILE    DCB   DDNAME=OFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192             
*                                                                               
* FOR DEMVSMN SPLIT FILE:                                                       
OFIL2    DCB   DDNAME=OFIL2,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192             
OFIL3    DCB   DDNAME=OFIL3,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192             
OFIL4    DCB   DDNAME=OFIL4,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192             
*                                                                               
* BACKOUT FILE (TO REVERSE AN UPDATE):                                          
BKFILE   DCB   DDNAME=BKFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192            
*                                                                               
* FOR DEMVSMN SPLIT FILE:                                                       
BKFIL2   DCB   DDNAME=BKFIL2,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192            
BKFIL3   DCB   DDNAME=BKFIL3,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192            
BKFIL4   DCB   DDNAME=BKFIL4,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192            
*                                                                               
*                                                                               
* VSAM DEMO FILE TO BE UPDATED (NOTE, READ-ONLY - SEE OFILE)                    
DEMACB   ACB   AM=VSAM,DDNAME=DEMVSM,MACRF=(KEY,DIR,SKP,IN),           X        
               BUFNI=10,BUFND=16,RMODE31=ALL                                    
DEMRPL   RPL   ACB=DEMACB,AM=VSAM,                                     X        
               AREA=VSAREA,AREALEN=(L'VSAREA),ARG=VSMKEY,              X        
               OPTCD=(KEY,FWD,SYN,NSP,KGE)                                      
*                                                                               
* FOR DEMVSMN SPLIT FILE:                                                       
DEMAC2   ACB   AM=VSAM,DDNAME=DEMVS2,MACRF=(KEY,DIR,SKP,IN),           X        
               BUFNI=10,BUFND=16,RMODE31=ALL                                    
DEMAC3   ACB   AM=VSAM,DDNAME=DEMVS3,MACRF=(KEY,DIR,SKP,IN),           X        
               BUFNI=10,BUFND=16,RMODE31=ALL                                    
DEMAC4   ACB   AM=VSAM,DDNAME=DEMVS4,MACRF=(KEY,DIR,SKP,IN),           X        
               BUFNI=10,BUFND=16,RMODE31=ALL                                    
DEMRP2   RPL   ACB=DEMAC2,AM=VSAM,                                     X        
               AREA=VSAREA,AREALEN=(L'VSAREA),ARG=VSMKEY,              X        
               OPTCD=(KEY,FWD,SYN,NSP,KGE)                                      
DEMRP3   RPL   ACB=DEMAC3,AM=VSAM,                                     X        
               AREA=VSAREA,AREALEN=(L'VSAREA),ARG=VSMKEY,              X        
               OPTCD=(KEY,FWD,SYN,NSP,KGE)                                      
DEMRP4   RPL   ACB=DEMAC4,AM=VSAM,                                     X        
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
LTCIN    DC    C'RECORDS READ',0H'0'                                            
LTCINB   DC    C'BYTES READ, INC RDW',0H'0'            MAX 30 BYTES             
*        FILE RECORDS (I.E. NOT DIRECTORY RECORDS):                             
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
*        DIRECTORY RECORDS:                                                     
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
*        OUTPUT COUNTERS:                                                       
LTCOUT   DC    C'UPDATE OFILE RECORDS WRITTEN',0H'0'                            
LTCOUTB  DC    C'OFILE BYTES WRITTEN, INC RDW',0H'0'   MAX 30 BYTES             
LTCOU2   DC    C'UPDATE OFIL2 RECORDS WRITTEN',0H'0'                            
LTCOU2B  DC    C'OFIL2 BYTES WRITTEN, INC RDW',0H'0'   MAX 30 BYTES             
LTCOU3   DC    C'UPDATE OFIL3 RECORDS WRITTEN',0H'0'                            
LTCOU3B  DC    C'OFIL3 BYTES WRITTEN, INC RDW',0H'0'   MAX 30 BYTES             
LTCOU4   DC    C'UPDATE OFIL4 RECORDS WRITTEN',0H'0'                            
LTCOU4B  DC    C'OFIL4 BYTES WRITTEN, INC RDW',0H'0'   MAX 30 BYTES             
LTCMAXLN DC    C'MAX O/P RECORD LENGTH, INC RDW',0H'0'                          
LTCAVGLN DC    C'AVG O/P RECORD LENGTH, INC RDW',0H'0'                          
LTCBAK   DC    C'BKFILE RECORDS WRITTEN',0H'0'                                  
LTCBA2   DC    C'BKFIL2 RECORDS WRITTEN',0H'0'                                  
LTCBA3   DC    C'BKFIL3 RECORDS WRITTEN',0H'0'                                  
LTCBA4   DC    C'BKFIL4 RECORDS WRITTEN',0H'0'                                  
*        RECORD CHECK WARNINGS AND ERRORS:                                      
LTCRCE01 DC    C'RDW LENGTH TOO SMALL',0H'0'                                    
LTCRCE02 DC    C'DDS LENGTH TOO SMALL',0H'0'                                    
LTCRCE03 DC    C'ELEMENT LENGTH ZERO',0H'0'                                     
LTCRCW01 DC    C'MISSING EOR, ADDED',0H'0'                                      
LTCRCW02 DC    C'EOR BEFORE RECORD END, FIXED',0H'0'                            
*        MERGE PROCESS COUNTERS:                                                
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
*        MERGE PROCESS, ERROR COUNTERS:                                         
LTCERP2F DC    C'PASSIVE REPLACE MAJOR, IGNORED',0H'0'                          
LTCERF2P DC    C'MAJOR REPLACE PASSIVE, IGNORED',0H'0'                          
LTCERDNF DC    C'KEY TO DELETE NOT FOUND, IGNORE',0H'0'                         
*        MERGE PROCESS, ADDED/REPLACED/DELETED COUNTERS:                        
LTCMJ    DC    C'MAJOR KEY SETS',0H'0'                                          
LTCMN    DC    C'MINOR KEYS',0H'0'                                              
LTCPV    DC    C'ORDINARY PASSIVES',0H'0'                                       
LTCPX    DC    C'EXTENDED PASSIVES',0H'0'                                       
*                                                                               
*              ERROR AND WARNING MESSAGES                                       
*                                                                               
LTCWNOIP DC    C'*WARNING* - INPUT FILE IS EMPTY',0H'0'                         
LTCWPDUP DC    C'*WARNING* - POSSIBLE DUPLICATE DATA RELOAD',0H'0'              
LTCMBZ   DC    C'*ERROR* - SHOULD NEVER BE NON-ZERO',0H'0'                      
LTCFLDUP DC    C'*IGNORED* - CAN''T DELETE SINGLE MINORS',0H'0'                 
*                                                                               
LTPSDN   DC    C'SPLIT=Y VALID ONLY WITH FILE=DEMVSMN',0H'0'                    
LTPICC   DC    C'INVALID CONTROL CARD',0H'0'                                    
LTPDPC   DC    C'DUPLICATE CARD',0H'0'                                          
LTPRAB   DC    C'RUN ABORTED DUE TO ABOVE ERRORS',0H'0'                         
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
* IHAASCB                                                                       
* IHAASSB                                                                       
* IAZJSAB                                                                       
         PRINT OFF                                                              
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
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
**PAN#1  DC    CL21'001DEVSUPDT  01/06/21'                                      
         END                                                                    
