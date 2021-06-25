*          DATA SET ACCLB61    AT LEVEL 234 AS OF 08/16/00                      
*PHASE T62161A                                                                  
*&&      SET   NOP=N                                                            
CLB61    TITLE '- PC COMMS - BUILD BILL HEADER'                                 
***********************************************************************         
* NTRY: AIO1 = BILL HEADER RECORD BUILT SO FAR (MINUS THE KEY)        *         
***********************************************************************         
         SPACE 1                                                                
CLB61    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB61**,R8,CLEAR=YES                                          
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING BHWORKD,RC                                                       
         L     R7,ALINK                                                         
         USING LINKD,R7                                                         
*                                                                               
         L     RE,AIO1             SAVE RECORD BUILT SO FAR                     
         XR    RF,RF                                                            
         ICM   RF,3,BFMRLEN-BFMRECD(RE)                                         
         LA    R0,SAVEIO                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,JOBINI           INITIALIZE FOR JOB                           
         BNE   EXIT                                                             
*                                                                               
         LA    RE,SAVEIO           SAVE RECORD BUILT SO FAR                     
         XR    RF,RF                                                            
         ICM   RF,3,BFMRLEN-BFMRECD(RE)                                         
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,HDRBLD           BUILD BILL HEADER RECORD                     
         BNE   EXIT                                                             
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET INITIAL DATA FOR JOB ETC.                            *         
***********************************************************************         
         SPACE 1                                                                
JOBINI   NTR1  ,                                                                
*                                  CALLS GETOPT & SETS BILLING CURRENCY         
         GOTO1 ASETUP,BOPARM,BEWJOB,0,0                                         
         BNE   EXIT                                                             
*        TM    BCJOBSTA,BCJOBPEN                                                
*        BNZ   *+14                                                             
*        MVC   FVMSGNO,=AL2(AE$ININP)                                           
*        B     EXITN                                                            
*                                  SET FORMAT CODE TO USERS                     
         CLI   BEWFORM,0                                                        
         BE    *+10                                                             
         MVC   CSFORMAT,BEWFORM                                                 
         CLI   BEWLANG,0                                                        
         BE    *+10                                                             
         MVC   CSFMLANG,BEWLANG                                                 
         MVC   BEWFORM,CSFORMAT                                                 
         MVC   BEWLANG,CSFMLANG                                                 
*                                                                               
         CLC   CSBILCUR,CSCPYCUR                                                
         BNE   *+8                                                              
         OI    BEWINDS,BEWICPY     SET BILLING IN COMPANY CURRENCY              
         CLC   CSBILCUR,BCCPYSEC                                                
         BNE   *+8                                                              
         OI    BEWINDS,BEWISEC     SET BILLING IN SECONDARY CURRENCY            
         TM    BEWINDS,BEWICPY+BEWISEC                                          
         BNZ   *+8                                                              
         OI    BEWINDS,BEWIFRGN    SET BILLING IN FOREIGN CURRENCY              
*                                                                               
         PUSH  USING               INIT KEY OF FORMAT HEADER RECORD             
         USING BFMRECD,BEWFMTKY                                                 
         XC    BFMKEY,BFMKEY                                                    
         MVI   BFMKTYP,BFMKTYPQ                                                 
         MVC   BFMKCPY,CUABIN                                                   
         MVI   BFMKSUB,BFMKSUBQ                                                 
         MVC   BFMKFMT,BEWFORM                                                  
         MVC   BFMKLANG,BEWLANG                                                 
         POP   USING                                                            
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD AND ADD BILL HEADER AND PASSIVE POINTER            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
HDRBLD   NTR1  ,                                                                
         L     R4,AIO1             INITIALIZE KEY OF RECORD                     
         USING BEDRECD,R4                                                       
         MVI   BEDKTYP,BEDKTYPQ                                                 
         MVC   BEDKCPY,CUABIN                                                   
         MVI   BEDKSUB,BEDKSUBQ                                                 
         MVC   BEDKJOB,BEWJOB                                                   
*                                                                               
KEY      USING BEDRECD,IOKEY       FIND NEXT SEQUENCE NUMBER                    
         MVC   KEY.BEDKEY,BEDKEY                                                
         GOTO1 AIO,IOHIGH+IORDEL+IOACCDIR                                       
         CLC   KEY.BEDKEY(BEDKJSEQ-BEDKEY),BEDKEY                               
         BNE   *+10                                                             
         MVC   BEDKJSEQ,KEY.BEDKJSEQ                                            
         DROP  KEY                                                              
         XR    RE,RE                                                            
         ICM   RE,3,BEDKJSEQ                                                    
         BCTR  RE,0                                                             
         STCM  RE,3,BEDKJSEQ                                                    
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BEWHDRKY,BEDKEY     SAVE HEADER KEY                              
*                                                                               
         XC    BEWBLH,BEWBLH                                                    
         USING BLHELD,BEWBLH       BUILD BLHELD                                 
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('BLHELQ',BEDRECD),0                 
         CLI   12(R1),0                                                         
         BNE   HBLD02                                                           
         L     RF,12(R1)                                                        
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BLHELD,0(RF)                                                     
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('BLHELQ',BEDRECD),0                 
*                                                                               
HBLD02   MVI   BLHEL,BLHELQ                                                     
         MVI   BLHLN,BLHLN2Q                                                    
         MVC   BLHJOB,BCJOBCOD                                                  
*        MVC   BLHBLNO,   NO                                                    
         MVC   BLHUSER,CUUSER                                                   
         MVC   BLHPERS,CUPASS                                                   
         MVC   BLHLUID,CUTSYM                                                   
         MVC   BLHCRED,ASCDAT                                                   
         XC    BLHBILD,BLHBILD     ZERO FOR DRAFT BILL                          
*                                                                               
         GOTO1 VDATCON,BCDMCB,(2,BCTODAYC),(0,BOWORK1)                          
         XR    RF,RF                                                            
         ICM   RF,1,P#RETDAY       GET NON-STANDARD RETENTION DAYS              
         BNZ   *+8                                                              
         LA    RF,7                DEFAULT IS 7                                 
         GOTO1 VADDAY,BCDMCB,BOWORK1,BOWORK1+6,(RF)                             
         GOTO1 VDATCON,BCDMCB,(0,BOWORK1+6),(2,BLHEXPD)                         
*                                                                               
*        MVC   BLHTRND,BCTODAYC    ?? BILL DATE                                 
*        MVC   BLHDUED,BCTODAYC    ?? DUE DATE                                  
*                                                                               
*        MVC   BLHFORM,BEWFORM                                                  
*        MVC   BLHLANG,BEWLANG                                                  
         MVC   BLHCUR,CSBILCUR                                                  
         MVC   BLHRVAL,CSEXCVAL                                                 
*        L     RF,AGOPBLK                                                       
*        L     RF,GOABEXT-GOBLOCK(RF)                                           
*        ZAP   BLHDSC,GODSCPCT-GOBBLOCK(L'GODSCPCT,RF)                          
*        ZAP   BLHSCH,GOSRGPCT-GOBBLOCK(L'GOSRGPCT,RF)                          
         OI    BLHINDS1,BLHIRTOT   SET TOTALS NEED REFRESHING                   
*                                                                               
         LA    R2,IOKEY                                                         
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(L'BCCPYPRD),BCCPYPRD                                     
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
*                                                                               
         LA    R3,LDGRFST          FIND DRAFT BILL NUMBER ELEMENT               
         USING SCIELD,R3                                                        
         XR    RF,RF                                                            
HBLD12   CLI   SCIEL,0                                                          
         BE    HBLD14                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITDBNO                                                 
         BE    HBLD16                                                           
         IC    RF,SCILN                                                         
         BXH   R3,RF,HBLD12                                                     
*                                                                               
HBLD14   LA    R3,BOELEM           ADD NEW DRAFT BILL NUMBER ELEMENT            
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITDBNO                                                 
         ZAP   SCIAMNT,BCPZERO                                                  
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),LDGRECD,SCIELD,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,16(R1)                                                        
*                                                                               
HBLD16   CP    SCIAMNT,=P'999999'  ADD NUMBER BY ONE                            
         BL    *+10                                                             
         ZAP   SCIAMNT,BCPZERO                                                  
         AP    SCIAMNT,=P'1'                                                    
         UNPK  BODUB1,SCIAMNT                                                   
         MVC   BLHBLNO,BODUB1+2                                                 
         OI    BLHBLNO+L'BLHBLNO-1,C'0'                                         
*                                                                               
P        USING BEDPAS,IOKEY        CHECK DOES NOT ALREADY EXIST                 
         XC    P.BEDPAS,P.BEDPAS                                                
         MVI   P.BEDPTYP,BEDPTYPQ                                               
         MVC   P.BEDPCPY,CUABIN                                                 
         MVI   P.BEDPSUB,BEDPSUBQ                                               
         MVC   P.BEDPBLNO,BLHBLNO                                               
         MVI   P.BEDPIND,BEDPIDFT                                               
         GOTO1 AIO,IOHIGH+IORDEL+IOACCDIR                                       
         CLC   P.BEDPAS(BEDPUSER-BEDPAS),IOKEYSAV                               
         BE    HBLD16              TRY NEXT BILL NUMBER                         
*                                                                               
         USING PBRRECD,IOKEY                                                    
         XC    PBRPAS,PBRPAS       CHECK MAINFRAME PROGRAM BILLS                
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,BLHBLNO                                                 
         MVI   PBRPIND,PBRPIDFT                                                 
         GOTO1 AIO,IOHIGH+IORDEL+IOACCDIR                                       
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BE    HBLD16              TRY NEXT BILL NUMBER                         
*                                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2,R3                                                            
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),BEDRECD,BLHELD,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SCIELQ',BEDRECD),         *        
               (1,=AL1(SCITCBAP))                                               
         CLI   12(R1),0                                                         
         BE    HBLD18              ALREADY ON RECORD                            
         PUSH  USING                                                            
         USING SCIELD,BOELEM       TOTAL NET/COMMISSION                         
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN2Q                                                    
         MVI   SCITYPE,SCITCBAP                                                 
         ZAP   SCIAMNT,BCPZERO                                                  
         ZAP   SCIADMN,BCPZERO                                                  
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),BEDRECD,SCIELD,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
HBLD18   DS    0H                                                               
         GOTO1 APUTRAC,BOPARM,('RACTADD+RACTCHA',BEDRECD)                       
*                                                                               
         GOTO1 AIO,IOADD+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BEWHDRDA,IODA       SAVE HEADER DISK ADDRESS                     
*                                                                               
         XC    IOKEY,IOKEY         ADD PASSIVE POINTER                          
P        USING BEDPAS,IOKEY                                                     
         MVI   P.BEDPTYP,BEDPTYPQ                                               
         MVC   P.BEDPCPY,CUABIN                                                 
         MVI   P.BEDPSUB,BEDPSUBQ                                               
         MVC   P.BEDPBLNO,BLHBLNO                                               
         MVI   P.BEDPIND,BEDPIDFT                                               
         MVC   P.BEDPUSER,BLHUSER                                               
         MVC   P.BEDPJOB,BLHJOB                                                 
         MVC   P.BEDPCRED,BLHCRED                                               
         MVC   P.BEDPBILD,BLHBILD                                               
         MVC   P.BEDPFORM,BLHFORM                                               
         MVC   P.BEDPPERS,BLHPERS                                               
         MVC   P.BEDKEXPD,BLHEXPD                                               
         MVC   P.BEDKBILD,BLHBILD                                               
         MVC   P.BEDKDA,BEWHDRDA                                                
         GOTO1 AIO,IOADD+IOACCDIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  P                                                                
*                                                                               
HDRBLDX  DS    0H                                                               
         MVC   BEWBLNO,BLHBLNO                                                  
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ADDEND   DC    C'ADD=END'                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
* ACCLBFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBFILE                                                      
         PRINT ON                                                               
* ACCLBLINK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBLINK                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
BHWORKD  DSECT                                                                  
SAVEIO   DS    XL2000                                                           
         DS    (OVERWRKL-(*-BHWORKD))X                                          
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'234ACCLB61   08/16/00'                                      
         END                                                                    
