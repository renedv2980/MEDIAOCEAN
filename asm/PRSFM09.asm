*          DATA SET PRSFM09    AT LEVEL 048 AS OF 02/25/15                      
*PHASE T41C09A                                                                  
         TITLE 'T41C09  DISTRICT RECORDS'                                       
*                                                                               
****  CHANGE LOG                                                                
*                                                                               
*  BPLA  02/15     ERREX2 BUG FIX                                               
*                                                                               
*  SMYE  09/02/05  DISALLOW DISTRICT CODE 999 IF REGION IS 999                  
*                                                                               
*  SMYE  09/97     TREAT ALL ENTERED IN DIVISION AND/OR REGION AND/OR           
*                  DISTRICT LIKE NO ENTRY FOR LIST AND REPORT                   
*                                                                               
*  SMYE  05/06/96  ALTERED ELEM LENGTH AND RECORD LENGTH IN VR PROC             
*  **************  (COMMENTED OUT 09/97)  *************************             
*                                                                               
T41C09   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C09                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   EXIT                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
*                                                                               
         BAS   RE,CLRNAME                                                       
*                                                                               
         LA    R2,DIVMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,DIVCLIH          CLIENT                                       
         GOTO1 VALICLT                                                          
         XC    QDIV,QDIV                                                        
         XC    SVDIV,SVDIV                                                      
         LA    R6,SVKEY                                                         
         USING PDSTKEY,R6                                                       
         XC    PDSTKEY,PDSTKEY        CLEAR                                     
         MVC   PDSTKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PDSTKMED,QMED                   MEDIA CODE                       
         MVI   PDSTKRCD,X'05'                  ID                               
         MVC   PDSTKCLT,QCLT                   CLIENT                           
         LA    R2,DIVDIVH          DIVISION                                     
         CLC   8(3,R2),=C'ALL'     ALL ENTERED IN DIVISION ?                    
         BE    VK1D                YES - TREAT LIKE NO ENTRY                    
         CLI   5(R2),0                                                          
         BNE   VK2                                                              
VK1D     CLI   ACTNUM,ACTLIST       NO DIV  - OK IF LIST                        
         BE    VK3                                                              
         CLI   ACTNUM,ACTREP        OK IF REPORT                                
         BE    VK3                                                              
         CLI   ACTNUM,X'11'         OK IF NOW                                   
         BE    VK3                                                              
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK3                                                              
VKMISS   MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK2      GOTO1 VALIDIV                                                          
         CLI   DIVNM,0              MEANS DIVISION NOT FOUND                    
         BE    VKDERR                                                           
         FOUT  DIVDIVNH,DIVNM,20                                                
         MVC   PDSTKDIV,QDIV                                                    
         MVC   SVDIV,QDIV                                                       
*                                                                               
VK3      DS    0H                                                               
         XC    SVREG,SVREG                                                      
         XC    QREG,QREG                                                        
         LA    R2,DIVREGH            REGION                                     
         CLC   8(3,R2),=C'ALL'     ALL ENTERED IN REGION ?                      
         BE    VK3A                YES - TREAT LIKE NO ENTRY                    
         CLI   5(R2),0                                                          
         BNE   VK3C                                                             
VK3A     CLI   ACTNUM,ACTLIST       NO - OK IF LIST                             
         BE    VK3Q                                                             
         CLI   ACTNUM,ACTREP        OK IF REPORT                                
         BE    VK3Q                                                             
         CLI   ACTNUM,X'11'         OK IF NOW                                   
         BE    VK3Q                                                             
         CLI   ACTNUM,ACTADD                                                    
         BE    VKMISS                                                           
         B     VK3Q                                                             
*                                                                               
VK3C     DS    0H                                                               
         LA    R2,DIVDIVH          IF REGION INPUT SO MUST DIV BE               
         CLI   5(R2),0                                                          
         BE    VKMISS                                                           
         LA    R2,DIVREGH                                                       
         GOTO1 VALIREG                                                          
         CLI   REGNM,0              MEANS REGION NOT FOUND                      
         BE    VKDERR                                                           
*                                                                               
         FOUT  DIVREGNH,REGNM,20                                                
         MVC   PDSTKREG,QREG                   REGION                           
         MVC   SVREG,QREG                                                       
*                                                                               
VK3Q     DS    0H                                                               
         XC    SVDST,SVDST                                                      
         XC    QDST,QDST                                                        
         LA    R2,DIVDSTH          DISTRICT                                     
         CLC   8(3,R2),=C'ALL'     ALL ENTERED IN DISTRICT ?                    
         BE    VK3T                YES - TREAT LIKE NO ENTRY                    
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK4                                                              
VK3T     CLI   ACTNUM,ACTLIST      NO, OK IF LIST                               
         BE    VK6                                                              
         CLI   ACTNUM,ACTREP       OK IF REPORT                                 
         BE    VK6                                                              
         CLI   ACTNUM,X'11'        OK IF NOW                                    
         BE    VK6                                                              
         CLI   ACTNUM,ACTADD       SEE IF ADDING                                
         BE    VKMISS              MUST HAVE DISTRICT                           
*                                                                               
VK4      DS    0H                                                               
         LA    R2,DIVREGH                                                       
         CLI   5(R2),0                                                          
         BE    VKMISS                                                           
         LA    R2,DIVDSTH                                                       
         GOTO1 VALIDST                                                          
         CLI   ACTNUM,ACTADD        SEE IF ADDING                               
         BNE   VK5                                                              
         CLI   DSTNM,0              MEANS DISTRICT ALREADY EXISTS               
         BNE   VKDUP                                                            
*                                   DISALLOW 999 DISTRICT FOR REG=999           
         CLC   SVREG,=C'999'        REGION 999 ?                                
         BNE   VK5                  NO                                          
         CLC   QDST,=C'999'         REGION 999 ?                                
         BNE   VK5                  NO                                          
         MVC   CONHEAD(53),=C'*ERROR* - DISTRICT 999 NOT ALLOWED WITH RX        
               EGION 999 **'                                                    
         OI    CONHEADH+6,X'80'    XMIT                                         
         GOTO1 ERREX2                                                           
*                                                                               
VK5      FOUT  DIVDSTNH,DSTNM,20                                                
         MVC   PDSTKDST,QDST                                                    
         MVC   SVDST,QDST                                                       
*****    L     R5,AIO                                                           
*****    USING DSTHDRD,R5                                                       
*****    DROP  R5                                                               
*                                                                               
*                                                                               
VK6      DS    0H                                                               
         MVC   KEY(25),SVKEY       SET KEY                                      
*                                                                               
VK900    B     EXIT                                                             
*                                                                               
VKDERR   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
VKDUP    MVI   ERROR,DUPLICAT                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PDSTREC,R6                                                       
         FOUT  DIVDIVH,PDSTKDIV,3                                               
         FOUT  DIVDIVNH,DIVNM,20                                                
         FOUT  DIVREGH,PDSTKREG,3                                               
         FOUT  DIVREGNH,REGNM                                                   
         FOUT  DIVDSTH,PDSTKDST,3                                               
         FOUT  DIVDSTNH,PDSTNAME                                                
*                                                                               
         LA    R2,DIVDSTH                                                       
         MVC   DIVDST,SPACES                                                    
         MVC   DIVDST(3),PDSTKDST                                               
         MVI   DIVDSTH+5,3                                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         MVI   ACTELOPT,C'N'       SET FOR NO ACTIVITY ELEMS                    
         MVC   SVKEY,KEY           SAVE THE RECORD KEY                          
         L     R6,AIO                                                           
         USING PDSTREC,R6                                                       
         LA    R2,DIVDNAMH                                                      
         CLI   5(R2),0             MUST BE INPUT                                
         BE    VR99                                                             
         CLI   ACTNUM,ACTADD       SEE IF ADDING                                
         BNE   VR50                                                             
         XC    PDSTREC(200),PDSTREC                                             
*****    XC    PDSTREC(67),PDSTREC                                              
         MVC   PDSTREC(16),KEY                                                  
         MVC   PDSTLEN,=H'173'     140 + 33                                     
         MVC   PDSTELEM(2),=X'058C'                                             
*****    MVC   PDSTLEN,=H'65'       32 + 33       NEW RECORD AND                
*****    MVC   PDSTELEM(2),=X'0520'                 ELEMENT LENGTHS             
*                                                                               
VR50     MVC   PDSTNAME,DIVDNAM                                                 
         OC    PDSTNAME,SPACES                                                  
         B     VR900                                                            
*                                                                               
VR99     DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR99C    DS    0H                                                               
         CLI   ERROR,X'FF'         TEST ERROR ALREADY SHOWN                     
         BE    VR99E                                                            
         B     TRAPERR             NO- LET GENCON DO IT                         
*                                                                               
VR99E    GOTO1 ERREX2                                                           
*                                                                               
VR900    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         XC    DIVDNAM,DIVDNAM                                                  
         FOUT  DIVDNAMH                                                         
*                                                                               
         L     R6,AIO                                                           
         USING PDSTREC,R6                                                       
*                                                                               
DR20     DS    0H                                                               
         MVC   DIVDNAM,PDSTNAME                                                 
         FOUT  DIVDNAMH                                                         
DR900    B     EXIT                                                             
         SPACE 3                                                                
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING PDSTREC,R6                                                       
         MVC   AIO,AIO1                                                         
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030                KEY IS LAST RECORD READ                     
*                                   CHECK VS. KEYSAVE                           
*                                                                               
         MVC   PDSTKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PDSTKCLT,QCLT                   CLIENT                           
         MVC   PDSTKMED,QMED                   MEDIA CODE                       
         MVC   PDSTKDIV,QDIV                   DIVISION                         
         MVC   PDSTKREG,QREG                   REGION                           
         MVI   PDSTKRCD,X'05'                  ID                               
         MVC   PDSTKDST,SVDST                                                   
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(7),KEYSAVE     CHECK THROUGH CLIENT                          
         BNE   LR900                                                            
         CLI   KEY+3,X'05'         SEE IF DISTRICT REC                          
         BNE   LR900                                                            
         OC    SVDIV,SVDIV         SEE IF DIVISION GIVEN                        
         BZ    LR040                                                            
         CLC   KEY+7(3),SVDIV                                                   
         BNE   LR900                                                            
*                                                                               
LR040    OC    SVREG,SVREG         SEE IF REGION GIVEN                          
         BZ    LR050                                                            
         CLC   KEY+10(3),SVREG                                                  
         BNE   LR900                                                            
*                                                                               
LR050    DS    0H                                                               
         GOTO1 GETREC              GET THE DST RECORD                           
         L     R6,AIO                                                           
*                                                                               
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    LR090                                                            
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
         MVC   SELHED(19),=C'DIV  REG  DST  NAME'                               
         MVC   0(3,R5),PDSTKDIV                                                 
         MVC   5(3,R5),PDSTKREG                                                 
         MVC   10(3,R5),PDSTKDST                                                
         MVC   15(20,R5),PDSTNAME                                               
DDISP    DS    0H                                                               
         FOUT  SELHEDH                                                          
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR090    DS    0H                  **NB- PRINTREP NOT FULLY CODED               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR020                                                            
*                                                                               
LR900    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* PRINT DISTRICT REPORT                                                         
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
*                                                                               
         LA    R4,KEY              SET UP REGISTER FOR KEY IN DIRECTORY         
         USING PDSTREC,R4                                                       
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   PDSTKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PDSTKCLT,QCLT                   CLIENT                           
         MVC   PDSTKMED,QMED                   MEDIA CODE                       
         MVC   PDSTKDIV,QDIV                   DIVISION                         
         MVC   PDSTKREG,QREG                   REGION                           
         MVI   PDSTKRCD,X'05'                  ID                               
         MVC   PDSTKDST,SVDST                                                   
         DROP  R4                                                               
         MVI   FIRSTT,0                       SET FOR FIRST READ                
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         GOTO1 HIGH                                                             
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(7),KEYSAVE     CHECK THRU CLIENT                             
         BNE   PR110                                                            
         CLI   KEY+3,X'05'                                                      
         BNE   PR110                                                            
         OC    SVDIV,SVDIV         SEE IF DIVISION GIVEN                        
         BZ    PR30C                                                            
         CLC   KEY+7(3),SVDIV                                                   
         BNE   PR110                                                            
*                                                                               
PR30C    OC    SVREG,SVREG         SEE IF REGION GIVEN                          
         BZ    PR30D                                                            
         CLC   KEY+10(3),SVREG                                                  
         BNE   PR110                                                            
*                                                                               
PR30D    CLC   KEY(10),KEYSAVE     SEE IF NEW DIVISION                          
         BNE   PR30D5                                                           
         CLI   FIRSTT,0            SEE IF FIRST TIME                            
         BE    PR30D5                                                           
         B     PR30G                                                            
*                                                                               
PR30D5   MVI   FORCEHED,C'Y'                                                    
         BAS   RE,NEWDIV                                                        
*                                                                               
PR30G    CLC   KEY(13),KEYSAVE     SEE IF NEW REGION                            
         BNE   PR30G5                                                           
         CLI   FIRSTT,0            SEE IF FIRST TIME                            
         BE    PR30G5                                                           
         B     PR31                                                             
*                                                                               
PR30G5   MVI   FORCEHED,C'Y'                                                    
         BAS   RE,NEWREG                                                        
*                                                                               
PR31     MVI   RDUPDATE,C'N'                                                    
         MVI   FIRSTT,1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PDSTREC,R6                                                       
*                                                                               
         MVI   RECFOUND,C'Y'       YES, FOUND DISTRICT REC                      
         MVC   P1(3),PDSTKDST                                                   
         MVC   P1+6(20),PDSTNAME                                                
         MVI   SPACING,2                                                        
*                                                                               
*                                                                               
PR100    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR20                NEXT RECORD ENTRY                            
*                                                                               
PR110    CLI   RECFOUND,C'Y'       REPORT HAS DATA IN IT                        
         BE    PRX                                                              
         MVC   P1(16),=C'NO RECORDS FOUND'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
CLRNAME  XC    DIVDIVN,DIVDIVN                                                  
         XC    DIVREGN,DIVREGN                                                  
         XC    DIVDSTN,DIVDSTN                                                  
         FOUT  DIVDIVNH                                                         
         FOUT  DIVREGNH                                                         
         FOUT  DIVDSTNH                                                         
         BR    RE                  RETURN                                       
*                                                                               
         SPACE 2                                                                
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(1),QMED                                                    
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+15(20),CLTNM                                                  
         MVC   H3+10(3),QCLT                                                    
         MVC   H4(8),=C'DIVISION'                                               
         MVC   H4+15(20),DIVNM                                                  
         MVC   H4+10(3),DISDIV                                                  
         MVC   H5(6),=C'REGION'                                                 
         MVC   H5+15(20),REGNM                                                  
         MVC   H5+10(3),DISREG                                                  
*                                                                               
HOOKX    B     EXIT                                                             
RECFOUND DC    X'0'                                                             
         SPACE 5                                                                
         EJECT                                                                  
NEWDIV   NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
         MVC   SAVEKEYS,KEYSAVE     ALSO SAVE KEYSAVE                           
         MVI   KEY+3,X'03'                                                      
         XC    KEY+10(10),KEY+10                                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'               MISSING DIVISION                              
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PDIVREC,R6                                                       
         MVC   DIVNM,PDIVNAME                                                   
         MVC   DISDIV,PDIVKDIV                                                  
         DROP  R6                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         MVC   KEYSAVE,SAVEKEYS      RESTORE ORIGIONAL KEYSAVE                  
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
NEWREG   NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
         MVI   KEY+3,X'04'                                                      
         XC    KEY+13(10),KEY+13                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'               MISSING REGION RECORD                         
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PREGREC,R6                                                       
         MVC   REGNM,PREGNAME                                                   
         MVC   DISREG,PREGKREG                                                  
         DROP  R6                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,52,C' PRINT DISTRICT REPORT'                                  
         SSPEC H2,52,C'----------------------'                                  
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'CODE  DISTRICT NAME'                                      
         SSPEC H8,1,C'----  -------------'                                      
         DC    X'00'                                                            
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFME9D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMF9D                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
FIRSTT   DS    X                 FIRST TIME FOR REPORT                          
SVCOMP   DS    X                                                                
SVDIV    DS    CL3                                                              
SVREG    DS    CL3                                                              
SVDST    DS    CL3                                                              
DISDIV   DS    CL3                 FOR HEADLINES                                
DISREG   DS    CL3                 FOR HEADLINES                                
SAVEKEY  DS    CL32                                                             
SAVEKEYS DS    CL32               KEYSAVE                                       
X        DS    XL100                                                            
*                                                                               
         EJECT                                                                  
DIVHDRD  DSECT                                                                  
       ++INCLUDE PDIVREC                                                        
REGHDRD  DSECT                                                                  
       ++INCLUDE PREGREC                                                        
DSTHDRD  DSECT                                                                  
       ++INCLUDE PDSTREC                                                        
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048PRSFM09   02/25/15'                                      
         END                                                                    
