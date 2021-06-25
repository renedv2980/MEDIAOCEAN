*          DATA SET PPREP4302  AT LEVEL 041 AS OF 07/06/00                      
*PHASE PP4302C,+0,NOAUTO           **** NOTE "C" PHASE                          
*INCLUDE PUBFLOAT                                                               
         TITLE 'CHANGE LOG'                                                     
*                                                                               
* KWAN 10/99   FILTERING ON STATE CODE (QOPT2 AND QOPT3, 2 DIGITS)              
*                                                                               
* SMYE 8/97    CHANGES TO READ CONTROL FILE FAX RECORDS                         
*                                                                               
* BPLA 1/96    MORE PUBLISHER FEATURES                                          
*                                                                               
* BPLA 7/95    PUBLISHER CHANGES                                                
*                                                                               
* BPLA 9/12/91 SET PAGE TO 1                                                    
*                                                                               
* ROSA 2/20/90 ADD FAX NUMBER                                                   
*                                                                               
***************************************                                         
*        QOPT1 P= PUBLISHERS ONLY                                               
*              X= EXCLUDE PUBLISHERS                                            
*              L= PUBLISHER PUB LISTING                                         
*                                                                               
*        QOPT2 AND QOPT3 USED FOR STATE CODE FILTERING                          
*                                                                               
         TITLE 'REP REPORT'                                                     
         PRINT NOGEN                                                            
PP4302   CSECT                                                                  
         NMOD1 0,PP4302                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LR    R5,RC                                                            
         A     R5,=F'4096'                                                      
         USING PPFILED,RC,R5                                                    
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BNE   EXT                                                              
         EJECT                                                                  
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),=C'A000'                                                
         CLC   QPAY(3),=C'ALL'                                                  
         BE    REPHIGH                                                          
         MVC   KEY+4(4),QPAY                                                    
         CLI   QPAY+4,C' '                                                      
         BE    *+10                                                             
         MVC   KEY+8(1),QPAY+4     SUFFIX CODE                                  
         B     REPHIGH                                                          
*                                                                               
*                                                                               
REPHIGH  GOTO1 HIGH                                                             
         B     REPRD1                                                           
*                                                                               
REPSEQ   GOTO1 SEQ                                                              
*                                                                               
REPRD1   CLC   KEYSAVE(4),KEY                                                   
         BNE   EXT                                                              
         CLC   QPAY(3),=C'ALL'                                                  
         BE    REPRINT                                                          
         CLI   QPAY+4,C' '                                                      
         BE    REPRD4                                                           
         CLC   KEYSAVE+4(5),KEY+4     MATCH REP AND SUFFIX                      
         BE    REPRINT                                                          
         B     EXT                                                              
*                                                                               
REPRD4   DS    0H                                                               
         CLC   KEYSAVE+4(4),KEY+4                                               
         BNE   EXT                                                              
         B     REPRINT                                                          
*                                                                               
REPRINT DS    0H                                                                
         LA    R0,PREPREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         CLC   =C'  ',QOPT2        STATE CODE FILTER PRESENT?                   
         BE    REPRD5                                                           
         CLC   PREPSTAC,QOPT2      MATCH STATE CODE FILTER?                     
         BNE   REPSEQ                                                           
*                                                                               
REPRD5   DS    0H                                                               
*                                                                               
         CLI   QOPT1,C'X'          SEE IF EXLUDING PUBLISHERS                   
         BNE   REPRD6                                                           
         TM    PREPSTAT,X'01'                                                   
         BO    REPRX                                                            
         B     PROCESS                                                          
*                                                                               
REPRD6   CLI   QOPT1,C'P'          PUBLISHERS ONLY                              
         BE    REPRD8                                                           
         CLI   QOPT1,C'L'          OR PUBLISHER LISTING                         
         BNE   PROCESS                                                          
REPRD8   TM    PREPSTAT,X'01'      MUST BE A PUBLISHER                          
         BNO   REPRX                                                            
*                                                                               
PROCESS  DS    0H                                                               
         IC    R7,LINE                                                          
         AHI   R7,3          NEED 3 LINES                                       
         CLI   QOPT1,C'L'    SEE IF PUBLISHER LISTING                           
         BNE   *+8                                                              
         AHI   R7,4          THEN NEED 7 LINES                                  
         STC   R7,SAVELINE                                                      
         CLC   SAVELINE(1),MAXLINES                                             
         BNH   REP1                                                             
         MVI   FORCEHED,C'Y'                                                    
REP1     MVC   P+3(4),PREPKREP                                                  
         CLI   PREPKREP+4,0                                                     
         BE    REP4                                                             
         MVI   P+7,C'.'                                                         
         MVC   P+8(1),PREPKREP+4     SUFFIX                                     
*                                                                               
REP4     DS    0H                                                               
         CLI   QOPT1,C'L'             SEE IF PUBLISHER PUB LISTING              
         BE    PUBL                                                             
         TM    PREPSTAT,X'01'         SEE IF PUBLISHER                          
         BZ    *+8                                                              
         MVI   P+0,C'P'                                                         
*                                                                               
         TM    PREPSTAT,X'02'         SEE IF CHECK FOR REPEAT IS ON             
         BZ    *+8                                                              
         MVI   P+1,C'R'                                                         
*                                                                               
         MVC   P+10(30),PREPNAME                                                
         MVC   P+42(30),PREPLIN1                                                
         BAS   R8,PRINTIT                                                       
         MVC   P+42(30),PREPLIN2                                                
         CLI   PREPELEM+1,152                                                   
*****    BNE   NOFAXNO                                                          
         BNH   NOFAXNO                                                          
         MVC   P+76(3),=C'FAX'                                                  
         MVC   P+87(12),PREPFAX                                                 
*                                                                               
         CLC   =C'FX=',PREPFAX     NUMBER IN CONTROL FILE ?                     
         BNE   NOFAXNO             NO                                           
         MVC   FRSVFAX,PREPFAX                                                  
         OC    FRSVFAX,SPACES                                                   
         BAS   RE,GETFAX           GET FAX FROM CONTROL FILE                    
         MVC   P+101(7),=C'FAX NO='                                             
         MVC   P+108(16),TOFAX                                                  
*                                                                               
NOFAXNO  DS    0H                                                               
         BAS   R8,PRINTIT                                                       
         MVC   P+42(09),=C'ATTENTION'                                           
         MVC   P+53(20),PREPATTN                                                
         MVC   P+76(09),=C'TELEPHONE'                                           
         MVC   P+87(12),PREPTEL                                                 
         BAS   R8,PRINTIT                                                       
*                                                                               
         CLC   PREPSTAC,=C'  '     SPACES IN STATE  CODE?                       
         BE    NOSTACD                                                          
         OC    PREPSTAC,PREPSTAC   NULLS IN STATE CODE?                         
         BZ    NOSTACD                                                          
         MVC   P+76(17),=C'FOREIGN BANK CODE'                                   
*                                                                               
         MVC   P+95(02),PREPSTAC   STATE CODE FOR CANADA                        
*                                                                               
         BAS   R8,PRINTIT                                                       
NOSTACD  BAS   R8,PRINTIT          SKIP A LINE                                  
         B     REPRX                                                            
*                                                                               
         EJECT                                                                  
PUBL     DS    0H                     PUBLISHER PUB LISTING                     
         XC    PLRKEY,PLRKEY          INITIALIZE - USED BY PPUB                 
         MVC   P+09(30),PREPNAME                                                
*                                                                               
         MVC   P+41(2),=C'NO'                                                   
         TM    PREPSTAT,X'02'           I/O REPEAT CHECK                        
         BNO   *+10                                                             
         MVC   P+41(3),=C'YES'                                                  
         BAS   R8,PPUB               FIND A PUB                                 
         BAS   R8,PRINTIT                                                       
*                                                                               
         MVC   P+09(30),PREPLIN1                                                
         BAS   R8,PPUB               FIND A PUB                                 
         BAS   R8,PRINTIT                                                       
         MVC   P+09(30),PREPLIN2                                                
         BAS   R8,PPUB               FIND A PUB                                 
         BAS   R8,PRINTIT                                                       
         MVC   P+09(9),=C'ATTENTION'                                            
         MVC   P+21(20),PREPATTN                                                
         BAS   R8,PPUB               FIND A PUB                                 
         BAS   R8,PRINTIT                                                       
         MVC   P+09(9),=C'TELEPHONE'                                            
         MVC   P+21(12),PREPTEL                                                 
         BAS   R8,PPUB               FIND A PUB                                 
         BAS   R8,PRINTIT                                                       
         MVC   P+09(3),=C'FAX'                                                  
         MVC   P+21(12),PREPFAX                                                 
         BAS   R8,PPUB               FIND A PUB                                 
         BAS   R8,PRINTIT                                                       
*                                                                               
         CLC   =C'FX=',PREPFAX     NUMBER IN CONTROL FILE ?                     
         BNE   PUBL2               NO                                           
         MVC   FRSVFAX,PREPFAX                                                  
         OC    FRSVFAX,SPACES                                                   
         BAS   RE,GETFAX           GET FAX FROM CONTROL FILE                    
         MVC   P+09(7),=C'FAX NO.'                                              
         MVC   P+21(16),TOFAX                                                   
         BAS   R8,PPUB               FIND A PUB                                 
         BAS   R8,PRINTIT                                                       
*                                                                               
PUBL2    IC    R7,LINE                                                          
         AHI   R7,2                                                             
         STC   R7,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BL    PUBL7                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+3(4),PREPKREP                                                  
         CLI   PREPKREP+4,0                                                     
         BE    PUBL5                                                            
         MVI   P+7,C'.'                                                         
         MVC   P+8(1),PREPKREP+4     SUFFIX                                     
*                                                                               
PUBL5    MVC   P+09(11),=C'(CONTINUED)'                                         
*                                                                               
                                                                                
PUBL7    BAS   R8,PPUB               FIND A PUB                                 
         B     PUBL9                                                            
*                                                                               
PUBL8    BAS   R8,PPUB                                                          
PUBL9    CLC   P+60(20),SPACES                                                  
         BE    PUBL10                                                           
         BAS   R8,PRINTIT                                                       
         B     PUBL8                                                            
PUBL10   DS    0H                                                               
         BAS   R8,PRINTIT           SKIP AFTER LAST                             
         B     REPRX                                                            
*                                                                               
REPRX    CLC   QPAY(3),=C'ALL'                                                  
         BNE   EXT                                                              
         B     REPSEQ                                                           
*                                                                               
         EJECT                                                                  
PPUB     DS    0H                                                               
****     CLI   PUBZNAME,C' '     SEE IF I HAVE A ZONE TO PRINT                  
****     BNH   PPUB1                                                            
****     MVC   P+66(20),PUBZNAME                                                
****     XC    PUBZNAME,PUBZNAME     CLEAR                                      
****     B     PPUBXX                                                           
*                                                                               
PPUB1    CLI   PLRKEY,X'FF'      LAST ALREADY READ                              
         BE    PPUBXX                                                           
*                                                                               
         MVC   SVKEY,KEY         SAVE REP KEY                                   
         MVC   KEY,PLRKEY                                                       
         OC    PLRKEY,PLRKEY     SEE IF I HAVE A PUBL PUB KEY                   
         BNZ   PPUB2             YES- THE DO SEQ READ                           
         MVI   KEY,X'F0'                                                        
         MVI   KEY+1,C'P'                                                       
         MVC   KEY+2(3),SVKEY      AGENCY/MEDIA                                 
         MVC   KEY+5(5),SVKEY+4    REP CODE AND SUFFIX                          
PPUB2    GOTO1 HIGHPUB                                                          
         OC    PLRKEY,PLRKEY     SEE IF I HAVE A PUBL PUB KEY                   
         BNZ   PPUB4             YES- THE DO SEQ READ                           
         B     PPUB5                                                            
*                                                                               
PPUB4    GOTO1 SEQPUB                                                           
PPUB5    CLC   KEY(10),KEYSAVE   CHECK THROUGH PUBLISHER                        
         BNE   PPUBL                                                            
         MVC   PLRKEY,KEY        SAVE THIS KEY                                  
         GOTO1 GETNAME           READS INTO PUBREC                              
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PUBKPUB),P+48                                 
         GOTO1 =V(PUBFLOAT),DMCB,PUBREC,P+66                                    
*                                                                               
**       MVC   P+66(20),PUBNAME                                                 
**       CLI   QMEDIA,C'O'                                                      
**       BNE   PPUB5B                                                           
**                                 FOR OUTDOOR USE ST,MKT FOR CITY              
**       LA    RF,P+87                                                          
**       MVC   0(2,RF),PUBSTACD                                                 
**       CLI   PUBSTACD,C' '                                                    
**       BNH   *+12                                                             
**       CLI   PUBSTACD,C'0'                                                    
**       BL    *+10                                                             
**       MVC   0(2,RF),PUBSTATE                                                 
**       MVI   2(RF),C','                                                       
**       MVC   4(20,RF),PUBZNAME                                                
**       XC    PUBZNAME,PUBZNAME                                                
**       B     PPUB5D                                                           
**UB5B   DS    0H                                                               
**       MVC   P+87(16),PUBCITY                                                 
**       MVC   P+105(2),PUBSTATE                                                
**       MVC   P+108(2),PUBSTACD   STATE CODE                                   
**UB5D   DS    0H                                                               
         B     PPUBX                                                            
*                                                                               
PPUBL    MVI   PLRKEY,X'FF'      SET LAST FOUND                                 
PPUBX    MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
PPUBXX   BR    R8                                                               
*                                                                               
PRINTIT  DS    0H                                                               
         CLI   QOPT1,C'L'      SEE IF PUBLISHER PUB LISTING                     
         BNE   PR2                                                              
         MVI   RCSUBPRG,10                                                      
         B     PRX                                                              
*                                                                               
PR2      CLI   QOPT1,C'P'      SEE IF PUBLISHERS ONLY                           
         BNE   PR4                                                              
         MVC   HEAD6+75(17),=C'*PUBLISHERS ONLY*'                               
         B     PRX                                                              
*                                                                               
PR4      CLI   QOPT1,C'X'      SEE IF PUBLISHERS EXCLUDED                       
         BNE   PRX                                                              
         MVC   HEAD6+75(21),=C'*PUBLISHERS EXCLUDED*'                           
         B     PRX                                                              
*                                                                               
PRX      GOTO1 REPORT                                                           
         BR    R8                                                               
         EJECT                                                                  
*                                                                               
*                               GET FAX NUMBER FROM CONTROL FILE                
GETFAX   NTR1                                                                   
         MVC   TOFAX,SPACES                                                     
         MVC   SVKEY,KEY          SAVE KEY                                      
*                                                                               
GETFAXB  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,QAGENCY                                                  
         MVC   CTFXCODE,FRSVFAX+3                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEY,PBUYREC                
         CLC   PBUYREC(18),KEYSAVE      COMPARE 7-BYTE FAX CODE                 
         BNE   GETFAXN                                                          
         LA    R4,PBUYREC       PBUYREC BEING USED FOR CONTROL FILE I/O         
         LA    R5,CTFXEL1                                                       
         B     GETFX4                                                           
         SPACE 1                                                                
GETFX2   ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         SPACE 1                                                                
GETFX4   CLI   0(R5),0                                                          
         BE    GETFAXN                                                          
         CLI   0(R5),CTFX1ELQ                                                   
         BE    GETFXNO                                                          
         B     GETFX2                                                           
         SPACE 1                                                                
         USING CTFX1EL,R5                                                       
GETFXNO  ZIC   R1,CTFX1LEN         FAX NUMBER                                   
         AHI   R1,-3                                                            
         CHI   R1,24                                                            
         BL    *+8                                                              
         LA    R1,24                                                            
         EX    R1,*+8                                                           
         B     GETFAXX                                                          
         MVC   TOFAX(0),CTFX1NUM                                                
         SPACE 1                                                                
*                                                                               
GETFAXN  MVC   TOFAX,=C'** NOT FOUND ** '                                       
*                                                                               
GETFAXX  XC    KEY,KEY                                                          
         MVC   KEY(25),SVKEY       RESTORE KEY                                  
         GOTO1 HIGH                RESTORE SEQ                                  
         CLC   KEY(25),SVKEY       SAME REC ?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO - VERY BAD                                
*                                                                               
         XIT1                                                                   
         DROP  R4,R5                                                            
         SPACE 3                                                                
*                                                                               
SAVELINE DS    CL1                                                              
SVKEY    DS    CL32                                                             
PLRKEY   DS    CL32                                                             
TOFAX    DS    CL16                FAX NUMBER FROM CONTROL FILE                 
FRSVFAX  DS    CL12                FAX IN FORM "FX=XXXXXXX"                     
*                                                                               
EXT      XMOD1 1                                                                
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE CTGENFAX                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041PPREP4302 07/06/00'                                      
         END                                                                    
