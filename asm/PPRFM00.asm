*          DATA SET PPRFM00    AT LEVEL 076 AS OF 03/21/18                      
*PHASE T40C00B                                                                  
*INCLUDE SRCHPASS                                                               
*INCLUDE SRCHCALL                                                               
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
* MHER  8/14    SAP INTERFACE CODE                                              
*                                                                               
* KWAN 09/18/08 HEX SECURITY FOR DISPLAY ONLY                                   
*                                                                               
*  BPLA   4/96     NO-OP ZENITH CHECK                                           
*                                                                               
* BPLA   10/95     ADD ZENITH AGENCY CHECK                                      
*                  REPLACE PGENEROL WITH PARTS THAT I USE.                      
*                                                                               
* BPLA   7/14/95   REPELEM EXPANDED TO 166 BYTES (FROM 164)                     
*                  RECORD LENGTH IS NOW 199                                     
*                  NEW FIELD - PUBLISHER? (Y,N)                                 
*                                                                               
*  BPLA  9/27/91   ADD REP NAME SEARCHING                                       
*  ROSA  12/20/89  ADD FAX NUMBER                                               
*                                                                               
* SWON  12/27/89 UPDATE EDIT ROUTINE TO SUPPORT FAX NUMBER                      
*                                                                               
         TITLE 'T40C00   PRINTPAK  REP MAINTENANCE -BASE'                       
T40C00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T40C00,RR=R9                                               
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         BAS   RE,INITL                                                         
*                                                                               
         L     R1,16(R1)           A(COMFACS)                                   
         ST    R1,ACOMFACS                                                      
*                                                                               
         L     RA,VTWA                                                          
         USING T40CFFD,RA                                                       
*                                                                               
         LA    R9,IOAREA                                                        
         EJECT                                                                  
         XC    REPMSG(60),REPMSG                                                
         TM    REPMEDH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         TM    REPCODEH+4,X'20'                                                 
         BNO   NOTVAL                                                           
         TM    REPACTH+4,X'20'                                                  
         BNZ   CKCOMB                                                           
*                                  CHANGE OF ACTION ONLY                        
         CLC   REPACT(3),=C'CHA'                                                
         BNE   NOTVAL                                                           
         CLI   BACT,3              WAS IT DISPLAY                               
         BNE   NOTVAL                                                           
*                                                                               
         CLC   T40CFFD+12(2),=X'FFFF'                                           
         BE    VALACT10                                                         
         TM    T40CFFD+12,X'01'    DISPLAY ONLY?                                
         BZ    VALACT10                                                         
         LA    R2,REPACTH                                                       
         LA    R3,FACCERR          NOT AUTHORIZED FOR THIS FUNCTION             
         B     ERROR                                                            
*                                                                               
VALACT10 MVI   BACT,2                                                           
         OI    REPACTH+4,X'20'     VALIDATE                                     
         B     CKCOMB                                                           
NOTVAL   NI    REPMEDH+4,X'DF'                                                  
         NI    REPCODEH+4,X'DF'                                                 
         NI    REPACTH+4,X'DF'                                                  
         XC    REPADDR(4),REPADDR                                               
*                                                                               
*   VALIDATE MEDIA                                                              
CKMED    DS    0H                                                               
         LA    R2,REPMEDH                                                       
         BAS   RE,ANY                                                           
         MVC   BMED,REPMED                                                      
         TM    4(R2),X'20'                                                      
         BO    CKREP                                                            
         XC    REPMEDN(10),REPMEDN                                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,X'01'         RECORD CODE                                  
         BAS   RE,HIGH                                                          
         LA    R3,MEDERR                                                        
         CLC   KEYSAVE(25),KEY                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         MVC   APROF,PAGYPROF+16                                                
         MVC   ANATION,PAGYNAT     NATIONALITY                                  
*                                                                               
         MVC   SVCTAGY,SPACES                                                   
***  NO-OP ZENITH CHECK FOR NOW                                                 
         B     CKMED5                                                           
***                                                                             
         LA    R5,PAGYREC+33                                                    
         MVI   ELCODE,X'05'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CKMED5                                                           
         MVC   SVCTAGY,2(R5)                                                    
*                                                                               
CKMED5   DS    0H                                                               
         FOUT  REPMEDNH,PAGYMED,10                                              
         NI    REPCODEH+4,X'DF'                                                 
         NI    REPACTH+4,X'DF'                                                  
         OI    4(R2),X'20'                                                      
         MVI   FORMAT,1            SET ACTION = FORMAT                          
         B     CKREP                                                            
*                                                                               
CKREP    DS    0H                                                               
*                                                                               
*        **NAME SEARCH CALL **                                                  
*                                                                               
         LA    R2,REPCODEH                                                      
         SR    R2,RA               GET DISPL OF KEY FIELD                       
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         MVC   DSPARM(DSPARML),SPACES                                           
         MVC   DSMEDCOD,BMED       SET MEDIA CODE                               
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'REP'),0,RR=RELO                         
*                                                                               
         LA    R2,REPCODEH                                                      
         MVI   BREPSUFX,0                                                       
         LA    R3,REPERR                                                        
         CLI   BMED,C'O'                                                        
         BNE   CKREP6                                                           
         CLI   REPCODE,C'A'                                                     
         BE    CKREP6                                                           
         ZIC   R5,5(R2)                                                         
         LA    R4,REPCODE                                                       
CKREP1   CLI   0(R4),C'.'          SCAN FOR DECIMAL                             
         BE    CKREP2                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,CKREP1                                                        
         B     CKREP6              NO DECIMAL FOUND                             
*                                  TRY FOR NNNN.N (OUTDOOR SUFFIX)              
*                                                                               
CKREP2   ZIC   R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,REPCODE),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
         CP    DUB,=P'999990'                                                   
         BH    ERROR                                                            
*                                                                               
         DP    DUB,=P'100'                                                      
         OI    DUB+5,X'0F'                                                      
         UNPK  BREP(4),DUB(6)                                                   
         CLC   BREP(4),=C'0000'                                                 
         BE    ERROR                                                            
         ZAP   DUB,DUB+6(2)                                                     
         DP    DUB,=P'10'                                                       
         CP    DUB+6(2),=P'0'                                                   
         BNE   ERROR                                                            
*                                 NNNN.0 IS OK                                  
         OI    DUB+5,X'0F'                                                      
         UNPK  BREPSUFX(1),DUB+5(1)                                             
         B     CKREP8                                                           
*                                                                               
CKREP6   DS    0H                                                               
         CLI   REPCODE,C'A'                                                     
         BNE   CKREP7                                                           
         CLI   BMED,C'O'           ONLY FOR OUTDOOR                             
         BNE   ERROR                                                            
         CLI   5(R2),4                                                          
         BNE   ERROR               MUST BE ANNN                                 
         LA    R4,REPCODE+1                                                     
         LA    R5,3                                                             
CKREP6B  CLI   0(R4),C'0'                                                       
         BL    ERROR                                                            
         CLI   0(R4),C'9'                                                       
         BH    ERROR                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,CKREP6B                                                       
         MVC   BREP(4),REPCODE                                                  
         B     CKREP8                                                           
*                                                                               
CKREP7   DS    0H                                                               
         CLI   5(R2),4                                                          
         BH    ERROR                                                            
         ZIC   R5,5(R2)                                                         
         LA    R4,REPCODE                                                       
CKREP7B  CLI   0(R4),C'0'          CHK FOR NUMERICS                             
         BL    ERROR                                                            
         CLI   0(R4),C'9'                                                       
         BH    ERROR                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,CKREP7B                                                       
         BAS   RE,PACK                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  BREP(4),DUB+5(3)                                                 
         LA    R3,REPERR                                                        
         CLC   BREP(4),=C'0000'                                                 
         BE    ERROR                                                            
CKREP8   TM    4(R2),X'20'                                                      
         BO    CKACT                                                            
         NI    REPACTH+4,X'DF'                                                  
         OI    4(R2),X'20'                                                      
         MVI   FORMAT,1                                                         
         B     CKACT                                                            
*  VALIDATE ACTION                                                              
*                                                                               
CKACT    LA    R2,REPACTH                                                       
         LA    R3,ACTERR                                                        
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         LA    R5,ACTIONS                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,RELO                                                          
         LA    R5,6(R5)                                                         
         IC    R4,5(R2)                                                         
         BCTR  R4,R0                                                            
CKACT1   EX    R4,COMP                                                          
         BE    CKACT2                                                           
         BXLE  R5,R6,CKACT1                                                     
         B     ERROR                                                            
CKACT2   MVC   BACT(1),7(R5)                                                    
*                                                                               
         CLC   T40CFFD+12(2),=X'FFFF'                                           
         BE    CKACT3                                                           
         TM    T40CFFD+12,X'01'    DISPLAY ONLY?                                
         BZ    CKACT3                                                           
         CLI   BACT,3              ACTION IS DISPLAY?                           
         BE    CKACT3                                                           
         LA    R3,FACCERR          NOT AUTHORIZED FOR THIS FUNCTION             
         B     ERROR                                                            
*                                                                               
CKACT3   TM    4(R2),X'20'                                                      
         BO    GETOVLY                                                          
*                                                                               
CKCOMB   DS    0H                                                               
         CLC   SVCTAGY,SPACES        SEE IF DOING CODE COORIDINATION            
         BNH   CKCOMB1                                                          
         GOTO1 =A(CHKCTREP),DMCB,(RC),RR=RELO                                   
         CLI   ERRAREA,X'FF'                                                    
         BNE   CKCOMB1                                                          
         NI    REPMEDH+4,X'DF'      UNVALIDATE MEDIA AND EXIT                   
         B     EXIT                                                             
*                                                                               
CKCOMB1  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),BREP                                                    
         MVC   KEY+8(1),BREPSUFX     SUFFIX                                     
         LA    R2,REPCODEH                                                      
         CLI   BACT,4              STND READ                                    
         BNL    CKCOMB2                                                         
         BAS   RE,HIGH                                                          
         CLI   BACT,1                                                           
         BE    CKADD                                                            
         LA    R3,NOREC                                                         
         CLC   KEYSAVE(25),KEY                                                  
         BE    CKCOMB4                                                          
*                                                                               
         CLI   BACT,3                                                           
         BNE   ERROR                                                            
         CLI   APROF,C'0'          DON'T LOOK FOR DEFAULT                       
         BE    ERROR                                                            
*                                                                               
         MVC   KEY(25),KEYSAVE                                                  
CKCOMB2  MVC   KEY(2),=C'ZZ'       IF DISPLAY - THEN TRY FOR ZZ REP             
         LA    R3,NOREC                                                         
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(25),KEY                                                  
         BNE   ERROR                                                            
CKCOMB4  MVC   REPADDR(4),KEY+27                                                
         OI    REPACTH+4,X'20'                                                  
         B     GETOVLY                                                          
*                                                                               
CKADD    LA    R3,DUPKEY                                                        
         CLC   KEYSAVE(25),KEY                                                  
         BE    ERROR                                                            
         XC    REPADDR(4),REPADDR                                               
         MVC   KEY(25),KEYSAVE                                                  
         B     GETOVLY                                                          
*                                                                               
COMP     CLC   8(0,R2),0(R5)       EXECUTED                                     
         EJECT                                                                  
*                                                                               
GETOVLY  XC    IOAREA(250),IOAREA                                               
         CLI   BACT,1                                                           
         BE    EDIT                                                             
         MVC   KEY+27(4),REPADDR                                                
         BAS   RE,GETREC                                                        
         CLI   BACT,2              TEST CHANGE                                  
         BNE   PUTFLDS             NO                                           
         CLI   FORMAT,1                                                         
         BNE   EDIT                                                             
*                                                                               
PUTFLDS  FOUT  REPNAMEH,PREPNAME,30                                             
         FOUT  REPLIN1H,PREPLIN1,30                                             
         FOUT  REPLIN2H,PREPLIN2,30                                             
         FOUT  REPATTNH,PREPATTN,20                                             
         FOUT  REPTELEH,PREPTEL,12                                              
         CLI   PREPELEM+1,152      NEW ELEMENT LENGTH?                          
         BNH   CLEARIT             NO FAX,BRANCH TO CLEAR FIELD                 
         FOUT  REPFAXNH,PREPFAX,12                                              
         B     AROUND                                                           
CLEARIT  FOUT  REPFAXNH,SPACES,12                                               
AROUND   FOUT  REPBKCDH,PREPSTAC,2                                              
*                                                                               
         MVC   WORK(3),=C'NO '                                                  
         TM    PREPSTAT,X'01'      SEE IF PUBLISHER BIT ON                      
         BZ    *+10                                                             
         MVC   WORK(3),=C'YES'                                                  
         FOUT  REPPUBLH,WORK,3                                                  
*                                                                               
         XC    REPIOR,REPIOR                                                    
         FOUT  REPIORH                                                          
         TM    PREPSTAT,X'01'      SEE IF PUBLISHER BIT ON                      
         BZ    PUTF2                                                            
         MVC   WORK(3),=C'NO '                                                  
         TM    PREPSTAT,X'02'      I/O REPEAT CHK                               
         BZ    *+10                                                             
         MVC   WORK(3),=C'YES'                                                  
         FOUT  REPIORH,WORK,3                                                   
*                                                                               
PUTF2    DS    0H                                                               
         CLI   SAPAGY,C'Y'                                                      
         BNE   PUTF3                                                            
*                                                                               
         XC    REPSAP,REPSAP                                                    
         OI    REPSAPH+6,X'80'                                                  
*                                                                               
         LA    R6,PREPELEM                                                      
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),PREPSAPELQ       TEST SAP ELEM                             
         BNE   PUTF3                                                            
         MVC   REPSAP,2(R6)                                                     
*                                                                               
PUTF3    DS    0H                                                               
         CLC   KEY(2),AGYALPHA                                                  
         BE    PUTF4                                                            
         CLI   BACT,5              COPY                                         
         BE    PUTF4                                                            
*                            STANDARD REC SO PROTECT FIELDS                     
         OI    REPNAMEH+1,X'20'                                                 
         OI    REPLIN1H+1,X'20'                                                 
         OI    REPLIN2H+1,X'20'                                                 
         OI    REPATTNH+1,X'20'                                                 
         OI    REPTELEH+1,X'20'                                                 
         OI    REPFAXNH+1,X'20'                                                 
         OI    REPPUBLH+1,X'20'                                                 
         OI    REPIORH+1,X'20'                                                  
         OI    REPBKCDH+1,X'20'                                                 
         FOUT  REPSTNDH,=C'** STND **',10                                       
         B     TRAN                                                             
*                                                                               
PUTF4    DS    0H                                                               
         TM    REPNAMEH+1,X'20'    SEE IF WAS PROTECTED                         
         BZ    PUTFX               NO - NO NEED TO UNPROT AND TRANSMIT          
         FOUT  REPSTNDH,SPACES,10                                               
         NI    REPNAMEH+1,X'DF'    UNPROTECT FIELDS                             
         NI    REPLIN1H+1,X'DF'                                                 
         NI    REPLIN2H+1,X'DF'                                                 
         NI    REPATTNH+1,X'DF'                                                 
         NI    REPTELEH+1,X'DF'                                                 
         NI    REPFAXNH+1,X'DF'                                                 
         NI    REPPUBLH+1,X'DF'                                                 
         NI    REPIORH+1,X'DF'                                                  
         NI    REPBKCDH+1,X'DF'                                                 
         MVC   REPLAST+1(2),=X'0101'   TRANSMIT                                 
PUTFX    CLI   BACT,3                                                           
         BE    DONE                                                             
         CLI   BACT,5                                                           
         BE    COPY                                                             
         LA    R2,REPNAMEH                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
EDIT     DS    0H                                                               
         TM    REPNAMEH+1,X'20'         SEE IF PROTECTED                        
         BZ    EDIT0                                                            
         FOUT  REPNAMEH,SPACES,30                                               
         FOUT  REPLIN1H,SPACES,30                                               
         FOUT  REPLIN2H,SPACES,30                                               
         FOUT  REPATTNH,SPACES,20                                               
         FOUT  REPTELEH,SPACES,12                                               
         FOUT  REPBKCDH,SPACES,2                                                
         FOUT  REPFAXNH,SPACES,12                                               
         FOUT  REPPUBLH,SPACES,3                                                
         FOUT  REPIORH,SPACES,3                                                 
         B     PUTF4          GO UNPROTECT AND SET CURSOR                       
*                                                                               
EDIT0    DS    0H                                                               
*                                                                               
*        FOR CHANGE NEED NAMESEARCH 'BEFORE' CALL                               
*                                                                               
         CLI   BACT,1              SKIP FOR ADD                                 
         BE    EDIT04                                                           
         LA    RF,SRCHBLK                                                       
         USING SEARCHD,RF                                                       
         MVC   SBSYSTEM,=CL6'PRINT'                                             
         MVI   SBID,C' '                                                        
         MVC   SBID+1(L'SBID-1),SBID                                            
         MVC   SBASAVE+02(8),=CL8'PRTDIR'   NAMESEARCH ASSUMES PUB              
         MVC   SBASAVE+10(8),=CL8'PRTFIL'   SO MUST CHANGE TO PRT               
         MVC   SBDODIRN,=Y(2)               THIS IS ITS CONVOLUTED              
         MVC   SBDOFILN,=Y(10)              WAY OF DOING THIS                   
         DROP  RF                                                               
         GOTO1 =V(SRCHPASS),DMCB,(C'B',SRCHBLK),ACOMFACS,IOAREA,       X        
               SRCHLST,REPADDR,0,RR=RELO                                        
*                                                                               
EDIT04   DS    0H                                                               
         LA    R2,REPNAMEH                                                      
         BAS   RE,ANY                                                           
         MVC   PREPNAME,REPNAME                                                 
         LA    R2,REPLIN1H                                                      
         BAS   RE,ANY                                                           
         MVC   PREPLIN1,REPLIN1                                                 
         LA    R2,REPLIN2H                                                      
         BAS   RE,ANY                                                           
         XC    PREPLIN2,PREPLIN2                                                
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         BCTR  R5,R0                                                            
         EX    R5,MVLIN2                                                        
EDIT1    LA    R2,REPATTNH                                                      
         XC    PREPATTN,PREPATTN                                                
         CLI   5(R2),0                                                          
         BE    EDIT2                                                            
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         BCTR  R5,R0                                                            
         EX    R5,MVATTN                                                        
EDIT2    LA    R2,REPTELEH                                                      
         XC    PREPTEL,PREPTEL                                                  
         CLI   5(R2),0                                                          
         BE    EDIT3                                                            
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         BCTR  R5,R0                                                            
         EX    R5,MVTELE                                                        
EDIT3    LA    R2,REPBKCDH                                                      
         XC    PREPSTAC,PREPSTAC                                                
         CLI   ANATION,C'C'             IF CANADIAN FOREIGN BK CODE=90          
         BNE   EDIT3A                                                           
         CLI   BACT,1                   IF ACTION ADD,                          
         BNE   EDIT3A                                                           
         CLI   5(R2),0                  NO INPUT, DEFAULT TO 90                 
         BNE   EDIT3B                                                           
         FOUT  REPBKCDH,=C'90',2                                                
         B     EDIT3B                                                           
EDIT3A   CLI   5(R2),0                                                          
         BE    EDIT4                                                            
EDIT3B   LA    R3,2        INVALID INPUT FIELD                                  
         CLC   8(2,R2),=C'90'            CANADA ONLY                            
         BNE   ERROR                                                            
         MVC   PREPSTAC,8(R2)                                                   
EDIT4    LA    R2,REPFAXNH                                                      
         XC    PREPFAX,PREPFAX           CLEAR OUT FIELD                        
         CLI   5(R2),0                   ANYTHING THERE                         
         BE    EDIT6                     NO- NO FAX,BRANCH                      
         CLC   REPFAXN(3),=C'MB='        EASYLINK MAIL BOX?                     
         BNE   EDIT4A                                                           
         LA    R3,2        INVALID INPUT FIELD                                  
         L     RF,ACOMFACS                                                      
         L     RF,CSCANNER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2),(3,SCANBLK)                                       
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         CLI   SCANBLK+1,X'08'     MUST BE 8 DIGITS                             
         BNE   ERROR                                                            
         TM    SCANBLK+3,X'80'     VALID NUMERIC                                
         BNO   ERROR                                                            
         CLC   SCANBLK+22(2),=C'62'                                             
         BNE   ERROR                                                            
EDIT4A   SR    R5,R5                                                            
         IC    R5,5(R2)                  GET LENGTH                             
         BCTR  R5,R0                                                            
         EX    R5,MVFAX                                                         
*                                                                               
EDIT6    DS    0H                    PUBLISHER?                                 
         LA    R3,2                 SET ERROR TO INVALID                        
         LA    R2,REPPUBLH                                                      
         NI    PREPSTAT,X'FE'       TURN OFF X'01'                              
         CLI   5(R2),0             CHECK FOR INPUT                              
         BE    EDIT6C               GO SEE IF USED AS A PUBLISHER               
         CLI   REPPUBL,C'N'                                                     
         BE    EDIT6C                                                           
         CLI   REPPUBL,C'Y'                                                     
         BNE   ERROR                                                            
         OI    PREPSTAT,X'01'                                                   
         B     EDIT8                                                            
*                                                                               
EDIT6C   DS    0H                                                               
         MVC   SVKEY,KEY           MUST SAVE REP KEY                            
*                                                                               
         XC    KEY,KEY             SEARCH FOR PUBLISHER POINTERS                
         MVC   KEY(2),=C'0P'                                                    
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(1),REPMED                                                  
         MVC   KEY+5(4),BREP                                                    
         MVC   KEY+9(1),BREPSUFX                                                
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(10),KEYSAVE                                                  
         BNE   EDIT6X                                                           
         XC    KEY,KEY                                                          
         MVC   KEY,PREPREC      MUST RESET KEY                                  
         MVC   REPMSG,=CL60'*REP HAS BEEN ASSIGNED AS A PUBLISHER*'             
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
EDIT6X   XC    KEY,KEY                                                          
         MVC   KEY,SVKEY        MUST RESET KEY                                  
         B     EDIT8                                                            
*                                                                               
EDIT8    DS    0H                                                               
         LA    R2,REPIORH          I/O REPEAT CHK? (IF PUBLISHER)               
         NI    PREPSTAT,X'FD'       SET OFF X'02'                               
         CLI   5(R2),0                                                          
         BE    EDITX                                                            
         TM    PREPSTAT,X'01'     SEE IF PUBLISHER                              
         BZ    ERROR                                                            
         CLI   REPIOR,C'N'                                                      
         BE    EDITX                                                            
         CLI   REPIOR,C'Y'                                                      
         BNE   ERROR                                                            
         OI    PREPSTAT,X'02'                                                   
*                                                                               
EDITX    MVC   PREPLEN(2),=H'0199'    MAKE RECLEN 199                           
         MVC   PREPELEM(2),=X'11A6'   MAKE ELEM LEN 166                         
*                                                                               
         CLI   SAPAGY,C'Y'                                                      
         BNE   EDITX2                                                           
*                                                                               
         LA    R6,PREPELEM                                                      
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         MVC   0(2,R6),=X'210C'       TEST SAP ELEM                             
         MVC   2(10,R6),REPSAP                                                  
         ICM   R0,3,PREPLEN                                                     
         AHI   R0,12                                                            
         STCM  R0,3,PREPLEN                                                     
*                                                                               
EDITX2   CLI   BACT,1                                                           
         BE    ADDIT                                                            
         BAS   RE,PUTREC                                                        
         B     ADDIT4                                                           
*                                                                               
*                                                                               
MVLIN2   MVC   PREPLIN2(0),REPLIN2                                              
MVATTN   MVC   PREPATTN(0),REPATTN                                              
MVTELE   MVC   PREPTEL(0),REPTELE                                               
MVFAX    MVC   PREPFAX(0),REPFAXN                                               
*                                                                               
ADDIT    MVC   PREPKEY(25),KEY                                                  
         BAS   RE,ADDREC                                                        
         MVC   REPADDR,KEY         SAVE DISK ADDR                               
*                                                                               
*        NEED NAMESEARCH 'AFTER' OR 'NEW' CALL                                  
*                                                                               
ADDIT4   DS    0H                                                               
         LA    RF,SRCHBLK                                                       
         USING SEARCHD,RF                                                       
         MVC   SBSYSTEM,=CL6'PRINT'                                             
         MVI   SBID,C' '                                                        
         MVC   SBID+1(L'SBID-1),SBID                                            
         MVC   SBASAVE+02(8),=CL8'PRTDIR'   NAMESEARCH ASSUMES PUB              
         MVC   SBASAVE+10(8),=CL8'PRTFIL'   SO MUST CHANGE TO PRT               
         MVC   SBDODIRN,=Y(2)               THIS IS ITS CONVOLUTED              
         MVC   SBDOFILN,=Y(10)              WAY OF DOING THIS                   
         DROP  RF                                                               
         MVI   BYTE,C'A'           'AFTER' FOR CHANGE                           
         CLI   BACT,1                                                           
         BNE   *+8                                                              
         MVI   BYTE,C'N'           'NEW' FOR AZDD                               
         GOTO1 =V(SRCHPASS),DMCB,(BYTE,SRCHBLK),ACOMFACS,IOAREA,       X        
               SRCHLST,REPADDR,0,RR=RELO                                        
*                                                                               
         B     REQ                                                              
*                                                                               
*                                      CHANGE IN PROTECTED STATUS               
TRAN     MVC   REPLAST+1(2),=X'0101'   TRANSMIT SCREEN                          
*                                                                               
DONE     MVC   REPMSG,=CL60'*** ACTION COMPLETED ***'                           
         LA    R2,REPMEDH                                                       
         B     EXIT                                                             
*                                                                               
REQ      DS    0H                                                               
         XC    QCTL,QCTL                                                        
         MVI   QAREA,C' '                                                       
         MVC   QAREA+1(79),QAREA                                                
         MVC   QAREA(2),=C'43'                                                  
         MVC   QAREA+2(2),AGYALPHA                                              
         MVC   QAREA+4(1),REPMED                                                
         MVC   QAREA+52(4),BREP                                                 
         CLI   BREPSUFX,0                                                       
         BE    *+10                                                             
         MVC   QAREA+56(1),BREPSUFX                                             
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,43                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                   
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    DONE                                                             
         SR    R3,R3                                                            
         B     ERROR                                                            
*                                                                               
         SPACE 2                                                                
COPY     DS    0H                                                               
         MVC   PREPKAGY,AGYALPHA                                                
         XC    KEY,KEY                                                          
         MVC   KEY(25),PREPKAGY                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   COPY2                                                            
         LA    R2,REPCODEH                                                      
         LA    R3,DUPKEY                                                        
         B     ERROR                                                            
COPY2    DS    0H                                                               
         BAS   RE,ADDREC                                                        
         B     DONE                                                             
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,RE                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
NEXTELX  LTR   R5,R5                                                            
         BR    RE                                                               
*                                                                               
*******************************************************                         
*                                                                               
         CNOP  2,4                                                              
ACTIONS  DC    H'8'                                                             
         DC    A(ACTIONSX-1)                                                    
         DC    CL7'ADD'                                                         
         DC    X'01'                                                            
         DC    CL7'CHANGE'                                                      
         DC    X'02'                                                            
         DC    CL7'DISPLAY'                                                     
         DC    X'03'                                                            
         DC    CL7'STND'                                                        
         DC    X'04'                                                            
         DC    CL7'COPY'                                                        
         DC    X'05'                                                            
ACTIONSX EQU   *                                                                
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
         SPACE 2                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
         SPACE 2                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         SPACE 2                                                                
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),IOAREA,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBDIR)                     
         SPACE 3                                                                
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* INITIALISATION CODE                                                           
*=============================================================                  
                                                                                
INITL    NTR1  BASE=*,LABEL=*                                                   
         LA    R4,8(RC)            GET START OF AREA TO CLEAR                   
         L     R5,4(RD)            GET PREVIOUS RD VALUE                        
         SR    R5,R4               GIVES LENGTH TO CLEAR                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
*                                                                               
         LM    R2,R4,0(R1)                                                      
*                                                                               
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
*                                                                               
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(36),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
*                                                                               
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         CLI   SAPAGY,0                                                         
         BNE   SETSAPX                                                          
         EJECT                                                                  
*===========================================================                    
* GET THE ACCESS RECORD TO SEE IF SAP AGENCY                                    
*==============================================================                 
                                                                                
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGYALPHA                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',WORK,IOAREA              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SAPAGY,C'N'                                                      
         LA    R4,IOAREA                                                        
         LA    R4,CT5DATA-CT5REC(R4)                                            
         SR    R0,R0                                                            
*                                                                               
SETSAP2  CLI   0(R4),0                                                          
         JE    SETSAP10                                                         
         CLI   0(R4),X'B4'                                                      
         BE    SETSAP4                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     SETSAP2                                                          
*                                                                               
         USING CTAGDD,R4                                                        
*                                                                               
SETSAP4  TM    CTAGOPTS,CTAGSAP    TEST SAP AGY                                 
         JZ    SETSAP10                                                         
         MVI   SAPAGY,C'Y'                                                      
*                                                                               
         MVC   REPSTTL,=CL10'SAP CODE'                                          
         OI    REPSTTLH+6,X'80'                                                 
*                                                                               
         NI    REPSAPH+1,X'FF'-X'20' SET INPUT FIELD UNPROT                     
         OI    REPSAPH+6,X'80'       SET TO XMT                                 
         J     SETSAPX                                                          
         DROP  R4                                                               
                                                                                
*=========================================================                      
* NOT AN SAP AGENCY - CLEAR TITLE AND PROTECT SAP FIELD                         
*=========================================================                      
                                                                                
SETSAP10 XC    REPSAP,REPSAP       SAP INPUT FIELD                              
         OI    REPSAPH+6,X'80'                                                  
         OI    REPSAPH+1,X'20'     SET TO PROTECTED                             
*                                                                               
         XC    REPSTTL,REPSTTL     SAP TITLE                                    
         OI    REPSTTLH+6,X'80'                                                 
*                                                                               
SETSAPX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* FOR CLIENTS DOING CODE CO-ORDINATION, NEED TO SEE IF CODE HAS                 
* BEEN ADDED TO CTFILE                                                          
*===================================================================            
         SPACE 1                                                                
CHKCTREP CSECT                                                                  
         NMOD1 0,CHKCTREP                                                       
         L     RC,0(R1)                                                         
*                                                                               
         CLI   BACT,1              ACTION ADD?                                  
         BNE   CHKX                NO - FORGET IT                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ZENRECD,R4                                                       
         MVI   ZENKCODE,ZENKCODQ                                                
         MVI   ZENKTYP,ZENREPQ                                                  
         MVI   ZENKSYS,C'P'        PRINT                                        
         MVC   ZENKMED,BMED                                                     
         MVC   ZENKAGY,SVCTAGY                                                  
         MVC   ZENKPREP,BREP                                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,PUBIO                    
*                                                                               
         LA    R4,PUBIO                                                         
         USING ZENRECD,R4                                                       
*                                                                               
         CLC   KEY(25),0(R4)                                                    
         BNE   CHKCTERR                                                         
*                                                                               
         LA    R4,ZENFIRST                                                      
         USING ZENELEM,R4                                                       
*                                                                               
         LA    R2,REPNAMEH                                                      
         MVC   8(22,R2),ZENREPNM                                                
         OI    6(R2),X'80'                                                      
         OI    1(R2),X'01'         SET MODIFIED FLAG                            
*                                                                               
         LA    R2,REPLIN1H                                                      
         OI    6(R2),X'80'                                                      
         MVC   8(24,R2),ZENREPAD      (PRINT HAS 30 BYTES)                      
*                                                                               
         LA    R2,REPLIN2H                                                      
         OI    6(R2),X'80'                                                      
*                         PRINT HAS 30 BYTES FOR CITY,STATE,ZIP                 
         XC    8(30,R2),8(R2)                                                   
         MVC   8(24,R2),ZENRADR2                                                
         LA    R1,32(R2)            SACN BACKWARD FOR SPACE                     
CHKC2    CLI   0(R1),C' '                                                       
         BH    CHKC3                                                            
         BCTR  R1,0                                                             
         B     CHKC2                                                            
*                                                                               
CHKC3    MVI   1(R1),C','                                                       
         MVC   2(3,R1),ZENRADR3     STATE CODE                                  
*                                   PUT AFTER CITY                              
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '           SEE IF 3RD BYTE USED                        
         BH    CHKC3X                                                           
         BCTR  R1,0                 BACK-UP ONE BYTE                            
*                                   PUT AFTER CITY                              
CHKC3X   DS    0H                                                               
         LA    R1,1(R1)            FOR SPACE BEFORE ZIP                         
         LA    RE,38(R2)                                                        
         SR    RE,R1                                                            
         BCTR  RE,0                 DECREMENT FOR EXECUTE                       
*                                                                               
         CH    RE,=H'4'             SEE IF ZIP WILL FIT                         
         BNH   *+8                                                              
         LH    RE,=H'4'             YES - MOVE IT ALL                           
         EX    RE,CHKC5                                                         
         B     CHKC8                                                            
*                                                                               
CHKC5    MVC   1(0,R1),ZENRADR4   WILL MOVE AS MUCH OF ZIP AS I CAN             
*                                                                               
CHKC8    OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,REPNAMEH        SET INPUT LENGTHS                             
         LA    R4,30                                                            
CHKC8A   CLI   38(R2),C' '                                                      
         BNH   CHKC8C                                                           
         BCT   R4,CHKC8A                                                        
*                                                                               
CHKC8C   STC   R4,REPNAMEH+5      SET INPUT LENGTH                              
*                                                                               
CHKC9    LA    R2,REPLIN1H        SET INPUT LENGTHS                             
         LA    R4,30                                                            
CHKC9A   CLI   38(R2),C' '                                                      
         BNH   CHKC9C                                                           
         BCT   R4,CHKC9A                                                        
*                                                                               
CHKC9C   STC   R4,REPLIN1H+5      SET INPUT LENGTH                              
*                                                                               
CHKCA    LA    R2,REPLIN2H        SET INPUT LENGTHS                             
         LA    R4,30                                                            
CHKCAA   CLI   38(R2),C' '                                                      
         BNH   CHKCAC                                                           
         BCT   R4,CHKCAA                                                        
*                                                                               
CHKCAC   STC   R4,REPLIN2H+5      SET INPUT LENGTH                              
*                                                                               
CHKX     XIT1                                                                   
         DROP R4                                                                
*                                                                               
CHKCTERR MVC   REPMSG,SPACES                                                    
         MVC   REPMSG(L'NOCTCODE),NOCTCODE                                      
         OI    REPMSGH+6,X'80'                                                  
         MVI   ERRAREA,X'FF'                                                    
         B     CHKX                                                             
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
NOCTCODE DC    C'REP CODE HAS NOT BEEN SET UP ON CONTROL FILE'                  
***********************************************************************         
*                                                                               
       ++INCLUDE GENOLD                                                         
*                                                                               
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PREPREC                                                        
*                                                                               
       ++INCLUDE FLDIND                                                         
         ORG   BYTE2                                                            
FORMAT   DS    CL1                                                              
*                                                                               
         ORG   IOAREA+1000                                                      
*                                                                               
ELCODE   DS    CL1                                                              
SCANBLK  DS    3CL32                                                            
*                                                                               
MEDERR   EQU   13                                                               
REPERR   EQU   122                                                              
ACTERR   EQU   12                                                               
NOREC    EQU   53                                                               
DUPKEY   EQU   52                                                               
FACCERR  EQU   096                 ACCESS TO THE DATA NOT AUTHORIZED            
*                                                                               
SPACES   DS    CL40                                                             
*                                                                               
PUBIO    DS    1000C           CAN BE USED AS ADDITIONAL I/O AREA               
*                                                                               
SVKEY    DS    CL32                                                             
*                                                                               
ACOMFACS DS    A                                                                
SRCHBLK  DS    XL512                                                            
SRCHLST  DS    CL(31*2*8+8+1)      KEYLN*2 TYPES*WORD LENGTH                    
*                                                                               
WORKL    EQU   *-GENOLD                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPRFMFFD                                                       
*                                                                               
         ORG   T40CFFD                                                          
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BREP     DS    CL4                                                              
BREPSUFX DS    CL1                 SUFFIX CODE FOR IOA                          
REPADDR  DS    F                                                                
APROF    DS    CL1                 PAGYPROF+16  ZZ DEFAULT CONTROL              
SVCTAGY  DS    CL2                                                              
SAPAGY   DS    C                                                                
ANATION  DS    CL1                 NATIONALITY                                  
*                                                                               
*                                                                               
*++INCLUDE GESRCHBLKD                                                           
*++INCLUDE PPSRCHPARM                                                           
*++INCLUDE DDCOMFACS                                                            
         PRINT OFF                                                              
       ++INCLUDE GESRCHBLKD                                                     
       ++INCLUDE PPSRCHPARM                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076PPRFM00   03/21/18'                                      
         END                                                                    
