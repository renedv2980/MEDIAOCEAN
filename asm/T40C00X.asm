*          DATA SET T40C00X    AT LEVEL 016 AS OF 05/01/02                      
*          DATA SET T40C00     AT LEVEL 026 AS OF 12/16/83                      
         TITLE 'CHANGE LOG'                                                     
*  ROSA  12/20/89  ADD FAX NUMBER                                  L01          
*                                                                               
***********************\              WHEN LIVE                   L01           
*    ++INCLUDE T40CFFXD *********** REMEMBER TO CHANGE TO T40CFFD               
***********************/              WHEN LIVE                   L01           
*                                                                  L01          
*************\                                                                  
*PHASE T40C00A,+0 ******* NOTE .."A" APPENDED TO PHASE NAME                     
*************/                                                                  
*                                                                               
*                                                                               
* SWON  12/27/89 UPDATE EDIT ROUTINE TO SUPPORT FAX NUMBER         L02          
*                                                                               
         TITLE 'T40C00   PRINTPAK  REP MAINTENANCE -BASE'                       
T40C00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 500,T40C00,RR=R9                                                 
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         BAS   RE,INITL                                                         
         USING T40CFFD,RA                                                       
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
         MVI   BACT,2                                                           
         OI    REPACTH+4,X'20'     VALIDATE                                     
         B     CKCOMB                                                           
NOTVAL   NI    REPMEDH+4,X'DF'                                                  
         NI    REPCODEH+4,X'DF'                                                 
         NI    REPACTH+4,X'DF'                                                  
         XC    REPADDR(4),REPADDR                                               
*                                                                               
*   VALIDATE MEDIA                                                              
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
         FOUT  REPMEDNH,PAGYMED,10                                              
         NI    REPCODEH+4,X'DF'                                                 
         NI    REPACTH+4,X'DF'                                                  
         OI    4(R2),X'20'                                                      
         MVI   FORMAT,1            SET ACTION = FORMAT                          
         B     CKREP                                                            
*                                                                               
CKREP    LA    R2,REPCODEH                                                      
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
         TM    4(R2),X'20'                                                      
         BO    GETOVLY                                                          
CKCOMB   XC    KEY,KEY                                                          
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
MEDERR   EQU   13                                                               
REPERR   EQU   122                                                              
ACTERR   EQU   12                                                               
NOREC    EQU   53                                                               
DUPKEY   EQU   52                                                               
SPACES   DC    CL40' '                                                          
COMP     CLC   8(0,R2),0(R5)       EXECUTED                                     
         EJECT                                                                  
*                                                                               
GETOVLY  XC    IOAREA(250),IOAREA                                               
         CLI   BACT,1                                                           
         BE    EDIT                                                             
         MVC   KEY+27(4),REPADDR                                                
         BAS   RE,GETREC                                                        
         CLI   BACT,2                                                           
         BNE   PUTFLDS                                                          
         CLI   FORMAT,1                                                         
         BNE   EDIT                                                             
*                                                                               
PUTFLDS  FOUT  REPNAMEH,PREPNAME,30                                             
         FOUT  REPLIN1H,PREPLIN1,30                                             
         FOUT  REPLIN2H,PREPLIN2,30                                             
         FOUT  REPATTNH,PREPATTN,20                                             
         FOUT  REPTELEH,PREPTEL,12                                              
         CLI   PREPELEM+1,152      NEW ELEMENT LENGTH?              L02         
         BNH   CLEARIT             NO FAX,BRANCH TO CLEAR FIELD     L02         
         FOUT  REPFAXNH,PREPFAX,12                                  L01         
         B     AROUND                                               L02         
CLEARIT  FOUT  REPFAXNH,SPACES,12                                   L02         
AROUND   FOUT  REPBKCDH,PREPSTAC,2                                  L02         
         CLC   KEY(2),AGYALPHA                                      L02         
         BE    PUTF4                                                            
         CLI   BACT,5              COPY                                         
         BE    PUTF4                                                            
*                            STANDARD REC SO PROTECT FIELDS                     
         OI    REPNAMEH+1,X'20'                                                 
         OI    REPLIN1H+1,X'20'                                                 
         OI    REPLIN2H+1,X'20'                                                 
         OI    REPATTNH+1,X'20'                                                 
         OI    REPTELEH+1,X'20'                                                 
         OI    REPFAXNH+1,X'20'                                    L01          
         OI    REPBKCDH+1,X'20'                                                 
         FOUT  REPSTNDH,=C'** STND **',10                                       
         B     TRAN                                                             
*                                                                               
PUTF4    DS    0H                                                               
         TM    REPNAMEH+1,X'20'    SEE IF WAS PROTECTED                         
         BZ    PUTFX               NO - NO NEED TO UNPROT AND TRANSMIT          
         FOUT  REPSTNDH,SPACES,10                                               
         NI    REPNAMEH+1,X'DF'    UNPROTEST FIELDS                             
         NI    REPLIN1H+1,X'DF'                                                 
         NI    REPLIN2H+1,X'DF'                                                 
         NI    REPATTNH+1,X'DF'                                                 
         NI    REPTELEH+1,X'DF'                                                 
         NI    REPFAXNH+1,X'DF'                                     L01         
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
         FOUT  REPFAXNH,SPACES,12                                  L01          
         B     PUTF4          GO UNPROTECT AND SET CURSOR                       
*                                                                               
EDIT0    DS    0H                                                               
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
         CLI   5(R2),0                                                          
         BE    EDIT4                                                L02         
         LA    R3,2        INVALID INPUT FIELD                                  
         CLC   8(2,R2),=C'90'            CANADA ONLY                            
         BNE   ERROR                                                            
         MVC   PREPSTAC,8(R2)                                                   
EDIT4    LA    R2,REPFAXNH                                          L02         
         XC    PREPFAX,PREPFAX           CLEAR OUT FIELD            L02         
         CLI   5(R2),0                   ANYTHING THERE             L02         
         BE    EDITX                     NO- NO FAX,BRANCH          L02         
         SR    R5,R5                                                L02         
         IC    R5,5(R2)                  GET LENGTH                 L02         
         BCTR  R5,R0                                                L02         
         EX    R5,MVFAX                                             L02         
*                                                                               
EDITX    CLI   BACT,1                                                           
         BE    ADDIT                                                            
         MVC   PREPLEN(2),=H'0197'     MAKE RECORD LENGTH 197     L01           
         MVC   PREPELEM(2),=X'11A4'   MAKE ELEMENT LENGTH 164    L01            
         BAS   RE,PUTREC                                                        
         B     REQ                                                              
*                                                                               
*                                                                               
MVLIN2   MVC   PREPLIN2(0),REPLIN2                                              
MVATTN   MVC   PREPATTN(0),REPATTN                                              
MVTELE   MVC   PREPTEL(0),REPTELE                                               
MVFAX    MVC   PREPFAX(0),REPFAXN                                               
*                                                                               
ADDIT    MVC   PREPKEY(25),KEY                                                  
         MVC   PREPLEN(2),=H'0197'     MAKE RECORD LENGTH 197     L01           
         MVC   PREPELEM(2),=X'11A4'   MAKE ELEMENT LENGTH 164    L01            
         BAS   RE,ADDREC                                                        
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
*                                                                               
       ++INCLUDE PGENEROL                                                       
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
         EJECT                                                                  
*                                                                               
***********************\              WHEN LIVE                   L01           
       ++INCLUDE T40CFFXD *********** REMEMBER TO CHANGE TO T40CFFD             
***********************/              WHEN LIVE                   L01           
         ORG   T40CFFD                                                          
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BREP     DS    CL4                                                              
BREPSUFX DS    CL1                 SUFFIX CODE FOR IOA                          
REPADDR  DS    F                                                                
APROF    DS    CL1                 PAGYPROF+16  ZZ DEFAULT CONTROL              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016T40C00X   05/01/02'                                      
         END                                                                    
