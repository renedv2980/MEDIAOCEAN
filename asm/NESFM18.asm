*          DATA SET NESFM18    AT LEVEL 210 AS OF 08/08/18                      
*PHASE T31C18E                                                                  
         TITLE 'NESFM18 - NETFILE MAINT - NTWK PROG REC NAD DEMOS'              
*                                                                               
* NOTES                                                                         
*                                                                               
* THIS OVERLAY IS FOR BOTH CPROG AND DPROG MAINTENANCE.  NESFME6 IS THE         
* SCREEN FOR DPROG.  NESFM82 IS FOR CPROG.  THIS OVERLAY USES ONLY THE          
* NESFME6 SCREEN AS IT IS IDENTICAL EXCEPT FOR THE LABEL OF                     
* "NAD CODE" VS. "COMDEF  ".  MAKE SURE THAT ANY CHANGES MADE TO                
* NESFME6 IS DUPLICATED IN NESFM82. - SCHT 10/2017                              
*                                                                               
*                                                                               
         PRINT NOGEN                                                            
T31C18   CSECT                                                                  
         NMOD1 0,T31C18                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASUBSYSD                                                      
         USING SYSD,R9                                                          
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T31C18,RB,R7                                                     
         MVC   AIO,AIO1                                                         
         MVI   GOAGAIN,C'N'        RESET SWAP SWITCH                            
         L     R3,AIO1                                                          
         USING NPGRECD,R3                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         SPACE 3                                                                
         OI    CONRECH+6,X'01'    SET FIELD MODIFIED BIT                        
         CLI   ACTNUM,ACTADD       ADD ACTION NOT ALLOWED                       
         BE    INVADD                                                           
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   VKCHK                                                            
         MVC   SVDMWORK,DMWORK                                                  
         BAS   RE,PROCPF                                                        
         BAS   RE,VR                                                            
         MVC   DMWORK(96),SVDMWORK                                              
         GOTO1 VSETSPT                                                          
         MVC   KEY,SVKEY                                                        
         B     EXIT1                                                            
VKCHK    CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   DKCHK                                                            
         BAS   RE,PROCPF                                                        
         BAS   RE,VK                                                            
         B     RESET                                                            
DKCHK    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   DRCHK                                                            
         BAS   RE,DK                                                            
         B     RESET                                                            
DRCHK    CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   LRCHK                                                            
         BAS   RE,PROCPF                                                        
         BAS   RE,DR                                                            
         B     RESET                                                            
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   PRCHK                                                            
         BAS   RE,LR                                                            
         B     RESET                                                            
PRCHK    CLI   MODE,PRINTREP       PRINT RECORDS                                
         BNE   PRPFK                                                            
         BAS   RE,LR                                                            
         B     RESET                                                            
PRPFK    CLI   MODE,PROCPFK        PROCESS PF KEYS                              
         BNE   EXIT1                                                            
         BAS   RE,PF                                                            
*                                                                               
RESET    MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
*                                                                               
EXIT     GOTO1 VSETSPT                                                          
EXIT1    XIT1                                                                   
*                                                                               
NPROG    EQU   24                  DPROG RECORD EQUATE                          
DPROG    EQU   24                  DPROG RECORD EQUATE                          
CPROG    EQU   67                  CPROG RECORD EQUATE                          
*                                                                               
*                                                                               
         EJECT                                                                  
VK       NTR1                                                                   
*                                                                               
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=X'0D20'                                                
         SPACE                                                                  
         LA    R2,WORK             * MEDIA                                      
         XC    0(8,R2),0(R2)                                                    
         MVI   8(R2),C'N'          SET UP DUMMY HEADER                          
         GOTO1 VALIMED                                                          
         MVC   SVKEY+2(1),BAGYMD                                                
         MVC   NPGKAM,BAGYMD                                                    
         SPACE                                                                  
         MVI   NOPTFLG,0           SET OPTIONAL FLAG                            
         CLI   ACTNUM,ACTLIST      LIST/PRINT OPTIONAL                          
         BNE   *+8                                                              
         MVI   NOPTFLG,1           SET OPTIONAL FLAG                            
         SPACE                                                                  
         LA    R2,NPRNETH          * NETWORK                                    
         GOTO1 VALIFLD                                                          
         BNZ   VK050                                                            
         MVI   ERROR,INVALID                                                    
         B     EDTERR                                                           
VK050    GOTO1 VALINTWK                                                         
         MVC   NPGKNET,QNETMKT                                                  
         MVC   SVKEY+3(2),QNETMKT                                               
         SPACE                                                                  
VK100    LA    R2,NPRPGRH          * PROGRAM                                    
         GOTO1 VALIFLD                                                          
         BNZ   VK150                                                            
         CLI   NOPTFLG,1                                                        
         BE    VK200                                                            
         MVI   ERROR,INVALID                                                    
         B     EDTERR                                                           
VK150    MVI   SOFTCHK,0                                                        
         CLI   NPREDTH+5,0         WAS DATE INPUTTED                            
         BNE   VK180               YES, FULLY VALIDATE PROGRAM                  
         CLI   NOPTFLG,1           IS ACTION LIST OR PRINT                      
         BNE   VK180               NO, FULLY VALIDATE PROGRAM                   
         MVI   SOFTCHK,1           SOFT VALIDATE THE PROGRAM                    
VK180    GOTO1 VALIPRO                                                          
         MVC   NPGKPROG,NFLD                                                    
         MVC   SVKEY+5(6),NFLD                                                  
         SPACE                                                                  
         SPACE                                                                  
VK200    LA    R2,NPREDTH          * END DATE                                   
         GOTO1 VALIFLD                                                          
         BNZ   VK250                                                            
         CLI   NOPTFLG,1                                                        
         BE    VK300                                                            
         MVI   ERROR,INVALID                                                    
         B     EDTERR                                                           
VK250    GOTO1 VALIDAT                                                          
         GOTO1 DATCON,DMCB,(0,QDATE),(2,SVKEY+11)                               
         MVC   NPGKPROG,SVKEY+11                                                
*                                                                               
         MVC   KEY,SVKEY                                                        
         CLI   ACTNUM,ACTADD                                                    
         BE    VK300                                                            
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                NO/SO CHECK IF END DATE MATCHES              
         CLC   KEY(11),KEYSAVE          A RECORD                                
         BNE   VK300                                                            
         CLC   KEY+11(2),KEYSAVE+11     A RECORD                                
         BNE   ENDATERR                                                         
*                                                                               
VK300    B     EXIT                                                             
*                                                                               
ENDATERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'*** END DATE ERROR - NEXT DATE'                   
         GOTO1 DATCON,DMCB,(2,KEY+11),(5,CONHEAD+31)                            
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
DELMSG   DS    0H                                                               
         LA    R2,CONACTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(43),=C'LAST SELECTION - HIT ENTER TO CONTINUE LIX        
               ST'                                                              
         FOUT  CONHEADH                                                         
         MVC   CONACT(8),=C'SELECT  '                                           
         FOUT  CONACTH                                                          
         GOTO1 ERREX2                                                           
*                                                                               
NODATA   DS    0H                                                               
         LA    R2,NPRNCDH                                                       
         MVI   ERROR,INVALID                                                    
         B     EDTERR                                                           
*        XC    CONHEAD,CONHEAD                                                  
*        MVC   CONHEAD(35),=C'*** INVALID OR MISSING NAD CODE ***'              
*        FOUT  CONHEADH                                                         
*        GOTO1 ERREX2                                                           
*                                                                               
INVADD   DS    0H                                                               
         LA    R2,NPRNCDH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'*** ACTION ADD NOT ALLOWED ***'                   
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
INVLEN   DS    0H                                                               
         LA    R2,NPRNCDH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'*** MAXIMUM OF 7 POSITION NUMERIC ***'            
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
         EJECT                                                                  
DK       NTR1                                                                   
*                                                                               
         MVC   SVKEY,KEY           FIX RESET                                    
*                                                                               
         MVC   NPRNET(4),QNET                                                   
         FOUT  NPRNETH                                                          
*                                                                               
         MVC   NPRPGR(6),KEY+5                                                  
         FOUT  NPRPGRH                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,KEY+11),(5,NPREDT)                                
         FOUT  NPREDTH                                                          
*                                                                               
         GOTO1 VSETSPT                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
LR       NTR1                                                                   
*                                                                               
         BAS   RE,SETOPT                                                        
         MVC   AIO,AIO1                                                         
         MVI   NFILE,C'S'          SPOT FILE                                    
         OC    KEY(17),KEY                                                      
         BNZ   LR100                                                            
         MVC   KEY,SVKEY                                                        
*                                                                               
LR100    GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         B     LR220                                                            
*                                                                               
LR200    GOTO1 VSETSPT                                                          
         GOTO1 SEQ                                                              
LR220    CLC   SVKEY(5),KEY                                                     
         BNE   LREXT                                                            
         MVC   SVKEY,KEY                                                        
         GOTO1 GETREC                                                           
*  CHECK DATE FILTER                                                            
         OC    OPTSPER,OPTSPER                                                  
         BZ    LR225                                                            
         CLC   OPTSPER,KEY+11                                                   
         BH    LR200                                                            
         CLC   OPTEPER,KEY+11                                                   
         BL    LR200                                                            
*  CHECK NAD INFO                                                               
LR225    LA    R4,24(R3)                                                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,NXTEL                                                         
         BNE   LR200                                                            
*                                                                               
         USING NPGEL03,R4                                                       
         CLI   RECNUM,CPROG                                                     
         BNE   LR227                                                            
         CLI   NPGLEN3,NPG3LNQ2                                                 
         JL    LR200                                                            
         OC    NPGCDEF,NPGCDEF                                                  
         JZ    LR200                                                            
         J     LR230                                                            
*                                                                               
LR227    OC    NPGNADDM,NPGNADDM                                                
         BZ    LR200                                                            
*                                                                               
         DROP  R4                                                               
*  CHECK DAYPART FILTER                                                         
LR230    OC    OPTDAYP,OPTDAYP                                                  
         BZ    LR240                                                            
*                                                                               
         LA    R4,24(R3)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,NXTEL                                                         
         BNE   LR200                                                            
*                                                                               
         USING NPGEL93,R4                                                       
*                                                                               
         CLC   OPTDAYP,NPG2DYP                                                  
         BNE   LR200                                                            
         DROP  R4                                                               
*                                                                               
LR240    CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         GOTO1 DATCON,DMCB,(2,NPGKEND),(5,LRDAT)                                
         MVC   LRCODE,NPGKPROG                                                  
*                                                                               
         LA    R4,24(R3)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND 92 ELEM                            
*                                                                               
         USING NPGELEM,R4                                                       
*  CHECK FILTERS FIELD FILTER                                                   
         OC    OPTFILT,OPTFILT                                                  
         BZ    LR250                                                            
         CLC   OPTFILT,NPGFILT                                                  
         BNE   LR200                                                            
*                                                                               
LR250    MVC   LRNAM,NPGNAME                                                    
         GOTO1 UNDAY,DMCB,NPGDAY,LRDAY                                          
         OC    NPGROT,NPGROT                                                    
         BZ    LR280                                                            
         XC    LRDAY,LRDAY                                                      
         GOTO1 UNDAY,DMCB,NPGROT,LRDAY                                          
LR280    GOTO1 UNTIME,DMCB,NPGTIME,LRTIME                                       
         OC    NPGSHARE,NPGSHARE                                                
         BZ    LR320                                                            
         LA    R2,LRSHR                                                         
         TM    NPGSTAT,X'80'                                                    
         BZ    LR300                                                            
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
LR300    DS    0H                                                               
         EDIT  (B2,NPGSHARE),(5,0(R2)),1,ALIGN=LEFT                             
*                                                                               
LR320    OC    NPGPPNO,NPGPPNO                                                  
         BZ    LR500                                                            
         SR    R0,R0                                                            
         ICM   R0,3,NPGPPNO                                                     
         EDIT  (R0),(5,LRNTI),ALIGN=LEFT,FILL=0                                 
*                                                                               
         SPACE                                                                  
LR500    GOTO1 LISTMON                                                          
         B     LR200               GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
* PRINTING THE LINE                                                             
PR       DS    0H                                                               
         MVI   NFILE,C'T'          STATION FILE                                 
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R5,P+10                                                          
         USING PLINED,R5                                                        
         MVC   PRNET,QNET          NETWORK NAME                                 
         MVC   PRCODE,NPGKPROG     PROGRAM CODE                                 
*                                                                               
         MVI   PRDAT+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(2,NPGKEND),(5,PRDAT+9)                              
*                                                                               
         LA    R4,24(R3)                                                        
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND 92 ELEM                            
*                                                                               
         USING NPGELEM,R4                                                       
*  CHECK FILTERS FIELD FILTER                                                   
         OC    OPTFILT,OPTFILT                                                  
         BZ    PR250                                                            
         CLC   OPTFILT,NPGFILT                                                  
         BNE   LR200                                                            
*                                                                               
PR250    MVC   PRNAM,NPGNAME                                                    
         GOTO1 UNDAY,DMCB,NPGDAY,PRDAY                                          
         OC    NPGROT,NPGROT                                                    
         BZ    PR280                                                            
         XC    PRDAY,PRDAY                                                      
         GOTO1 UNDAY,DMCB,NPGROT,PRDAY                                          
PR280    GOTO1 UNTIME,DMCB,NPGTIME,PRTIME                                       
         OC    NPGSHARE,NPGSHARE                                                
         BZ    PR320                                                            
         LA    R2,PRSHR                                                         
         TM    NPGSTAT,X'80'                                                    
         BZ    PR300                                                            
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
PR300    DS    0H                                                               
         EDIT  (B2,NPGSHARE),(5,0(R2)),1,ALIGN=LEFT                             
*                                                                               
PR320    OC    NPGPPNO,NPGPPNO                                                  
         BZ    PR400                                                            
         SR    R0,R0                                                            
         ICM   R0,3,NPGPPNO                                                     
         EDIT  (R0),(5,PRNTI),ALIGN=LEFT,FILL=0                                 
*                                                                               
         DROP  R4                                                               
*                                                                               
PR400    LA    R4,24(R3)                                                        
         MVI   ELCODE,X'93'                                                     
         BAS   RE,NXTEL                                                         
         BNE   PR500                                                            
         USING NPG2ELEM,R4                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,NPG2STD),(5,PRDAT)                                
*                                                                               
PR500    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR200                                                            
         SPACE                                                                  
PREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
NXTEL    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    NXTEL2                                                           
         CLC   ELCODE,0(R4)                                                     
         BER   RE                                                               
         B     NXTEL                                                            
*                                                                               
NXTEL2   LTR   R4,R4                                                            
         BR    RE                                                               
         SPACE 2                                                                
DR       NTR1                                                                   
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   *+10                                                             
         MVC   SVLIST,LISTDIR                                                   
*--CLEAR SCREEN                                                                 
         LA    R2,NPRFSTH                                                       
         LA    R6,42                                                            
DR20     MVI   5(R2),0                                                          
         FOUT  (R2),SPACES,1                                                    
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVI   5(R2),0                                                          
         FOUT  (R2),SPACES,12                                                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVI   5(R2),0                                                          
         FOUT  (R2),SPACES,1                                                    
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVI   5(R2),0                                                          
         FOUT  (R2),SPACES,7                                                    
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R6,DR20                                                          
         XC    KEY,KEY                                                          
*--CHECK NAD CODE READ NAD RECORD                                               
         USING NPGEL03,R6                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR25                                                             
         MVC   WORK+20(6),NPGNADDM                                              
         CLI   RECNUM,CPROG                                                     
         BNE   DR30                                                             
         CLI   1(R6),NPG3LNQ1                                                   
         BE    DR25                                                             
         OC    NPGCDEF,NPGCDEF                                                  
         JZ    DR25                                                             
         MVC   WORK+20(6),NPGCDEF                                               
         B     DR30                                                             
DR25     CLI   NPRNCDH+5,0                                                      
         BE    NODATA                                                           
         MVC   WORK+20(6),NPRNCD                                                
*        OC    WORK+20(6),SPACES                                                
*-GET AGENCY/MEDIA CODE                                                         
DR30     LA    R2,WORK                                                          
         XC    0(8,R2),0(R2)                                                    
         MVI   8(R2),C'N'                                                       
         MVC   AIO,AIO3                                                         
         MVI   BYTE,C'A'           USER SET IO AREA                             
         GOTO1 VALIMED                                                          
         MVC   AIO,AIO3                                                         
*                                                                               
         MVC   KEY(2),=XL2'0D16'                                                
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(6),WORK+20                                                 
         CLI   RECNUM,CPROG                                                     
         BNE   *+8                                                              
         MVI   KEY+9,NNDKRSCQ                                                   
*                                                                               
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NODATA              NOT DEFINED IN NAD DEF REC                   
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY                                                        
*--MOVE NAD CODE TO SCREEN                                                      
         MVC   NPRNCD(6),KEYSAVE+3                                              
         OI    NPRNCDH+4,X'20'     SET PREV VAL BIT                             
         FOUT  NPRNCDH                                                          
*                                                                               
DR100    DS    0H                                                               
*                                                                               
         XC    DBLOCK(256),DBLOCK                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=CL3'NAD'                                                 
         MVI   DBSELMED,C'N'                                                    
         MVC   DBSELAGY,AGENCY                                                  
         LA    R2,NPRFSTH          FIRST DISPLAY FIELD                          
         L     R4,AIO3             NAD DEMO DESCRIPTION RECORD                  
         LA    R5,42               NUMBER OF ENTRIES                            
         L     R6,AIO              NPROG RECORD                                 
         MVI   ELCODE,NPGDELQ                                                   
         CLI   RECNUM,CPROG                                                     
         BNE   *+8                                                              
         MVI   ELCODE,NPGCELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR300                                                            
*                                                                               
         USING NPGCELDD,R6                                                      
DR140    CLI   RECNUM,CPROG                                                     
         BNE   DR150                                                            
         MVI   8(R2),C'T'          DISPLAY MODIFIER                             
         MVI   5(R2),1                                                          
         FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVC   8(L'NPGCCAT,R2),NPGCCAT  DISPLAY DEMO CATEGORY                   
         MVI   5(R2),L'NPGCCAT                                                  
         FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            SKIP + FIELD                                 
         AR    R2,RF                                                            
*                                                                               
         ICM   R0,15,NPGCAMT       DISPLAY DEMO AMOUNT                          
         CVD   R0,DUB                                                           
*        DP    DUB,=P'10'                                                       
*        EDIT  (P6,DUB),(6,8(R2)),0,ALIGN=LEFT                                  
         EDIT  (P8,DUB),(6,8(R2)),0,ALIGN=LEFT                                  
         FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     DR200                                                            
*                                                                               
         USING NPGELDD,R6                                                       
DR150    TM    NPGDFLG,NPGDAEQ     IS THIS AN AE-OVERRIDE?                      
         BO    DR200               YES, SKIP IT. NOT PART OF DPROG              
         DROP  R6                                                               
         MVC   WORK(6),0(R6)                                                    
*********                                                                       
* DECODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,WORK+3),('DEMOCON_16',DUB),(R5),DUB+3           
*********                                                                       
         MVC   8(1,R2),DUB+1       (MODIFIER)                                   
         CLI   8(R2),C'I'          CHECK FOR DEFAULT VPH MODIFIER               
         BNE   *+8                                                              
         MVI   8(R2),C'V'          CHANGE TO VPH                                
         MVI   5(R2),1              LENGTH                                      
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
*                                                                               
         MVC   BYTE,WORK+4         SAVE MODIFIER                                
         MVI   DUB+1,C'I'         DISPLAY AS AN IMPRESSION                      
**********                                                                      
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,DUB),('DEMOCON_17',DUB),(R5),DUB+3              
**********                                                                      
         GOTO1 VDEMCON1,DMCB,(1,DUB),(10,8(R2)),(0,DBLOCK)                      
******** GOTO1 VDEMCON1,DMCB,(1,DUB),(9,8(R2)),(0,DBLOCK)                       
         MVC   5(1,R2),DMCB          LENGTH                                     
         FOUT  (R2)                                                             
         MVC   WORK+4(1),BYTE      RESTORE MODIFIER                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
         GOTO1 HELLO,DMCB,(C'G',=C'SPTFILE'),(ELCODE,0(R4)),(4,WORK+2)          
         CLI   DMCB+12,0           X'00' - ACTION SUCCESSFUL                    
         BE    DR180                                                            
         MVI   8(R2),C'-'   A C'-' INDICATES ELEMENT ON UNIVERSE                
*                           RECORD BUT NOT ON NAD DEMO RECORD, ELEMENT          
*                           WILL BE REMOVED ON NEXT CHANGE ACTION.              
         CLI   DMCB+12,X'06'       X'06' - ELEMENT NOT FOUND                    
         BE    DR180                                                            
         DC    H'0'                                                             
DR180    FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
*                                                                               
         BAS   RE,DISVALUE                                                      
*                                                                               
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
DR200    BAS   RE,NEXTEL                                                        
         BE    DR210                                                            
*                                                                               
         CLI   RECNUM,CPROG                                                     
         BNE   DR300                                                            
         B     DR500               YES - ALL DONE AND EXIT                      
*                                                                               
DR210    L     R4,AIO3                                                          
         BCT   R5,DR140                                                         
*                                                                               
DR300    L     R6,AIO3             NAD DEMO DESCRIPTION RECORD                  
         L     R4,AIO              NPROG RECORD                                 
         MVI   ELCODE,NNDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DR500                                                            
*                                                                               
         USING NNDELDD,R6                                                       
DR310    CLI   RECNUM,CPROG        COMSCORE UNIVERSE?                           
         BNE   DR350                                                            
         MVI   8(R2),C'T'          IMPRESSIONS                                  
         MVI   5(R2),1                                                          
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVC   8(L'NNDCDEMO,R2),NNDCDEMO    DEMO CATEGORY                       
         MVI   5(R2),L'NNDCDEMO                                                 
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     DR390                                                            
*                                                                               
DR350    MVC   WORK(6),0(R6)                                                    
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'SPTFILE'),(ELCODE,0(R4)),(4,WORK+2)          
         MVC   BYTE,DMCB+12                                                     
         CLI   DMCB+12,0                                                        
         BE    DR400                                                            
*********                                                                       
* DECODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,WORK+3),('DEMOCON_16',DUB),(R5),DUB+3           
*********                                                                       
         MVC   8(1,R2),DUB+1       (MODIFIER)                                   
         CLI   8(R2),C'I'          CHECK FOR DEFAULT VPH MODIFIER               
         BNE   *+8                                                              
         MVI   8(R2),C'V'          CHANGE TO VPH                                
         MVI   5(R2),1              LENGTH                                      
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
*                                                                               
         MVI   WORK+4,C'I'         SETUP FOR IMPRESSION DISPLAY                 
**********                                                                      
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,DUB),('DEMOCON_17',DUB),(R5),DUB+3              
**********                                                                      
         GOTO1 VDEMCON1,DMCB,(1,DUB),(10,8(R2)),(0,DBLOCK)                      
*******  GOTO1 VDEMCON1,DMCB,(1,DUB),(9,8(R2)),(0,DBLOCK)                       
         MVC   5(1,R2),DMCB        LENGTH                                       
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVI   8(R2),C'+'   A C'+' INDICATES ELEMENT ON NAD DEMO                
*                           RECORD BUT NOT ON UNIVERSE RECORD, ELEMENT          
*                           WILL BE ADDED ON NEXT CHANGE ACTION.                
         CLI   BYTE,X'06'                                                       
         BE    DR380                                                            
         DC    H'0'                                                             
DR380    FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
DR390    BAS   RE,NEXTEL                                                        
         BNE   DR500                                                            
         CLI   RECNUM,CPROG                                                     
         BE    DR310                                                            
         L     R4,AIO                                                           
         BCT   R5,DR350                                                         
*                                                                               
DR400    BAS   RE,NEXTEL                                                        
         BNE   DR500                                                            
         L     R4,AIO                                                           
         B     DR350                                                            
*                                                                               
DR500    B     EXIT                                                             
*                                                                               
         SPACE 5                                                                
*--DISVALUE CHECKS TO SEE IF THE DEMO IS A RATING,                              
*--IMPRESSION, OR A VPH DOES THE CORRESPONDING                                  
*--EDIT TO DISPLAY THE VALUE.                                                   
*                                                                               
* INPUT                                                                         
* BYTE = R6 (ADDRESS OF DEMO ELEMENT)                                           
* ELEM = R2 (ADDRESS OF SCREEN)                                                 
*                                                                               
*                                                                               
DISVALUE NTR1                                                                   
*                                                                               
*********                                                                       
* DECODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,3(R6)),('DEMOCON_16',DUB),DBLOCK,DUB+3          
*********                                                                       
         CLI   DUB+1,C'V'                                                       
         BE    VPHDIS                                                           
         CLI   DUB+1,C'R'                                                       
         BE    RTGDIS                                                           
         CLI   DUB+1,C'T'                                                       
         BE    IMPDIS                                                           
         CLI   DUB+1,C'H'                                                       
         BE    IMPDIS                                                           
         DC    H'0'                                                             
         SPACE 3                                                                
VPHDIS   L     R0,8(R6)                                                         
         LTR   R0,R0                                                            
         BZ    VPHDEX                                                           
         CVD   R0,DUB                                                           
*        DP    DUB,=P'10'                                                       
         EDIT  (P8,DUB),(6,8(R2)),0,ALIGN=LEFT                                  
         MVI   5(R2),6                                                          
         FOUT  (R2)                                                             
VPHDEX   B     EXIT                                                             
         SPACE 3                                                                
RTGDIS   L     R0,8(R6)                                                         
         LTR   R0,R0                                                            
         BZ    RTGDEX                                                           
         CVD   R0,DUB                                                           
         CLI   7(R6),X'82'         2 DEC PRECISSION                             
         BE    RTGD100                                                          
         EDIT  (P8,DUB),(6,8(R2)),1,ALIGN=LEFT                                  
         MVI   5(R2),7                                                          
         FOUT  (R2)                                                             
         B     EXIT                                                             
RTGD100  EDIT  (P8,DUB),(6,8(R2)),2,ALIGN=LEFT                                  
         MVI   5(R2),7                                                          
         FOUT  (R2)                                                             
RTGDEX   B     EXIT                                                             
         SPACE 3                                                                
IMPDIS   L     R1,8(R6)                                                         
         LTR   R1,R1                                                            
         BZ    IMPDEX                                                           
         CLI   7(R6),X'42'         CABLE PRECISSION                             
         BE    IMPD100                                                          
*                                                                               
         SR    R0,R0                                                            
         A     R1,=F'5'                                                         
         D     R0,=F'10'                                                        
*                                                                               
         LTR   R1,R1                                                            
         BNZ   IMPD10                                                           
         LR    R1,R0                                                            
         S     R1,=F'5'                                                         
         B     IMPD20                                                           
IMPD10   SR    R0,R0                                                            
         M     R0,=F'10'                                                        
IMPD20   CVD   R1,DUB                                                           
         EDIT  (P8,DUB),(7,8(R2)),0,ALIGN=LEFT                                  
         MVI   5(R2),7                                                          
         FOUT  (R2)                                                             
         B     EXIT                                                             
IMPD100  CVD   R1,DUB                                                           
         EDIT  (P8,DUB),(7,8(R2)),1,ALIGN=LEFT                                  
         MVI   5(R2),7                                                          
         FOUT  (R2)                                                             
IMPDEX   B     EXIT                                                             
         EJECT                                                                  
VR       NTR1                                                                   
         XC    ELEM(200),ELEM                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   VR10                                                             
         USING NPGEL03,R6                                                       
         CLI   NPGLEN3,NPG3LNQ1    OLD LENGTH?                                  
         JH    VR05                                                             
         MVC   ELEM(NPG3LNQ1),0(R6)    YES - SAVE IT AND READD NEW LEN          
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(X'03',AIO),0                      
         B     VR10                                                             
*                                                                               
VR05     CLI   RECNUM,CPROG                                                     
         BNE   *+14                                                             
         MVC   NPGCDEF,NPRNCD                                                   
         B     *+10                                                             
         MVC   2(6,R6),NPRNCD                                                   
         B     VR50                                                             
         DROP  R6                                                               
*--CREATE 03 ELEMENT                                                            
VR10     L     R6,AIO                                                           
*                                                                               
         LA    RF,ELEM                                                          
         USING NPGEL03,RF                                                       
         MVI   NPGSPEL,NPG3ELQ                                                  
         MVI   NPGLEN3,NPG3LNQ2                                                 
*                                                                               
         CLI   RECNUM,CPROG                                                     
         BNE   *+14                                                             
         MVC   NPGCDEF,NPRNCD      COMSCORE DEMO DEFINITION CODE                
         B     *+10                                                             
         MVC   NPGNADDM,NPRNCD     NAD DEMO DEFINITION CODE                     
         GOTO1 ADDELEM                                                          
         DROP  RF                                                               
*                                                                               
VR50     L     R6,AIO                                                           
*        MVI   ELCODE,X'DD'        DELETE DEMO ELEMNTS                          
*        GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R6)),0                   
         BRAS  RE,DELDDEL          DELETE 'DD' ELEMS NOT AE-OVERRIDES           
         TM    NPRNCDH+4,X'20'     WAS NAD CODE CHANGED                         
         BO    VR100               NO                                           
         B     DR                                                               
*                                                                               
VR100    DS    0H                                                               
         LA    R2,NPRFSTH                                                       
         LA    R5,42               NUMBER OF ENTRIES                            
         XC    ELEM(200),ELEM                                                   
*                                                                               
         MVI   ELEM,NPGDELQ        X'DD'                                        
         MVI   ELEM+1,NPGDLNQ                                                   
         CLI   RECNUM,CPROG                                                     
         BNE   *+12                                                             
         MVI   ELEM,NPGCELQ        X'DE'                                        
         MVI   ELEM+1,NPGCLNQ                                                   
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=CL3'NAD'                                                 
         MVI   DBSELMED,C'N'                                                    
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDEMVAL1,CDEMOVAL                                                
         DROP  1                                                                
*                                                                               
VR200    CLI   5(R2),0             CHECK END OF LIST                            
         BE    VR500                                                            
*                                                                               
         CLI   RECNUM,CPROG                                                     
         BNE   VR210                                                            
         MVC   BYTE,8(R2)          MODIFIER                                     
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         LA    RF,ELEM                                                          
         USING NPGCELDD,RF                                                      
         MVC   NPGCCAT,8(R2)       DEMO CATEGORY                                
         DROP  RF                                                               
*                                                                               
         ZIC   RF,0(R2)            BUMP TO VALUE (SKIP + FIELD)                 
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         LR    R4,R2                                                            
         B     VR280                                                            
*                                                                               
VR210    LR    R4,R2                                                            
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         CLI   8(R4),C'-'          IF INDICATOR EQUALS C'-'                     
         BNE   VR250               DONT WRITE TO RECORD                         
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         LR    R2,R4                                                            
         B     VR300                                                            
*--MOVE IN THE DEMO                                                             
VR250    MVC   BYTE,8(R2)          MODIFIER                                     
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         MVC   WORK,0(R2)                                                       
         MVI   WORK+1,0            REMOVE PROTECT BIT                           
         GOTO1 VDEMVAL1,DMCB,(1,WORK),(1,ELEM+3),(0,DBLOCK)                     
         CLI   DMCB+4,0                                                         
         BE    BADDEM                                                           
*                                                                               
*********                                                                       
* DECODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,ELEM+3),('DEMOCON_16',DUB),(R5),DUB+3           
*********                                                                       
         MVC   DUB+1(1),BYTE       SET THE MODIFIER                             
**********                                                                      
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,DUB),('DEMOCON_17',ELEM+3),(R5),DUB+3           
**********                                                                      
*                                                                               
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         LR    R2,R4                                                            
*                                                                               
         LA    RE,PRECTAB                                                       
         LA    RF,7                                                             
*                                                                               
VR270    CLC   BYTE,0(RE)                                                       
         BE    VR275                                                            
         LA    RE,2(RE)                                                         
         BCT   RF,VR270                                                         
         DC    H'0'                INVALID MODIFIER                             
VR275    MVC   ELEM+7(1),1(RE)     SET DEMO PRECISSION                          
*                                                                               
         MVI   ELEM+6,X'80'        SET DEMO FOR NAD                             
                                                                                
VR280    BAS   RE,VALVALUE         VALIDATE THE DEMO AMOUNT                     
*                                                                               
VR300    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R5,VR200                                                         
*                                                                               
VR500    BAS   RE,ACTIVITY                                                      
         BAS   RE,DR                                                            
         B     EXIT                                                             
         EJECT                                                                  
*--VALVALUE CHECKS TO SEE IF THE DEMO IS A RATING,                              
*--IMPRESSION, OR A VPH DOES THE CORRESPONDING                                  
*-- VALIDATION AND FILLS IN THE REST OF THE ELEMENT.                            
*                                                                               
* INPUT                                                                         
* BYTE = MODIFIER (R,T,H,V)                                                     
* ELEM = PARTIALLY FILLED ELEMENT AREA                                          
*                                                                               
* OUTPUT                                                                        
* ELEM = FULLY FILLED ELEMENT AREA                                              
*                                                                               
VALVALUE NTR1                                                                   
         CLI   RECNUM,CPROG                                                     
         BE    COMVAL                                                           
*                                                                               
         CLI   BYTE,C'V'                                                        
         BE    VPHVAL                                                           
         CLI   BYTE,C'R'                                                        
         BE    RTGVAL                                                           
         CLI   BYTE,C'T'                                                        
         BE    IMPVAL                                                           
         CLI   BYTE,C'H'                                                        
         BE    IMPVAL                                                           
         DC    H'0'                                                             
         SPACE 3                                                                
*--VPH VALIDATION AREA                                                          
*--MOVE IN THE AMOUNT                                                           
VPHVAL   MVC   ELEM+8(4),=XL4'00000000'                                         
         CLI   5(R4),0             CHECK NO INPUT                               
         BE    VPHV270                                                          
         ZIC   R5,5(R4)                                                         
         GOTO1 CASHVAL,DMCB,8(R4),(R5)                                          
         CLI   DMCB,0                                                           
         BNE   EDTERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BE    VPHV270             WRITE THE ZERO OVERRIDE                      
         BL    EDTERR              CAN'T BE NEGATIVE                            
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   EDTERR              NO DECIMAL                                   
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,15,ELEM+8                                                     
VPHV270  GOTO1 ADDELEM                                                          
         B     EXIT                                                             
         SPACE 3                                                                
*--RATING VALIDATION AREA                                                       
*--MOVE IN THE AMOUNT                                                           
RTGVAL   MVC   ELEM+8(4),=XL4'00000000'                                         
         CLI   5(R4),0             CHECK NO INPUT                               
         BE    RTGV270                                                          
         CLI   5(R4),7                                                          
         BH    INVLEN                                                           
         ZIC   R5,5(R4)                                                         
         CLI   ELEM+7,X'82'        CHECK FOR 2 DEC PRECISSION                   
         BNE   RTGV100                                                          
         GOTO1 CASHVAL,DMCB,(2,8(R4)),(R5)                                      
         B     RTGV120                                                          
RTGV100  GOTO1 CASHVAL,DMCB,(1,8(R4)),(R5)                                      
RTGV120  CLI   DMCB,0                                                           
         BNE   EDTERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BE    RTGV270             WRITE THE ZERO OVERRIDE                      
         STCM  R0,15,ELEM+8                                                     
RTGV270  GOTO1 ADDELEM                                                          
         B     EXIT                                                             
         SPACE 3                                                                
*--IMPRESSION VALIDATION AREA                                                   
*--MOVE IN THE AMOUNT                                                           
IMPVAL   MVC   ELEM+8(4),=XL4'00000000'                                         
         CLI   5(R4),0             CHECK NO INPUT                               
         BE    IMPV270                                                          
         CLI   5(R4),7                                                          
         BH    INVLEN                                                           
         ZIC   R5,5(R4)                                                         
         CLI   ELEM+7,X'42'        CHECK FOR 1 DEC PRECISSION                   
         BNE   IMPV100                                                          
         GOTO1 CASHVAL,DMCB,(1,8(R4)),(R5)                                      
         B     IMPV120                                                          
IMPV100  GOTO1 CASHVAL,DMCB,(0,8(R4)),(X'40',(R5))                              
IMPV120  CLI   DMCB,0                                                           
         BNE   EDTERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BE    IMPV270             WRITE THE ZERO OVERRIDE                      
         STCM  R0,15,ELEM+8                                                     
IMPV270  GOTO1 ADDELEM                                                          
         B     EXIT                                                             
*                                                                               
COMVAL   CLI   5(R4),0             CHECK NO INPUT                               
         BE    IMPV270                                                          
         CLI   5(R4),7                                                          
         BH    INVLEN                                                           
         ZIC   R5,5(R4)                                                         
*        GOTO1 CASHVAL,DMCB,8(R4),(R5)                                          
         GOTO1 CASHVAL,DMCB,(0,8(R4)),(X'40',(R5))                              
         CLI   DMCB,0                                                           
         BNE   EDTERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BE    COMVALX             WRITE THE ZERO OVERRIDE                      
*        CVD   R0,DUB                                                           
*        DP    DUB,=P'1000'                                                     
*        CP    DUB+5(3),=P'0'      MUST GET ZERO REMAINDER                      
*        BNE   EDTERR              NO DECIMAL OR ONES                           
*        CVD   R0,DUB              I.E. NEAREST 10,000                          
*        DP    DUB,=P'10'                                                       
*        MVC   WORK(6),DUB                                                      
*        ZAP   DUB,WORK(6)                                                      
*        CVB   R0,DUB                                                           
*                                                                               
         LA    RF,ELEM                                                          
         USING NPGCELDD,RF                                                      
         STCM  R0,15,NPGCAMT                                                    
         DROP  RF                                                               
*                                                                               
COMVALX  GOTO1 ADDELEM                                                          
         B     EXIT                                                             
         EJECT                                                                  
*--PFKEY MODE                                                                   
PF       NTR1                                                                   
         CLI   PFAID,0                                                          
         BE    EXIT                                                             
         CLI   PFAID,12                                                         
         BNE   PF50                                                             
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   *+10                                                             
         MVC   LISTDIR,SVLIST                                                   
         B     EXIT                                                             
PF50     OI    GENSTAT2,X'08'      SAVE THE SCREEN                              
         BAS   RE,PROCPF                                                        
         B     EXIT                                                             
         EJECT                                                                  
ACTIVITY NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM            DELETE OLD DESC                               
         XC    ELEM(10),ELEM                                                    
         MVC   ELEM(2),=X'0108'                                                 
         GOTO1 DATCON,DMCB,(5,0),(3,ELEM+2)                                     
         MVC   ELEM+5(1),CONACT    SAVE ACTION                                  
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                      PROCESS PF KEYS                                *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         LA    R2,NPRNETH                                                       
         SPACE 1                                                                
PROCPFA  CLI   PFAID,PF3           PF3 FOR NPROG ACTION CHANGE                  
         BE    PROCPF4                                                          
         CLI   PFAID,PF2           PF2 FOR NPROG ACTION DISPLAY                 
         BE    PROCPF1                                                          
         CLI   PFAID,PF8           PF8 FOR NAD DEF ACTION DISPLAY               
         BE    PROCPF6                                                          
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF1  LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         SPACE 1                                                                
PROCPF2  MVI   PFAID,0                                                          
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   PROCPF3                                                          
         GOTO1 VCALL,WORK,=C'PROGRAM',,(4,NPRNET),(6,NPRPGR),          X        
               (8,NPREDT),0                                                     
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF3  GOTO1 VTRANSF,WORK,=C'PROGRAM',,(4,NPRNET),(6,NPRPGR),        X        
               (8,NPREDT),0                                                     
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF4  LA    R1,=C'CHA'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         B     PROCPF2                                                          
         SPACE 1                                                                
PROCPF6  LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         SPACE 1                                                                
         MVI   PFAID,0                                                          
         GOTO1 VCALL,WORK,=C'DEMDEF  ',,(6,NPRNCD),0                            
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPFX  B     EXIT                                                             
         EJECT                                                                  
*  SET OPTION FILTERS FOR LIST SCREEN                                           
*                                                                               
SETOPT   NTR1                                                                   
         XC    OPTSPER,OPTSPER                                                  
         XC    OPTFILT,OPTFILT                                                  
         XC    OPTDAYP,OPTDAYP                                                  
*                                                                               
         LA    R2,LPROPTH          * OPTIONS                                    
         GOTO1 VALIFLD                                                          
         BZ    EXIT                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BE    STO310                                                           
         CLI   ACTNUM,ACTLIST                                                   
         BNE   EDTERR                                                           
STO310   L     R4,AIO2                                                          
         USING SCAND,R4                                                         
         XC    DMCB,DMCB                                                        
         GOTO1 SCANNER,DMCB,(20,(R2)),(4,0(R4))                                 
         CLI   DMCB+4,0                                                         
         BE    EDTERR                                                           
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,DMCB+4                                                      
*                                                                               
STO320   CLI   FLD1,C'F'                                                        
         BE    STO360                                                           
*                                                                               
         CLI   FLD1,C'D'                                                        
         BE    STO370                                                           
*                                                                               
         CLI   FLD1,C'P'                                                        
         BNE   EDTERR                                                           
* VALIDATE THE DATE FIELD                                                       
*                                                                               
         XC    SCANAREA,SCANAREA                                                
         MVC   SCANAREA+5(1),FLD2LEN                                            
         MVC   SCANAREA+8(20),FLD2                                              
         LA    R2,WORK2                                                         
         GOTO1 SCANNER,DMCB,SCANAREA,(2,0(R2)),C',=,-'                          
         CLI   4(R1),0                                                          
         BE    EDTERR                                                           
         CLI   1(R2),0             ARE 2 DATES INPUTTED                         
         BNE   STO330                                                           
         MVC   WORK2+1(1),WORK2                                                 
         MVC   WORK2+8(4),WORK2+4                                               
         MVC   WORK2+22(10),WORK2+12                                            
STO330   CLI   0(R4),0             TEST FIRST DATE GIVEN                        
         BE    EDTERR                                                           
         GOTO1 DATVAL,DMCB,(0,12(R2)),(0,50(R2))                                
         OC    0(4,R1),0(R1)                                                    
         BZ    EDTERR                                                           
         CLC   3(1,R1),0(R2)                                                    
         BNE   EDTERR                                                           
STO335   CLI   1(R4),0             TEST SECOND DATE GIVEN                       
         BE    EDTERR                                                           
         GOTO1 DATVAL,DMCB,(0,22(R2)),(0,56(R2))                                
         OC    0(4,R1),0(R1)                                                    
         BZ    EDTERR                                                           
         CLC   3(1,R1),1(R2)                                                    
         BNE   EDTERR                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,50(R2)),(2,OPTSPER)                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,56(R2)),(2,OPTEPER)                               
*                                                                               
         CLC   OPTSPER(2),OPTEPER   START CANNOT BE GREATER THEN END            
         BH    EDTERR                                                           
         B     STO380                                                           
*                                                                               
STO360   ICM   RE,1,FLD2LEN                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   OPTFILT(0),FLD2                                                  
         B     STO380                                                           
*                                                                               
STO370   MVC   OPTDAYP,FLD2                                                     
         B     STO380                                                           
*                                                                               
STO380   LA    R4,42(R4)                                                        
         BCT   R5,STO320                                                        
         DROP  R4                                                               
         B     EXIT                                                             
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,41,C'NETWORK PROGRAM RECORDS'                                 
         SSPEC H2,41,C'-----------------------'                                 
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PRNET(3),=C'NET'                                                 
         MVC   PRNET+132(4),=4C'-'                                              
         MVC   PRCODE+1(4),=C'CODE'                                             
         MVC   PRCODE+132(6),=6C'-'                                             
         MVC   PRDAT+1(14),=C'START-END DATE'                                   
         MVC   PRDAT+132(17),=17C'-'                                            
         MVC   PRNAM+2(12),=C'PROGRAM NAME'                                     
         MVC   PRNAM+132(16),=16C'-'                                            
         MVC   PRDAY(3),=C'DAY'                                                 
         MVC   PRDAY+132(4),=4C'-'                                              
         MVC   PRTIME+3(4),=C'TIME'                                             
         MVC   PRTIME+132(11),=11C'-'                                           
         MVC   PRSHR(5),=C'SHARE'                                               
         MVC   PRSHR+132(6),=6C'-'                                              
         MVC   PRNTI(3),=C'NTI'                                                 
         MVC   PRNTI+132(4),=4C'-'                                              
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     LFMERR                                                           
*                                                                               
EDTERR   MVI   ERROR,INVALID                                                    
*                                                                               
LFMERR   GOTO1 ERREX                                                            
*                                                                               
BADDEM   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DEMOMSG),DEMOMSG                                       
         GOTO1 ERREX2                                                           
*                                                                               
DEMOMSG  DC    C'INVALID DEMOGRAPHIC CATEGORY'                                  
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
MAXUNIS  EQU   49                                                               
MAXVALS  EQU   36                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* DELETE ALL 'DD'/'DE' ELEMENTS EXCEPT FOR AE OVERRIDES                         
* ROUTINE USES IO2                                                              
*********************************************************************           
DELDDEL  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,NPGDELQ                                                     
         CLI   RECNUM,CPROG                                                     
         BNE   *+8                                                              
         MVI   BYTE,NPGCELQ                                                     
*                                                                               
         L     R2,AIO              PROGRAM RECORD                               
         LA    R2,24(R2)                                                        
         L     R3,AIO2             DUMMY AREA                                   
         MVI   0(R3),0                                                          
*                                                                               
DELDD10  CLI   0(R2),0                                                          
         BE    DELDD40                                                          
         SR    R1,R1                                                            
         ICM   R1,1,1(R2)          ELEMENT LENGTH                               
         BZ    DELDD40                                                          
*                                                                               
         CLC   BYTE,0(R2)                                                       
         BNE   DELDD20             GO COPY NON-'DD' ELEMENT                     
*                                                                               
         USING NPGCELDD,R2                                                      
         CLI   RECNUM,CPROG                                                     
         BNE   DELDD15                                                          
*        TM    NPGCFLG,NPGCAEQ                                                  
*        BO    DELDD20             GO COPY AE OVERRIDES                         
         B     DELDD30                                                          
*                                                                               
         USING NPGELDD,R2                                                       
DELDD15  TM    NPGDFLG,NPGDAEQ                                                  
         BO    DELDD20             GO COPY AE OVERRIDES                         
         B     DELDD30                                                          
         DROP  R2                                                               
*                                                                               
DELDD20  LR    RE,R1               COPY ELEMENT TO DUMMY AREA                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)                                                    
         AR    R3,R1                                                            
         MVI   0(R3),0                                                          
*                                                                               
DELDD30  AR    R2,R1               GO TO NEXT ELEMENT                           
         B     DELDD10                                                          
*                                                                               
DELDD40  L     R2,AIO              PROGRAM RECORD                               
         LA    R2,24(R2)           COPY ELEMENTS BACK                           
         L     R3,AIO2                                                          
DELDD50  CLI   0(R3),0             EOR. DONE                                    
         BE    DELDD60                                                          
         ZIC   R1,1(R3)                                                         
         LR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R3)                                                    
         AR    R3,R1                                                            
         AR    R2,R1                                                            
         B     DELDD50                                                          
*                                                                               
DELDD60  MVI   0(R2),0             EOR                                          
         J     EXIT1                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
PLINED   DSECT                                                                  
PRNET    DS    CL4                                                              
         DS    CL4                                                              
PRCODE   DS    CL6                                                              
         DS    CL4                                                              
PRDAT    DS    CL17                                                             
         DS    CL4                                                              
PRNAM    DS    CL16                                                             
         DS    CL4                                                              
PRDAY    DS    CL8                                                              
         DS    CL4                                                              
PRTIME   DS    CL11                                                             
         DS    CL4                                                              
PRSHR    DS    CL6                                                              
         DS    CL4                                                              
PRNTI    DS    CL4                                                              
         SPACE 2                                                                
LLINED   DSECT                                                                  
LRDAT    DS    CL8                                                              
         DS    CL1                                                              
LRCODE   DS    CL6                                                              
         DS    CL3                                                              
LRNAM    DS    CL16                                                             
         DS    CL3                                                              
LRDAY    DS    CL8                                                              
         DS    CL1                                                              
LRTIME   DS    CL11                                                             
         DS    CL1                                                              
LRSHR    DS    CL6                                                              
         DS    CL2                                                              
LRNTI    DS    CL4                                                              
         EJECT                                                                  
       ++INCLUDE DEDEMEQUS2                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         ORG   CONHEAD-64                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*DSECT TO COVER SAVED STORAGE IN TWA0 FOR NESFM00                               
T31CFFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEADH+3520)-(T31CFFD+3072))                                 
         ORG   CONHEADH+3520-SAVAREAL                                           
SAVAREA  DS    0C                                                               
CALLSP   DS    X                   CALL ROUTINE STACK POINTER                   
CALLSTK  DS    XL9                 STACK (LIST OF OVERLAYS)                     
LASTOV   DS    X                   LAST OVERLAY                                 
SVLIST   DS    XL188               LISTDIR SAVE AREA                            
         ORG CONTAGH                                                            
       ++INCLUDE NESFME6D                                                       
         ORG CONTAGH                                                            
       ++INCLUDE NESFME7D                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENNAD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE NEGETNUND                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
*CTGENFILE                                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
WORK2    DS    CL64                                                             
SVFMTSW  DS    H                                                                
OLDVPHS  DS    CL34                OLD VPHS                                     
SPFIL20  DS    CL20                20 SPACES                                    
ODAYTIM  DS    CL6                 OLD DAY/TIME/UNIQ                            
NDAYTIM  DS    CL6                 NEW DAY/TIME/UNIQ                            
ELEMCODE DS    CL1                 92 OR 93 ELEMNT CODE                         
OPTDAYP  DS    CL1                 OPTION DAYPART                               
OPTFILT  DS    CL3                 OPTION FILTER                                
OPTSPER  DS    CL2                 OPTION START PERIOD                          
OPTEPER  DS    CL2                 OPTION END PERIOD                            
SCANAREA DS    CL28                                                             
SVDMWORK DS    CL96                DEMWORK SAVE AREA                            
MAXVPHF  EQU   48                  MAXIMUM VPH FIELDS                           
MAX93VPH EQU   58                  MAXIMUM VPHS IN RECORD (93 ELEMENT)          
*                                                                               
SCAND    DSECT                                                                  
*              DSECT TO COVER SCANNER LINES                                     
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'210NESFM18   08/08/18'                                      
         END                                                                    
