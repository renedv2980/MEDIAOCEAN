*          DATA SET NESFM14A   AT LEVEL 182 AS OF 05/01/02                      
*PHASE T31C14A,+0                                                               
         TITLE 'NESFM14 - NETFILE MAINT - NTWK UNIVERSE REC'                    
         PRINT NOGEN                                                            
T31C14   CSECT                                                                  
         NMOD1 0,T31C14,R8,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASUBSYSD                                                      
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   GOAGAIN,C'N'                                                     
         SPACE 3                                                                
         OI    CONRECH+6,X'01'     REVALIDATE EVERY TIME                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   VRCHK                                                            
         GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(R9),(RA),(RC),(R6),RR=RELO             
*--ABOVE GOTO TO PROCPF                                                         
         BAS   RE,VK                                                            
         B     RESET                                                            
VRCHK    CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   DKCHK                                                            
         GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(R9),(RA),(RC),(R6),RR=RELO             
*--ABOVE GOTO TO PROCPF                                                         
         BAS   RE,VR                                                            
         B     RESET                                                            
DKCHK    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   DRCHK                                                            
         GOTO1 =A(OVFLRTN),DMCB,(1,DUB),(R9),(RA),(RC),(R6),RR=RELO             
         B     RESET                                                            
DRCHK    CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   LRCHK                                                            
         GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(R9),(RA),(RC),(R6),RR=RELO             
*--ABOVE GOTO TO PROCPF                                                         
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
PRPFK    GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(R9),(RA),(RC),(R6),RR=RELO             
*--ABOVE GOTO TO PROCPF                                                         
         CLI   MODE,PROCPFK                                                     
         BNE   EXIT                                                             
         BAS   RE,PF                                                            
*                                                                               
RESET    MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
*                                                                               
EXIT     GOTO1 VSETSPT                                                          
EXIT1    XIT1                                                                   
         EJECT                                                                  
VK       NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NUNRECD,R6                                                       
         MVC   NUNKAGY,AGENCY                                                   
         MVC   NUNKTYP,=2X'0D22'                                                
         LA    R2,UNICODH                                                       
         GOTO1 VALIFLD,DMCB                                                     
         BZ    NOINPUT                                                          
         LTR   R0,R0               IS INPUT NUMERIC                             
         BNZ   VKCODE              YES/CODE                                     
*                                                                               
         CLC   NFLD(4),=C'DDS='    IS IT DDS DISPLAY                            
         BNE   VKDATE                                                           
         CLI   ACTNUM,ACTDIS       DDS UNIV ONLY ALLOWS DISPLAY                 
         BE    *+12                                                             
         MVI   ERROR,INVACT                                                     
         B     LFMERR                                                           
         XC    NUNKAGY,NUNKAGY     AGY=00 FOR DDS                               
         OI    WHENOK,X'01'        DDS UNIV DOES OWN IO                         
         MVI   QDPT,X'FF'                                                       
         MVC   NFLD(8),NFLD+4                                                   
         SH    R1,=H'4'                                                         
*                                                                               
VKDATE   C     R1,=F'5'                                                         
         BL    VKCODE                                                           
         GOTO1 DATVAL,DMCB,(0,NFLD),WORK                                        
         OC    DMCB(4),DMCB                                                     
         BZ    VKINVAL                                                          
         GOTO1 DATCON,DMCB,WORK,(2,NUNKEND)                                     
         MVI   NUNKTYPE,0                     0=END DATE                        
         CLI   ACTNUM,ACTADD       IS IT ADD                                    
         BE    VKEXIT                                                           
         CLI   QDPT,X'FF'          IS IT DDS UNIVS                              
         BE    VKEXIT                                                           
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                NO/SO CHECK IF END DATE MATCHES              
         CLC   KEY(13),KEYSAVE        A RECORD                                  
         BNE   ENDATERR                                                         
         B     VKEXIT                                                           
VKCODE   C     R1,=F'4'                                                         
         BH    VKINVAL                                                          
         MVC   WORK(4),NFLD                                                     
         XC    NFLD(5),NFLD                                                     
         LA    R0,4                                                             
         SR    R0,R1                                                            
         LA    R3,NFLD                                                          
         AR    R3,R0                                                            
         MVC   0(4,R3),WORK                                                     
         PACK  WORK(3),NFLD(5)                                                  
         MVC   NUNKCODE,WORK                                                    
         MVI   NUNKTYPE,1                     1=UNIV CODE                       
VKEXIT   DS    0H                                                               
         MVC   SVKEY,KEY           STORE KEY                                    
         GOTO1 VSETSPT                                                          
         B     EXIT                                                             
*                                                                               
NOINPUT  MVI   ERROR,MISSING                                                    
         B     LFMERR                                                           
VKINVAL  MVI   ERROR,INVALID                                                    
         B     LFMERR                                                           
ENDATERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'*** END DATE ERROR - NEXT DATE'                   
         GOTO1 DATCON,DMCB,(2,NUNKEND),(5,CONHEAD+31)                           
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
SPECPCT  DS    0H                                                               
         LA    R2,UNICODH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=C'** ENTER AMENDED DATA **'                         
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
         DROP  R6                                                               
         EJECT                                                                  
LR       NTR1                                                                   
*                                                                               
LR02     MVI   NLISTS,X'0F'        SET NUM OF LIST LINES                        
         LA    R3,KEY                                                           
         USING NUNRECD,R3                                                       
         SPACE                                                                  
         OC    KEY(20),KEY                                                      
         BNZ   *+10                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         B     LR22                                                             
         SPACE                                                                  
LR20     GOTO1 VSETSPT                                                          
         GOTO1 SEQ                                                              
         SPACE                                                                  
LR22     DS    0H                                                               
         CLC   KEY(10),SVKEY               ID/AM                                
         BNE   LRX                                                              
LR22B    DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         L     R6,AIO                                                           
         USING NUNRECD,R6                                                       
         CLI   NUNKTYPE,0          IS IT ENDDATE                                
         BE    LRDAT                                                            
         UNPK  WORK(5),NUNKCODE(3)                                              
         MVC   LRCODE,WORK                                                      
         B     LR30                                                             
LRDAT    GOTO1 DATCON,DMCB,(2,NUNKEND),(5,LRDATE)                               
*                                                                               
*                                                                               
LR30     XC    LRDESC,LRDESC                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR100                                                            
*                                                                               
         LA    RE,39                                                            
         CLI   1(R6),42                                                         
         BH    LR50                                                             
*                                                                               
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'3'                                                         
         BM    LR100                                                            
LR50     EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LRDESC(0),2(R6)                                                  
*                                                                               
LR100    GOTO1 LISTMON                                                          
         B     LR20                GOTO READ SEQ                                
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
*        DROP  R3,R5,R6                                                         
         EJECT                                                                  
PR       DS    0H                                                               
*                                                                               
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
*                                                                               
         MVI   NFILE,C'T'          STATION FILE                                 
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R4,P+10                                                          
         USING PLINED,R4                                                        
         L     R6,AIO                                                           
         USING NUNRECD,R6                                                       
*                                                                               
         CLI   NUNKTYPE,0          IS IT ENDDATE                                
         BE    PRDAT                                                            
         UNPK  WORK(5),NUNKCODE(3)                                              
         MVC   PRCODE,WORK                                                      
         B     PR30                                                             
PRDAT    GOTO1 DATCON,DMCB,(2,NUNKEND),(5,PRDATE)                               
*                                                                               
*                                                                               
PR30     XC    PRDESC,PRDESC                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR100                                                            
*                                                                               
         LA    RE,39                                                            
         CLI   1(R6),42                                                         
         BH    PR50                                                             
*                                                                               
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'3'                                                         
         BM    DR160                                                            
PR50     EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRDESC(0),2(R6)                                                  
*                                                                               
PR100    GOTO1 SPOOL,DMCB,(R7)                                                  
         B     LR20                                                             
*                                                                               
PREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6,R7                                                         
         EJECT                                                                  
DR       NTR1                                                                   
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   *+10                                                             
         MVC   SVLIST,LISTDIR                                                   
*                                                                               
         CLI   QDPT,X'FF'          DDS DISPLAY                                  
         BNE   DR100                                                            
         XC    UNIDESC,UNIDESC                                                  
         FOUT  UNIDESCH                                                         
*        FOUT  UNITYPEH,=C'N  ',3                                               
         XC    UNIUUNI,UNIUUNI                                                  
         FOUT  UNIUUNIH                                                         
         B     DR200                                                            
*                                                                               
DR100    DS    0H                                                               
*        MVC   KEY,SVKEY                                                        
*        GOTO1 GETREC                                                           
*                                                                               
         XC    UNIDESC,UNIDESC                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR160                                                            
*                                                                               
         ZIC   R5,1(R6)                                                         
         SH    R5,=H'3'                                                         
         BM    DR160                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   UNIDESC(0),2(R6)                                                 
*                                                                               
DR160    FOUT  UNIDESCH                                                         
*                                                                               
         XC    UNINCDE,UNINCDE                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR180                                                            
*                                                                               
         MVC   UNINCDE(6),2(R6)                                                 
*                                                                               
DR180    FOUT  UNINCDEH                                                         
*                                                                               
         B     DR280                                                            
         EJECT                                                                  
*                                                                               
DR200    DS    0H                                                               
         LA    R5,ELEM                                                          
         XC    ELEM(50),ELEM                                                    
         USING GUVD,R5                                                          
*        NOTE GUVAGY LEFT AS 0000 FOR DDS                                       
*                                                                               
         CLI   SVKEY+10,0          DATE                                         
         BNE   DR220                                                            
         MVC   GUVDATE,SVKEY+11    COMPRESSED DATE                              
         B     DR240                                                            
*                                                                               
DR220    MVC   GUVCODE,SVKEY+11    CODE PWOS                                    
DR240    L     R1,AIO2                                                          
         LA    R1,28(R1)                                                        
         ST    R1,GUVAOUT                                                       
         MVI   GUVTYPE,2                                                        
         MVC   GUVCMFCS,ACOMFACS                                                
         MVC   DMCB+4(4),=X'D9000A32'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         NI    WHENOK,X'FE'        RESET DDS OWN-IO BIT TO ZERO                 
         MVI   QDPT,0                                                           
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),ELEM                                                   
         CLI   GUVERROR,0                                                       
         BNE   DR260                                                            
         L     R6,AIO2                                                          
         LA    R6,24(R6)                                                        
         MVC   0(2,R6),=X'02B4'                                                 
         B     DR500               SKIP PCT ADJUSTMENT                          
*                                                                               
DR260    LA    R2,UNICODH          CURSOR TO KEY                                
         MVI   ERROR,NOTFOUND                                                   
         B     LFMERR                                                           
*                                                                               
*                                                                               
DR280    L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    DR300                                                            
         DC    H'0'                MUST FIND 02 ELEM                            
DR300    DS    0H                                                               
*  COMMENT OUT TYPE DISPLAY                                                     
         USING NUNEL02,R6                                                       
*        MVC   UNITYPE(3),=C'N  '        DISPLAY X'00' AS 'N'                   
*        CLI   NUNTYPE,0                                                        
*        BE    DR340                                                            
*        MVC   UNITYPE(1),NUNTYPE                                               
*        CLI   NUNTYPE,C'A'              SEE IF ALPHA NUMERIC                   
*        BNL   DR340                                                            
*        ZIC   R0,NUNTYPE                                                       
*        CVD   R0,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  UNITYPE(2),DUB                                                   
* DR340  FOUT  UNITYPEH                                                         
*                                                                               
         LA    R2,UNISPECH         CHECK FOR PERCENT ADJ                        
         CLI   5(R2),0             NO INPUT                                     
         BE    DR500                                                            
*                                                                               
         LA    R7,NUNIVES                                                       
         LA    R5,44               CAN'T USE MAXUNIS                            
*                                  SINCE ONLY 20 DISPLAY                        
*                                  BUILD AND EXPRESSION FOR CASHVAL             
*                                                                               
*                                  TO EDIT                                      
         XC    WORK(20),WORK                                                    
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK+7(0),8(R2)        MOVE ADJUSTMENT                           
         LA    R3,WORK+7                                                        
         AR    R3,R4               POINT TO LAST CHAR                           
         CLI   0(R3),C'%'                                                       
         BE    DR400                                                            
         MVI   1(R3),C'%'          % NOT INPUT - ADD IT                         
         LA    R4,1(R4)                                                         
*                                                                               
DR400    DS    0H                                                               
         AH    R4,=H'8'            SET TOTAL LENGHT OF EXPRESSION               
DR420    L     R0,0(R7)                                                         
         LTR   R0,R0                                                            
         BZ    DR480               MIGHT BE ZERO                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+1(6),DUB                                                    
         MVC   WORK(4),WORK+1                                                   
         MVI   WORK+4,C'.'         ALTER TO N.NN FOR CASH VAL                   
         GOTO1 CASHVAL,DMCB,WORK,(R4)                                           
         CLI   DMCB,0                                                           
         BNE   EDTERR                                                           
*                                                                               
         L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BNH   EDTERR              CAN'T GO NEGATIVE                            
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'50'                                                  
         BL    DR460                                                            
         AP    DUB(6),=P'1'        ROUND TO NEAREST 10,000                      
DR460    ZAP   DUB,DUB(6)                                                       
         MP    DUB,=P'100'                                                      
         CVB   R0,DUB                                                           
         ST    R0,0(R7)                                                         
DR480    LA    R7,4(R7)                                                         
         BCT   R5,DR420                                                         
*                                                                               
DR500    DS    0H                                                               
         LA    R2,UNIHM1H                                                       
         LA    R3,ADDTAB                                                        
         LA    R5,MAXUNIS                                                       
         LA    R4,DISPTAB                                                       
*                                                                               
DR520    MVC   8(6,R2),=CL6'0'                                                  
         LA    R7,NUNELEM                                                       
*                                                                               
         CLI   0(R4),0                                                          
         BNE   DR540                                                            
         BAS   RE,GETADLT                                                       
         LA    R3,2(R3)                                                         
         OR    R0,R0                                                            
         BZ    DR580                                                            
         B     DR560                                                            
DR540    ZIC   R0,0(R4)                                                         
         SLL   R0,2                                                             
         ST    R0,FULL                                                          
         CLC   NUNLEN,FULL+3                                                    
         BNH   DR580               DISPLACEMENT IS > THEN ELEMENT               
         AR    R7,R0               ADD DISPLACEMENT                             
         OC    0(4,R7),0(R7)                                                    
         BZ    DR580                                                            
         L     R0,0(R7)                                                         
DR560    CVD   R0,DUB                                                           
         DP    DUB,=P'10'                                                       
         PRINT GEN                                                              
         EDIT  (P6,DUB),(6,8(R2)),0,ALIGN=LEFT                                  
DR580    FOUT  (R2)                                                             
         PRINT NOGEN                                                            
         BAS   RE,NEXTFLD                                                       
         LA    R4,1(R4)            NEXT DISPLACEMENT                            
         BCT   R5,DR520                                                         
         B     DR600                                                            
*                                                                               
         SPACE 2                                                                
** ACUMULATE MEN AND WOMAN TO GET TOTAL                                         
GETADLT  LA    R7,NUNELEM                                                       
         SR    R0,R0                                                            
*                                                                               
         ZIC   RF,0(R3)                                                         
         SLL   RF,2                                                             
         ST    RF,FULL                                                          
         CLC   NUNLEN,FULL+3                                                    
         BNH   GETAEX              DISPLACEMENT IS > THEN ELEMENT               
         AR    R7,RF                                                            
         L     R0,0(R7)                                                         
*                                                                               
         LA    R7,NUNELEM                                                       
         ZIC   RF,1(R3)                                                         
         SLL   RF,2                                                             
         ST    RF,FULL                                                          
         CLC   NUNLEN,FULL+3                                                    
         BNH   GETAEX              DISPLACEMENT IS > THEN ELEMENT               
         AR    R7,RF                                                            
         L     RF,0(R7)                                                         
*                                                                               
         AR    R0,RF                                                            
GETAEX   BR    RE                                                               
         EJECT                                                                  
*                                                                               
DR600    CLI   QDPT,X'FF'          DDS DISPLAY                                  
         BNE   DR620                                                            
         XC    UNIACTV,UNIACTV                                                  
         FOUT  UNIACTVH,=C'DDS STANDARD UNIVERSES',22                           
         B     DR660                                                            
*                                                                               
DR620    CLI   UNISPECH+5,0        CHK FOR SPECIALS                             
         BE    DR680                                                            
         XC    UNIACTV,UNIACTV                                                  
         FOUT  UNIACTVH,UNISPEC,16     DISPLAYS PCT ADJ                         
DR660    FOUT  UNISPECH,BLANKS,16                                               
******   MVI   SVFMTSW,0           SET TO FORMAT MODE                           
         OI    UNIW1H+1,X'01'      SET TO MODIFIED                              
         B     SPECPCT                                                          
*                                                                               
DR680    FOUT  UNISPECH,BLANKS,16                                               
*                                                                               
         XC    UNIACTV,UNIACTV                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR720                                                            
*                                                                               
         XC    UNIACTV,UNIACTV                                                  
         CLC   UNICOD(4),=C'DDS='     DDS DISPLAY                               
         BE    DR720                                                            
         MVC   UNIACTV(13),=C'LAST ACTIVITY'                                    
         USING NUNEL01,R6                                                       
         GOTO1 DATCON,DMCB,(3,NUNACTD),(5,UNIACTV+14)                           
         MVC   UNIACTV+24(3),=C'ADD'                                            
         CLI   NUNACT,C'A'                                                      
         BE    *+10                                                             
         MVC   UNIACTV+24(6),=C'CHANGE'                                         
*                                                                               
DR720    FOUT  UNIACTVH                                                         
*                                                                               
DR740    DS    0H                                                               
         XC    UNIUUNI,UNIUUNI                                                  
         LA    R5,UNIUUNI                                                       
         LA    R7,L'UNIUUNI(R5)                                                 
         L     R6,AIO                                                           
         USING UDEUND,R6                                                        
         MVI   ELCODE,X'C1'                                                     
         BAS   RE,GETEL            USER DEMO UNIVERSE ELEM                      
         BE    DR780                                                            
         B     DR880                                                            
DR760    BAS   RE,NEXTEL                                                        
         BNE   DR860                                                            
*                                                                               
DR780    GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(R9),(RA),(RC),(R6),RR=RELO             
*--ABOVE GOTO TO "FMTDEMO"                                                      
         ZIC   R1,WORK             WORK HAS LENGHT                              
         LR    R0,R5               SEE IF IT WILL FIT ON THIS LINE              
         AR    R0,R1                                                            
         AH    R0,=H'3'            ADJUST FOR VALUE                             
         CR    R0,R7                                                            
         BNH   DR820                                                            
         DC    H'0'                TOO MANY DEMO                                
DR820    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK+1      WORK+1 HAS DESC                              
         AR    R5,R1                                                            
         MVI   1(R5),C'='                                                       
         LA    R5,2(R5)                                                         
         BAS   RE,EDITB4                                                        
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         B     DR760               NEXT DEMO                                    
*                                                                               
DR860    BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   DR880                                                            
         MVI   0(R5),C' '          BLANK OUT LAST COMMA                         
DR880    FOUT  UNIUUNIH                                                         
         B     EXIT                                                             
         EJECT                                                                  
EDITB4   DS    0H                                                               
         ICM   R0,15,UDEUNIV                                                    
         CVD   R0,DUB                                                           
         DP    DUB,=P'10'                                                       
         EDIT  (P6,DUB),(6,WORK),0,ALIGN=LEFT,WRK=WORK+20                       
         LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,MOVEIT                                                        
         AR    R5,R0                                                            
         CR    R5,R7                                                            
         BNH   0(RE)               NORMAL RETURN                                
         DC    H'0'                CAN'T FIT ON LINE                            
*                                                                               
MOVEIT   MVC   0(0,R5),WORK                                                     
*                                                                               
         EJECT                                                                  
***********************************************************                     
*                                                                               
VR       NTR1                                                                   
         CLI   QDPT,X'FF'          IS IT DDS UNIV                               
         BE    VREX1                                                            
         CLI   UNISPECH+5,0        CHK FOR INPUT IN SPECIAL                     
         BE    VR100                                                            
         CLI   ACTNUM,ACTADD                                                    
         BNE   VREX1                                                            
         LA    R2,UNISPECH                                                      
         B     EDTERR              NO PCT ADJUSTMENT FOR ADDS                   
*                                                                               
VR100    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'66'                                                     
         GOTO1 REMELEM            DELETE OLD DESC                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'C1'                                                     
         GOTO1 REMELEM            DELETE OLD USER DEMO UNIVERSES                
         LA    R2,UNIDESCH                                                      
         CLI   5(R2),0                                                          
         BE    VR120                                                            
         XC    ELEM(100),ELEM                                                   
         MVI   ELEM,X'66'                                                       
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+2(0),8(R2)                                                  
         AH    R5,=H'3'                                                         
         STC   R5,ELEM+1           SET ELEM LENGHT                              
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
*                                                                               
VR120    LA    R2,UNIW1H           CURSOR TO FIRST FIELD                        
         MVI   NOPTFLG,0           REQUIRED                                     
         GOTO1 VALIFLD                                                          
         BZ    MISSERR                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    VR180                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         GOTO1 REMELEM             PXZ                                          
         BE    VR200              PXZ                                           
         B     VR180                                                            
         BAS   RE,GETEL                                                         
         BE    VR160                                                            
         DC    H'0'                MUST FIND 02 ELEM                            
VR160    MVC   ELEM(168),0(R6)      TO PRESERVE OLD UNIVS                       
*                                  NO LONGER DISPLAYED                          
         B     VR200                                                            
*                                                                               
VR180    XC    ELEM(218),ELEM                                                   
         MVC   ELEM(2),=X'02B4'       SET CODE AND LENGHT                       
VR200    LA    R7,ELEM                                                          
         USING NUNELEM,R7                                                       
         LA    R2,UNIHM1H                                                       
         LA    R5,MAXUNIS                                                       
         LA    R4,DISPTAB                                                       
*                                                                               
VR220    CLI   0(R4),0                                                          
         BE    VR260                                                            
         BAS   RE,CHKSPEC          CHECK SPECIAL COLUMNS                        
         CLI   BYTE,X'FF'                                                       
         BE    VR260                                                            
         CLI   5(R2),0                                                          
         BNE   VR240                                                            
         MVI   ERROR,MISSING                                                    
         B     LFMERR                                                           
*                                                                               
VR240    ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   DMCB,0                                                           
         BNE   EDTERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BL    EDTERR              CAN'T BE NEGATIVE                            
*!!!     BNH   EDTERR              CAN'T BE NEGATIVE                            
         CVD   R0,DUB                                                           
         DP    DUB,=P'1000'                                                     
         CP    DUB+5(3),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   EDTERR              NO DECIMAL OR ONES                           
         CVD   R0,DUB              I.E. NEAREST 10,000                          
         DP    DUB,=P'10'                                                       
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
         LA    R6,NUNELEM                                                       
         ZIC   R1,0(R4)                                                         
         SLL   R1,2                                                             
         AR    R6,R1               ADD DISPLACEMENT                             
         ST    R0,0(R6)                                                         
*                                                                               
VR260    BAS   RE,NEXTFLD          SET R2 TO NEXT SCREEN FLD                    
         LA    R4,1(R4)            NEXT UNIV                                    
         BCT   R5,VR220                                                         
*--COMMENT OUT TYPE VALIDATION                                                  
*        LA    R2,UNITYPEH                                                      
*        XC    NUNTYPE,NUNTYPE                                                  
*        CLI   5(R2),0                                                          
*        BE    VR360                                                            
*        CLI   5(R2),2                                                          
*        BH    EDTERR                                                           
*        MVC   NUNTYPE,UNITYPE                                                  
*        CLI   UNITYPE,C'N'                                                     
*        BE    VR360                                                            
*        CLI   UNITYPE,C'C'                                                     
*        BE    VR360                                                            
*        CLI   UNITYPE,C'0'                                                     
*        BL    EDTERR                                                           
*        CLI   5(R2),1          SEE IF ONE DIGIT                                
*        BE    VR320                                                            
*        CLI   UNITYPE+1,C'0'                                                   
*        BL    EDTERR                                                           
* VR320  DS    0H                                                               
*        SR    R1,R1                                                            
*        SR    R0,R0                                                            
*        ZAP   DUB,=P'0'                                                        
*        IC    R1,5(R2)                                                         
*        LTR   R1,R1               ERROR IF ZERO LENGTH                         
*        BZ    EDTERR                                                           
*        TM    4(R2),X'08'         OR NON-NUMERIC                               
*        BZ    EDTERR                                                           
*        BCTR  R1,0                                                             
*        EX    R1,*+12                                                          
*        CVB   R0,DUB                                                           
*        B     VR340                                                            
*        PACK  DUB,8(0,R2)                                                      
*  VR340 STC   R0,NUNTYPE                                                       
*                                                                               
VR360    DS    0H                                                               
*                                                                               
****     MVI   ELCODE,X'02'        PXZ ALREADY DELETED                          
*****    L     R6,AIO                                                           
*****    GOTO1 REMELEM          DELETE OLD 02 ELEM                              
         L     R6,AIO                                                           
         GOTO1 ADDELEM          ADD NEW ONE                                     
*                                                                               
         LA    R2,UNIUUNIH         CHK FOR USER DEMO UNIVS                      
         CLI   5(R2),0                                                          
         BE    VREX                                                             
         L     R3,AIO2                                                          
         LA    R4,8                                                             
VR460    XC    0(250,R3),0(R3)                                                  
         LA    R3,250(R3)                                                       
         BCT   R4,VR460                                                         
         L     R3,AIO2                                                          
         GOTO1 SCANNER,DMCB,(R2),(10,0(R3))                                     
         CLI   DMCB+4,0                                                         
         BE    EDTERR                                                           
         L     R5,AIO2                                                          
         USING SCAND,R5                                                         
VR480    CLC   0(2,R5),=X'0000'    END OF TABLE                                 
         BE    VREX                                                             
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING UDEUND,R7                                                        
         MVC   ELEM(2),=X'C10F'                                                 
         CLI   FLD1LEN,0                                                        
         BNH   EDTERR                                                           
         CLI   FLD1LEN,7                                                        
         BH    EDTERR                                                           
         MVC   ELEM+2(7),FLD1                                                   
         TM    FLD2VAL,X'80'       CHK NUMERIC                                  
         BZ    EDTERR                                                           
         CLI   FLD2LEN,6           MAX IS 6 CHARS                               
         BH    EDTERR                                                           
         L     R0,FLD2B                                                         
         C     R0,=F'0'                                                         
         BNH   EDTERR              CAN'T BE NEGATIVE                            
         CVD   R0,DUB                                                           
         DP    DUB,=P'10'                                                       
         CP    DUB+6(2),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   EDTERR              NO DECIMAL OR ONES                           
         CVD   R0,DUB              I.E. NEAREST 10,000                          
         MP    DUB,=P'10'                                                       
         CVB   R0,DUB                                                           
         STCM  R0,15,ELEM+9                                                     
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         LA    R5,32(R5)           NEXT SCANNER ENTRY                           
         B     VR480                                                            
*                                                                               
VREX     DS    0H                                                               
         BAS   RE,ACTIVITY                                                      
VREX1    BAS   RE,DR                                                            
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
*  CHECK IF COLUMN IS SPECILA AND CALCULATE                                     
*  R4= DISPTAB ENTRY                                                            
*                                                                               
CHKSPEC  NTR1                                                                   
         LA    R6,ELEM                                                          
         USING NUNELEM,R6                                                       
         MVI   BYTE,0                                                           
*                                                                               
         CLI   0(R4),18            TOTAL TEENS                                  
         BE    CHKSP020                                                         
         CLI   0(R4),20            TOTAL 6-11                                   
         BE    CHKSP040                                                         
         CLI   0(R4),21            TOTAL 2-11                                   
         BE    CHKSP060                                                         
         B     CHKSPEX                                                          
*                                                                               
CHKSP020 L     RE,64(R6)           TOTAL BOYS                                   
         L     RF,68(R6)           TOTAL GIRLS                                  
         AR    RE,RF                                                            
         ST    RE,72(R6)           TOTAL TEENS                                  
         B     CHKSP999                                                         
*                                                                               
CHKSP040 L     RE,136(R6)          BOYS 6-11                                    
         L     RF,112(R6)          GIRLS 6-11                                   
         AR    RE,RF                                                            
         ST    RE,80(R6)           TOTAL 6-11                                   
         B     CHKSP999                                                         
*                                                                               
CHKSP060 L     RE,116(R6)          BOYS 2-11                                    
         L     RF,140(R6)          GIRLS 2-11                                   
         AR    RE,RF                                                            
         ST    RE,84(R6)           TOTAL 2-11                                   
         B     CHKSP999                                                         
*                                                                               
CHKSP999 MVI   BYTE,X'FF'                                                       
CHKSPEX  B     EXIT                                                             
         DROP  R6                                                               
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
         GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(R9),(RA),(RC),(R6),RR=RELO             
*--ABOVE GOTO TO PROCPF                                                         
         B     EXIT                                                             
         EJECT                                                                  
ACTIVITY NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM            DELETE OLD DESC                               
         XC    ELEM(10),ELEM                                                    
         MVC   ELEM(2),=X'0108'                                                 
         GOTO1 DATCON,DMCB,(5,0),(3,ELEM+2)                                     
         MVC   ELEM+5(1),CONACT       SAVE ACTION                               
         CLI   CONACT,C'S'         CHECK SELECT                                 
         BNE   *+8                                                              
         MVI   ELEM+5,C'C'                                                      
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         XIT1                                                                   
         EJECT                                                                  
NEXTUN   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUN                                                           
         BR    RE                                                               
         SPACE 2                                                                
NEXTFLD  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'28'         TEST PROTECTED AND HIGHLIGHTED               
         BO    NEXTFLD                                                          
         BR    RE                                                               
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,41,C'NETWORK UNIVERSE RECORDS'                                
         SSPEC H2,41,C'------------------------'                                
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
*                                                                               
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PRCODE(9),=C'CODE/DATE'                                          
         MVC   PRCODE+132(13),=13C'-'                                           
         MVC   PRDESC(7),=C'COMMENT'                                            
         MVC   PRDESC+132(40),=40C'-'                                           
         B     EXIT                                                             
         DROP  R2,R6                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     LFMERR                                                           
*                                                                               
EDTERR   MVI   ERROR,INVALID                                                    
*                                                                               
LFMERR   GOTO1 ERREX                                                            
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
MAXUNIS  EQU   53                                                               
MAXVALS  EQU   36                                                               
         EJECT                                                                  
         LTORG                                                                  
RELO     DS    A                                                                
BLANKS   DC    132X'40'                                                         
         EJECT                                                                  
*-- FIELDS TO ADD TO GET ADULT NUMBERS                                          
ADDTAB   DC    AL1(07),AL1(13)     AD18+                                        
         DC    AL1(02),AL1(08)     AD18-34                                      
         DC    AL1(03),AL1(09)     AD18-49                                      
         DC    AL1(25),AL1(31)     AD21+                                        
         DC    AL1(26),AL1(32)     AD21-49                                      
         DC    AL1(27),AL1(33)     AD21-54                                      
         DC    AL1(04),AL1(10)     AD25-54                                      
         DC    AL1(23),AL1(24)     AD35-64                                      
         DC    AL1(06),AL1(12)     AD55+                                        
*        DC    AL1(16),AL1(17)     ADTEENS                                      
         DC    AL1(30),AL1(36)     AD15-24                                      
         DC    AL1(05),AL1(11)     AD45+                                        
*        DC    AL1(28),AL1(34)     AD6-11                                       
*        DC    AL1(29),AL1(35)     AD2-11                                       
         DC    X'0000'             END OF TABLE                                 
         EJECT                                                                  
*                                                                               
*              DISPLACEMENT OF UNIV IN 02 ELEM                                  
DISPTAB  DS    0C                                                               
HOMES    DC    AL1(22)             HOMES   22 X 4 - 4                           
*                                                                               
WOMEN    DC    AL1(07)             WN18+   07 X 4 - 4                           
         DC    AL1(02)             WN18-34 02 X 4 - 4                           
         DC    AL1(03)             WN18-49 03 X 4 - 4                           
         DC    AL1(25)             WN21+   25 X 4 - 4                           
         DC    AL1(26)             WN21-49 26 X 4 - 4                           
         DC    AL1(27)             WN25-49 27 X 4 - 4                           
         DC    AL1(04)             WN25-54 04 X 4 - 4                           
         DC    AL1(23)             WN35-64 23 X 4 - 4                           
         DC    AL1(06)             WN55+   06 X 4 - 4                           
*                                                                               
MEN      DC    AL1(13)             MN18+   13 X 4 - 4                           
         DC    AL1(08)             MN18-34 08 X 4 - 4                           
         DC    AL1(09)             MN18-49 09 X 4 - 4                           
         DC    AL1(31)             MN21+   31 X 4 - 4                           
         DC    AL1(32)             MN21-49 32 X 4 - 4                           
         DC    AL1(33)             MN25-49 33 X 4 - 4                           
         DC    AL1(10)             MN25-54 10 X 4 - 4                           
         DC    AL1(24)             MN35-64 24 X 4 - 4                           
         DC    AL1(12)             MN55+   12 X 4 - 4                           
*                                                                               
ADULTS   DC    AL1(00)             AD18+                                        
         DC    AL1(00)             AD18-34                                      
         DC    AL1(00)             AD18-49                                      
         DC    AL1(00)             AD21+                                        
         DC    AL1(00)             AD21-49                                      
         DC    AL1(00)             AD25-49                                      
         DC    AL1(00)             AD25-54                                      
         DC    AL1(00)             AD35-64                                      
         DC    AL1(00)             AD55+                                        
*                                                                               
SUBS     DC    AL1(17)             WNTEEN  17 X 4 - 4                           
         DC    AL1(30)             WN15-24 30 X 4 - 4                           
         DC    AL1(05)             WN45*   05 X 4 - 4                           
*                                                                               
         DC    AL1(28)             GL6-11  28 X 4 - 4                           
         DC    AL1(29)             GL2-11  29 X 4 - 4                           
*                                                                               
         DC    AL1(16)             MNTEEN  16 X 4 - 4                           
         DC    AL1(36)             MN15-24 36 X 4 - 4                           
         DC    AL1(11)             MN45+   11 X 4 - 4                           
*                                                                               
         DC    AL1(34)             BY6-11  34 X 4 - 4                           
         DC    AL1(35)             BY2-11  35 X 4 - 4                           
*                                                                               
         DC    AL1(01)             AD2+    01 X 4 - 4                           
         DC    AL1(18)             ADTEENS                                      
         DC    AL1(00)             AD15-24                                      
         DC    AL1(00)             AD45+                                        
         DC    AL1(20)             AD6-11                                       
         DC    AL1(40)             AD9-11  40 X 4 - 4                           
         DC    AL1(37)             AD9-14  47 X 4 - 4                           
         DC    AL1(21)             AD2-11                                       
*                                                                               
         DC    AL1(15)             WW18+   15 X 4 - 4                           
         DC    AL1(38)             WW18-49 38 X 4 - 4                           
         DC    AL1(39)             WW25-54 39 X 4 - 4                           
         DC    AL1(41)             MOMS    41 X 4 - 4                           
*                                                                               
         DC    AL1(42)             HHWC<18 42 X 4 - 4                           
         DC    AL1(43)             HHWC<12 43 X 4 - 4                           
         DC    AL1(44)             HHWC<06 44 X 4 - 4                           
         DC    X'0000'             END OF TABLE                                 
         SPACE 2                                                                
*--SECONDARY NMOD                                                               
         EJECT                                                                  
*--SET OPTION AND PF KEY ROUTINES                                               
*                                                                               
         DS    0F                                                               
         DROP  R8,RB                                                            
OVFLRTN  NMOD1 0,**UNIVOV                                                       
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING OVFLRTN,RB,R6                                                    
         L     R9,4(R1)                                                         
         L     RA,8(R1)                                                         
         L     RC,12(R1)                                                        
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   0(R1),0                                                          
         BE    OVBR0                                                            
         CLI   0(R1),1                                                          
         BE    OVBR1                                                            
         CLI   0(R1),2                                                          
         BE    OVBR2                                                            
*                                                                               
OVBR0    BAS   RE,FMTDEMO                                                       
         B     OPXIT                                                            
OVBR1    BAS   RE,DK                                                            
         B     OPXIT                                                            
OVBR2    BAS   RE,PROCPF                                                        
         B     OPXIT                                                            
         EJECT                                                                  
FMTDEMO  NTR1                                                                   
         L     R6,16(R1)           ADDRESS OF ELEMENT                           
         USING UDEUND,R6                                                        
         MVC   WORK(10),SPACES                                                  
         MVI   WORK,0                                                           
         MVC   WORK+1(7),UDEUNNAM                                               
         LA    R1,7                                                             
         LA    R4,WORK+7                                                        
FMTDM5   CLI   0(R4),C' '          SCAN BACKWARD FOR NON-SPACE                  
         BNE   FMTDM10                                                          
         BCTR  R4,0                                                             
         BCT   R1,FMTDM5                                                        
FMTDM10  STC   R1,WORK                                                          
         B     OPEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
DK       NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         L     R3,AIO                                                           
         USING NUNRECD,R3                                                       
         LA    R2,UNICODH                                                       
         XC    UNICOD,UNICOD                                                    
         CLC   NUNKAGY,=2X'00'     IS IT DDS                                    
         BNE   *+14                                                             
         MVC   8(4,R2),=C'DDS='                                                 
         LA    R2,4(R2)                                                         
         CLI   NUNKTYPE,0          IS IT ENDDATE                                
         BE    ENDAT                                                            
         UNPK  WORK(5),NUNKCODE(3)                                              
         MVC   8(4,R2),WORK                                                     
         B     DKXIT                                                            
ENDAT    GOTO1 DATCON,DMCB,(2,NUNKEND),(5,8(R2))                                
*                                                                               
DKXIT    GOTO1 VSETSPT                                                          
         FOUT  UNICODH                                                          
         B     OPEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                      PROCESS PF KEYS                                *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         LA    R2,UNICODH                                                       
         SPACE 1                                                                
PROCPFA  CLI   PFAID,PF5           PF3 FOR ESTIMATE ACTION CHANGE               
         BE    PROCPF4                                                          
         CLI   PFAID,PF4           PF2 FOR ESTIMATE ACTION DISPLAY              
         BNE   PROCPFX                                                          
         SPACE 1                                                                
         LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         SPACE 1                                                                
PROCPF2  MVI   PFAID,0                                                          
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   PROCPF3                                                          
         GOTO1 VCALL,WORK,=C'DUNIV',,(12,UNICOD),0                              
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF3  GOTO1 VTRANSF,WORK,=C'DUNIV',,(12,UNICOD),0                            
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF4  LA    R1,=C'CHA'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         B     PROCPF2                                                          
         SPACE 1                                                                
PROCPFX  B     OPEXIT                                                           
*                                                                               
OPEXIT   XIT1                                                                   
         EJECT                                                                  
*                                                                               
OPXIT    XMOD1                                                                  
         SPACE 2                                                                
         LTORG                                                                  
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
         EJECT                                                                  
*                                                                               
LLINED   DSECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         DS    CL5                                                              
LRCODE   DS    CL4                                                              
         DS    CL1                                                              
LRDATE   DS    CL8                                                              
         DS    CL2                                                              
LRDESC   DS    CL40                                                             
*                                                                               
PLINED   DSECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         DS    CL5                                                              
PRCODE   DS    CL4                                                              
         DS    CL1                                                              
PRDATE   DS    CL8                                                              
         DS    CL2                                                              
PRDESC   DS    CL40                                                             
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
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
       ++INCLUDE NESFMF9D                                                       
         ORG CONTAGH                                                            
       ++INCLUDE NESFMFAD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENUNIVA                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE NEGETNUND                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
*CTGENFILE                                                                      
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'182NESFM14A  05/01/02'                                      
         END                                                                    
