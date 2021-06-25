*          DATA SET REDAR40    AT LEVEL 039 AS OF 08/07/97                      
*PHASE T80F40A                                                                  
*INCLUDE REGENPBY                                                               
         TITLE 'T80F40 - REDAR40 - DARE AGENCY REVISION LIST'                   
***********************************************************************         
*                                                                     *         
*  REDAR40 (T80F40) --- DARE AGENCY REVISION LIST                     *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 22JUL97 SKU INITIAL RELEASE                                         *         
*                                                                     *         
***********************************************************************         
T80F40   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80F40*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
                                                                                
         MVI   MYSCRNUM,X'F5'                                                   
         MVC   REVLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
                                                                                
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         CLI   CALLSP,0            MUST BE CALLED TO GET HERE                   
         BNE   VKX                                                              
         LA    R2,CONRECH                                                       
         B     INVLRCAC            INVALID REC/ACTION                           
                                                                                
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         TWAXC REVLISTH,REVENDLH,PROT=Y                                         
         MVI   BLISTNUM,BLISTMAX                                                
                                                                                
         TM    DISPFLAG,NEXTPG     FIRST PASS?                                  
         BO    VR30                                                             
                                                                                
         XC    BUYKEY,BUYKEY                                                    
                                                                                
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                  YES, READ IN THE AGENCY ORDER RECORD         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         OC    RDARREP#,RDARREP#                                                
         BZ    VR10                                                             
                                                                                
         ZAP   WORK(5),=P'0'       CONTRACT NUMBER                              
         MVO   WORK(5),RDARREP#                                                 
         EDIT  (P5,WORK),(8,REVHDLN),ALIGN=LEFT                                 
         STC   R0,REVHDLNH+5       SET LENGTH OF DESTINATION                    
         MVI   REVHDLNH+4,X'08'    SET VALID NUMERIC                            
                                                                                
VR10     DS    0H                  AGENCY ORDER NUMBER                          
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RDARKORD                                                 
         EDIT  (P5,WORK),(8,REVAORD),ALIGN=LEFT                                 
                                                                                
VR15     DS    0H                                                               
         MVC   KEY(27),RDARKEY                                                  
         MVI   KEY+RDARKRT-RDARKEY,X'50' TRAILER RECORD                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                TRAILER MUST BE THERE                        
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,SORTBUYS         BUILD AGENCY/CONTRACT BUY TABLE              
*                                                                               
         XC    DISPFLAG,DISPFLAG                                                
         OI    DISPFLAG,FIRSTPG                                                 
         LA    R2,REVHDLNH                                                      
         CLI   5(R2),0                                                          
         BE    VR25                                                             
         GOTO1 VALICON,DMCB,(R2)                                                
                                                                                
VR25     DS    0H                  USER SPECIFIED BUY NUMBER TO DISPLAY         
         CLI   REVABUYH+5,0                                                     
         BNE   VR30                                                             
         MVI   BUYNUM,0                                                         
         OI    REVABUYH+4,X'20'                                                 
         B     VR40                                                             
                                                                                
VR30     DS    0H                                                               
         LA    R2,REVABUYH                                                      
         TM    4(R2),X'20'         CHECK IF START FROM A DIFFERENT BUY          
         BO    VR40                                                             
         OI    REVABUYH+4,X'20'                                                 
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    INVLFLD                                                          
         CH    R0,=H'254'                                                       
         BH    INVLFLD                                                          
         STC   R0,BUYNUM                                                        
         LA    R2,REVLISTH         POINT TO START OF LIST                       
         B     DISB50                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY STANDARD COMMENTS, ORDER COMMENTS AND AGENCY BUYS                     
***********************************************************************         
VR40     DS    0H                                                               
         LA    R2,REVLISTH         POINT TO START OF LIST                       
                                                                                
         CLI   BUYNUM,0            DON'T START FROM TOP?                        
         BNE   DISB50                                                           
                                                                                
         TM    DISPFLAG,FIRSTPG                                                 
         BO    VR45                                                             
                                                                                
DISB50   DS    0H                                                               
         OC    BUYKEY,BUYKEY                                                    
         BNZ   DISB70                                                           
                                                                                
VR45     DS    0H                                                               
         MVC   BUYKEY,AGYVKEY                                                   
                                                                                
         LA    R6,BUYKEY                                                        
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'40'       TYPE BUY                                     
         DROP  R6                                                               
                                                                                
DISB70   DS    0H                                                               
         LA    R6,BUYKEY                                                        
         USING RDARKEY,R6                                                       
         MVC   RDARKSEQ,BUYNUM                                                  
         DROP  R6                                                               
                                                                                
         MVC   KEY(L'BUYKEY),BUYKEY                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BE    DISB75                                                           
         MVI   BUYNUM,0            NOT FOUND                                    
         B     DISB90                                                           
                                                                                
DISB75   DS    0H                                                               
         MVC   BUYKEY,KEY                                                       
*************                                                                   
* CHECK THIS !!!!!!!!!!!!!!!!!!!!!!!!                                           
*************                                                                   
         CLI   BLISTNUM,BLISTMAX                                                
         BNE   DISB78                                                           
         ZIC   RF,BLISTNUM         MINUS ONE FOR TITLE                          
         BCTR  RF,0                                                             
         STC   RF,BLISTNUM                                                      
                                                                                
DISB78   DS    0H                                                               
         MVC   PARAS(4),DATAMGR    SET UP ROUTINE ADDRESS BLOCK                 
         MVC   PARAS+4(4),VOUTDAY                                               
         MVC   PARAS+8(4),UNTIME                                                
         MVC   PARAS+12(4),DATCON                                               
         MVC   PARAS+16(4),ADDAY                                                
         MVC   PARAS+20(4),GETDAY                                               
*                                                                               
         L     R3,AIO2             USE SECOND HALF OF IO2 FOR AGENCY            
         LA    R3,2000(R3)          BUY DISPLAY                                 
*                                                                               
         GOTO1 VREGENDB,DMCB,(X'80',BUYKEY),(BLISTNUM,(R3)),           X        
               (BUYNUM,AIO3),PARAS                                              
         ZIC   R3,DMCB+8                                                        
         MVC   BUYNUM,DMCB+9                                                    
         LTR   R3,R3                                                            
         BZ    DISB90                                                           
                                                                                
         MVC   8(BUYTITLX,R2),BUYTITLE                                          
         OI    1(R2),X'08'         SET TO HIGH INTENSITY                        
         BAS   RE,BUMPNEXT                                                      
                                                                                
         L     R4,AIO2                                                          
         LA    R4,2000(R4)                                                      
                                                                                
DISB80   DS    0H                                                               
         MVC   8(78,R2),1(R4)                                                   
         NI    1(R2),X'FF'-X'08'   SET TO NORMAL                                
         BAS   RE,BUMPNEXT                                                      
                                                                                
         LA    RF,REVENDLH         CHECK IF WE'RE EXCEEDING SCREEN SIZE         
         CR    R2,RF                                                            
         BH    DISB90              SKIP IF BUY LIST DOESN'T FIT!                
                                                                                
DISB85   DS    0H                                                               
         LA    R4,81(R4)                                                        
         BCT   R3,DISB80                                                        
*                                                                               
         GOTO1 LISTKBUY,DMCB,(R2)                                               
*                                                                               
DISB90   DS    0H                                                               
         XC    REVABUY,REVABUY                                                  
         CLI   BUYNUM,0                                                         
         BNE   DISB100                                                          
         XC    DISPFLAG,DISPFLAG   NO MORE BUYS, SET TO DISPLAY                 
*                                    FROM BEGINNING ON NEXT PASS                
         OI    DISPFLAG,FIRSTPG                                                 
         B     HITBOTTM                                                         
                                                                                
DISB100  DS    0H                                                               
         EDIT  BUYNUM,(3,REVABUY),ALIGN=LEFT                                    
         OI    DISPFLAG,NEXTPG                                                  
                                                                                
DISBX    DS    0H                                                               
         B     NEXTPAGE                                                         
         EJECT                                                                  
***********************************************************************         
* SORT CONTRACT BUYS BY AGENCY BUY NUMBER AND CONTRACT BUY NUMBER               
* USES IO2 AS SORT AREA AND IO3 TO READ CONTRACT BUY RECORDS                    
***********************************************************************         
SORTBUYS NTR1                                                                   
         XCEFL SORTAREA,1530                                                    
                                                                                
         SR    R3,R3                                                            
         LA    R4,SORTAREA                                                      
                                                                                
         XC    KEY,KEY             CONSTRUCT CONTRACT BUY KEY                   
         LA    R6,KEY                                                           
         USING RBUYKEY,R6                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,AGENCY                                                  
         PACK  WORK(1),CCONNUM+3(1) REVERSE THE COMPLIMENT                      
         PACK  WORK+1(1),CCONNUM+2(1)                                           
         PACK  WORK+2(1),CCONNUM+1(1)                                           
         PACK  WORK+3(1),CCONNUM(1)                                             
         MVC   RBUYKCON,WORK                                                    
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(22),KEYSAVE                                                  
         BNE   SORTBX                                                           
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
                                                                                
SORTB10  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   0(1,R4),RBUYAGBL                                                 
         MVC   1(1,R4),RBUYKLIN                                                 
         MVC   2(4,R4),KEY+28                                                   
         LA    R4,6(R4)                                                         
         LA    R3,1(R3)            NUMBER OF SORT RECORDS                       
                                                                                
         OC    RBUYAGBL,RBUYAGBL   IF NO REFERENCE TO AGENCY BUY#               
         BNZ   *+8                 PRINT ALL THESE CONTRACT BUYS AT THE         
         MVI   0(R4),X'FF'         END (SHOULDN'T HAPPEN, BUT J.I.C.)           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(22),KEYSAVE                                                  
         BE    SORTB10                                                          
         DROP  R6                                                               
                                                                                
         GOTO1 XSORT,DMCB,SORTAREA,(R3),6,2,0                                   
                                                                                
SORTBX   DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                               
***********************************************************************         
LISTKBUY NTR1                                                                   
         L     R2,0(R1)                                                         
         LA    R4,SORTAREA                                                      
*                                                                               
LB10     DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BE    LBX                                                              
         LA    RF,BUYKEY                                                        
         USING RDARKEY,RF                                                       
         CLC   RDARKSEQ,0(R4)                                                   
         BH    LBX                 MATCHING K BUY TO AGENCY BUY?                
         BE    LB15                                                             
         LA    R4,6(R4)                                                         
         B     LB10                                                             
         DROP  RF                                                               
*                                                                               
LB15     DS    0H                                                               
         MVC   KEY+28(4),2(R4)     YES                                          
         MVC   AIO,AIO3            PUT BUY RECORD IN IO3                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         LA    R6,KEY              PUT CONTRACT RECORD IN IO1                   
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R3,AIO2             USE SECOND HALF OF IO2 FOR AGENCY            
         LA    R3,2000(R3)          BUY DISPLAY                                 
*                                                                               
         GOTO1 =V(REGENPBY),DMCB,AIO3,(32,(R3)),PARAS+4,AIO1,RR=RELO            
         ZIC   R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BZ    LBX                 NO BUYLINES FOUND                            
         L     R4,AIO2                                                          
         LA    R4,2000(R4)                                                      
                                                                                
LB20     DS    0H                                                               
         MVC   8(78,R2),1(R4)                                                   
         NI    1(R2),X'FF'-X'08'   SET TO NORMAL                                
         BAS   RE,BUMPNEXT                                                      
                                                                                
         LA    RF,REVENDLH         CHECK IF WE'RE EXCEEDING SCREEN SIZE         
         CR    R2,RF                                                            
         BH    LBX                 SKIP IF BUY LIST DOESN'T FIT!                
                                                                                
         LA    R4,81(R4)                                                        
         BCT   R3,LB20                                                          
                                                                                
LBX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
***************                                                                 
* FOR ACTION LIST                                                               
***************                                                                 
STPFKL00 CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   EXIT                                                             
*                                                                               
         CLI   PFKEY,0             ENTER KEY IS OKAY                            
         BE    STPFKL10                                                         
*                                                                               
STPFKL10 LA    R2,LPFTABLE         YES, USE LIST PFKEY TABLE                    
         TM    CTLRFLG1,CF1BRDQ    BRAND ORDER IN PROCESS??                     
         BZ    STPFINIT                                                         
         LA    R2,BLPFTAB                                                       
*                                                                               
STPFINIT GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
STPFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUMP TO NEXT FIELD                                                            
***********************************************************************         
BUMPNEXT DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                                  
***********************************************************************         
LPFTABLE DS    0C                                                               
*                                                                               
* RETURN TO SELECT SCREEN                                                       
         DC    AL1(LPF12X-*,12,0,0,(LPF12X-LPF12)/KEYLNQ,0)                     
         DC    CL3' ',CL8'ORDER',CL8'SELECT '                                   
LPF12    DC    AL1(KEYTYTWA,L'REVHDLN-1),AL2(REVHDLN-T80FFFD)                   
LPF12X   EQU   *                                                                
*                                                                               
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                                  
***********************************************************************         
BLPFTAB  DS    0C                                                               
*                                                                               
* RETURN TO SELECT SCREEN                                                       
         DC    AL1(BLPF12X-*,12,0,0,(BLPF12X-BLPF12)/KEYLNQ,0)                  
         DC    CL3' ',CL8'BRAND',CL8'SELECT '                                   
BLPF12   DC    AL1(KEYTYTWA,L'REVHDLN-1),AL2(REVHDLN-T80FFFD)                   
BLPF12X  EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(1)                                                   
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
INVLRCAC MVC   RERROR,=AL2(INVRCACT)                                            
         B     ERREND                                                           
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
INVPFERR MVI   GERROR1,ERINVPFK    INVALID PFKEY                                
         B     ERREND                                                           
*                                                                               
NEXTREQ  MVC   RERROR,=AL2(3)      ENTER NEXT REQUEST                           
         B     INFEND                                                           
*                                                                               
NEXTPAGE MVC   RERROR,=AL2(111)    HIT ENTER FOR NEXT SCREEN                    
         B     INFEND                                                           
*                                                                               
HITBOTTM MVC   RERROR,=AL2(110)    BUY DISPLAYED                                
         B     INFEND                                                           
*                                                                               
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR                                                          
*                                                                               
INFEND   DS    0H                                                               
         LA    R2,REVABUYH                                                      
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BUYTITLE DC    C'C Lin Days         Times       Len Eff. Dates    Nw'           
         DC    C'         Npw       Rate Spt'                                   
BUYTITLX EQU   *-BUYTITLE                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REDARFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REDARF5D                                                       
       ++INCLUDE REDARWORKD                                                     
       ++INCLUDE REDARDSECT                                                     
         PRINT ON                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
DISPFLAG DS    X                   DISPLAY STATUS                               
FIRSTPG  EQU   X'02'               DISPLAY FIRST PAGE                           
NEXTPG   EQU   X'04'               DISPLAY NEXT PAGE                            
BUYKEY   DS    CL(L'RDARKEY)       HAS LAST BUY KEY DISPLAYED                   
BLISTNUM DS    X                   NUMBER OF AVAIL ROWS FOR BUY LIST            
BLISTMAX EQU   16                  MAX IS 16 ROWS FOR BUY LIST                  
BUYNUM   DS    X                   NEXT BUY NUMBER                              
SORTAREA DS    255CL6              CONTRACT BUYS SORT AREA                      
*                                  BYTE 1 = AGENCY BUY NUMBER                   
*                                  BYTE 2 = CONTRACT BUY NUMBER                 
*                                  BYTE 3-6 = CONTRACT BUY D/A                  
*                                                                               
* BUY LIST LINE                                                                 
*                                                                               
LBUYD    DSECT                                                                  
LBUYMC   DS    CL2                                                              
         DS    CL1                                                              
LBUYLINE DS    CL3                                                              
         DS    CL1                                                              
LBUYDAYS DS    CL12                                                             
         DS    CL1                                                              
LBUYTIME DS    CL12                                                             
         DS    CL1                                                              
LBUYLEN  DS    CL4                                                              
         DS    CL1                                                              
LBUYDATE DS    CL11                                                             
         DS    CL1                                                              
LBUYNW   DS    CL3                                                              
         DS    CL1                                                              
LBUYNPW  DS    CL3                                                              
         DS    CL1                                                              
LBUYRATE DS    CL10                                                             
         DS    CL1                                                              
LBUYSPT  DS    CL4                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039REDAR40   08/07/97'                                      
         END                                                                    
