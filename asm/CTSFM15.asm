*          DATA SET CTSFM15    AT LEVEL 009 AS OF 05/01/02                      
*PHASE TA0A15A                                                                  
                                                                                
***********************************************************************         
* TITLE: TA0A15 - OFFICE RECORD MAINT/LIST                            *         
* COMMENTS: MAINTAINS ADDS OFFICE RECORDS                             *         
***********************************************************************         
                                                                                
         TITLE 'TA0A15 OFFICE MAINTENANCE/LIST'                                 
TA0A15   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A15**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVI   ACTELOPT,C'Y'       CREATE ACTIVITY ELEMENT ON RECORDS           
         MVI   NLISTS,NLSTLNQ                                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE A RECORD                              
         BE    DELREC                                                           
         CLI   MODE,RECREST        RESTORE A RECORD                             
         BE    RESREC                                                           
         CLI   MODE,XRECPUT        RECORD JUST REPLACED                         
         BE    XRR                                                              
         CLI   MODE,XRECADD        RECORD JUST ADDED                            
         BE    XRA                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALIDATE KEY ROUTINE                                               *          
**********************************************************************          
                                                                                
VK       XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING COCREC,R6                                                        
         MVI   COCKTYP,COCKTYPQ    RECORD TYPE X'05'                            
         MVI   COCKSUB,COCKSUBQ    SUB TYPE X'0B'                               
         MVC   COCKAGY,AGENCY      AGENCY                                       
*                                                                               
         CLI   ACTNUM,ACTLIST      IF ACTION LIST OR REPORT                     
         BE    VKX                                                              
         CLI   ACTNUM,ACTREP                                                    
         BE    VKX                 THAT'S ALL                                   
*                                                                               
VK2      LA    R2,OFFNUMH          OFFICE NUMBER                                
         CLI   5(R2),1                                                          
         BL    MISSFLD                                                          
         BH    VK3                                                              
         CLI   8(R2),C'?'          ALLOW ? TO GET NEXT NUMBER                   
         BNE   VK3                                                              
         MVI   8(R2),C'0'          CHANGE ? TO 0                                
         OI    4(R2),X'08'         SET NUMERIC                                  
         OI    6(R2),X'80'                                                      
VK3      CLI   5(R2),3                                                          
         BH    INVLFLD                                                          
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    NOTNFLD                                                          
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STC   R0,COCKHEX                                                       
         CLI   COCKHEX,127         THIS VALUE OCCUPIED BY 03FE ELEMENT          
         BE    INVLFLD                                                          
         CLI   COCKHEX,254         MAXIMUM VALUE                                
         BNL   INVLFLD                                                          
*                                                                               
         CLI   COCKHEX,0           OFFICE=0 OK FOR ACTION ADD                   
         BNE   VKX                                                              
         CLI   ACTNUM,ACTADD                                                    
         BNE   INVLFLD                                                          
         BAS   RE,RO               READ OFFICE RECORDS                          
         CLI   OFFNEXT,0                                                        
         BE    INVLFLD                                                          
         MVC   COCKHEX,OFFNEXT     SET NEXT AVAIL HEX CODE IN KEY               
         SR    R0,R0                                                            
         IC    R0,COCKHEX                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(3,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
VKX      MVC   SVKEY,KEY                                                        
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE RECORD ROUTINE                                            *          
**********************************************************************          
                                                                                
VR       LA    R6,ELEM                                                          
         USING COCELEM,R6                                                       
         MVI   OFFLASTH,0                                                       
         XC    OFFLASTC,OFFLASTC                                                
         XC    ELEM,ELEM                                                        
         MVI   COCELCD,COCELCDQ                                                 
         CLI   ACTNUM,ACTADD                                                    
         BE    VR2                                                              
         MVI   ELCODE,COCELCDQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
VR2      LA    R2,OFFCODEH         OFFICE CODE                                  
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   OFFCODE,C' '                                                     
         BNH   INVLFLD                                                          
         MVC   COCCODE,OFFCODE                                                  
         CLI   COCCODE+1,0                                                      
         BNE   *+8                                                              
         MVI   COCCODE+1,C' '                                                   
*                                                                               
VR4      BAS   RE,RO               READ EXISTING OFFICE RECORDS                 
         LA    RF,OFFUSED+2                                                     
         LA    R1,1                                                             
VR6      CLC   COCCODE,0(RF)       CHECK OFFICE CODE ALREADY EXISTS             
         BNE   VR8                                                              
         L     RE,AIO                                                           
         CLM   R1,1,COCKHEX-COCKEY(RE)                                          
         BNE   INVLFLD                                                          
VR8      LA    R1,1(R1)            BUMP HEX CODE                                
         LA    RF,2(RF)                                                         
         CLC   0(2,RF),=X'03FE'    SKIP PAST ELEMENT CODE ENTRY                 
         BE    VR8                                                              
         CHI   R1,254                                                           
         BNH   VR6                                                              
*                                                                               
VR10     LA    R2,OFFNAMEH         OFFICE NAME                                  
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COCNAME(0),OFFNAME                                               
         LA    R1,COCNAME-COCELEM+1(R1)                                         
         STC   R1,COCELLN                                                       
*                                                                               
VR20     GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VR30                                                             
         DC    H'0'                                                             
*                                                                               
VR30     MVC   OFFLASTC,COCCODE    SAVE LAST OFFICE CODE AND HEX                
         L     RE,AIO                                                           
         MVC   OFFLASTH,COCKHEX-COCKEY(RE)                                      
VRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                     *          
***********************************************************************         
                                                                                
DR       LA    R4,OFFCODEH         CLEAR SCREEN FIELDS                          
         TWAXC (R4)                                                             
*                                                                               
         L     R6,AIO                                                           
         USING COCREC,R6                                                        
         MVI   ELCODE,COCELCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING COCELEM,R6                                                       
*                                                                               
         MVC   OFFCODE,COCCODE     OFFICE 2 CHR ALPHA CODE                      
         OI    OFFCODEH+6,X'80'                                                 
*                                                                               
         XC    OFFNAME,OFFNAME     OFFICE NAME                                  
         LA    R0,COCNAME-COCELEM                                               
         SR    R1,R1                                                            
         IC    R1,COCELLN                                                       
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFNAME(0),COCNAME                                               
         OI    OFFNAMEH+6,X'80'                                                 
*                                                                               
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
********************************************************************            
* DISPLAY KEY                                                                   
********************************************************************            
                                                                                
DK       LA    R6,KEY              DISPLAY OFFICE NUMBER                        
         USING COCREC,R6                                                        
         SR    R0,R0                                                            
         IC    R0,COCKHEX                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  OFFNUM(3),DUB                                                    
         OI    OFFNUMH+6,X'80'                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* RECORD JUST REPLACED OR ADDED                                      *          
**********************************************************************          
                                                                                
XRR      EQU   *                                                                
XRA      EQU   *                                                                
         SR    RE,RE               UPDATE LIST OF OFFICE CODES USED             
         ICM   RE,1,OFFLASTH                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SLL   RE,1                                                             
         LA    RE,OFFUSED(RE)                                                   
         MVC   0(2,RE),OFFLASTC    SAVE LAST OFFICE CODE AND HEX                
*                                                                               
         LA    R6,KEY              SET KEY TO READ OFFICE X'00'                 
         XC    KEY,KEY                                                          
         USING COCREC,R6                                                        
         MVI   COCKTYP,COCKTYPQ    X'05' REC TYPE                               
         MVI   COCKSUB,COCKSUBQ    X'0B' REC SUB TYPE                           
         MVC   COCKAGY,AGENCY      AGENCY                                       
         MVC   AIO,AIO2                                                         
*                                                                               
XR00     GOTO1 HIGH                READ OFFICE X'00' RECORD                     
         L     R6,AIO              POINT TO NEW RECORD                          
         MVI   WORK,C'N'           SET DOESNT EXIST                             
         CLI   COCKTYP,COCKTYPQ    RECORD TYPE                                  
         BNE   XR01                                                             
         CLI   COCKSUB,COCKSUBQ    SUB TYPE                                     
         BNE   XR01                                                             
         CLC   COCKAGY,AGENCY      AGENCY                                       
         BNE   XR01                                                             
         CLI   COCKHEX,0           HEX CODE                                     
         BNE   XR01                                                             
         MVI   WORK,C'Y'           SET ALREADY EXISTS                           
*                                                                               
XR01     XC    0(28,R6),0(R6)      BUILD NEW OFFICE X'00' RECORD                
         MVI   COCKTYP,COCKTYPQ    X'05' REC TYPE                               
         MVI   COCKSUB,COCKSUBQ    X'0B' REC SUB TYPE                           
         MVC   COCKAGY,AGENCY      AGENCY                                       
*                                                                               
XR02     LA    RE,COCDATA          POINT TO FIRST ELEMENT                       
         LA    RF,OFFUSED+2        POINT TO 1ST HALF OF OFFICE USED             
         MVI   0(RE),X'02'                                                      
         MVI   1(RE),254                                                        
         MVC   2(252,RE),0(RF)                                                  
*                                                                               
XR03     LA    RE,COCDATA+254      POINT TO SECOND ELEMENT                      
         LA    RF,OFFUSED+256      POINT TO 2ND HALF OF OFFICE USED             
         MVI   0(RE),X'03'                                                      
         MVI   1(RE),254                                                        
         MVC   2(252,RE),0(RF)                                                  
*                                                                               
XR04     LA    RE,COCDATA+508      POINT TO END OF RECORD                       
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         SR    RE,R6               SET RECORD LENGTH                            
         STCM  RE,3,COCLEN                                                      
*                                                                               
XR05     CLI   WORK,C'Y'           ADD/CHANGE THE OFFICE X'00' RECORD           
         BE    XR06                                                             
         GOTO1 ADD                                                              
         B     XR07                                                             
XR06     GOTO1 WRITE                                                            
*                                                                               
XR07     CLI   DMCB+8,0                                                         
         BE    XRX                                                              
         DC    H'0'                                                             
*                                                                               
XRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE/RESTORE A RECORD                                  *         
***********************************************************************         
                                                                                
DELREC   B     DR                  DISPLAY RECORD ON DELETE                     
*                                                                               
RESREC   L     R1,AIO              TEST RECORD IS DELETED                       
         TM    COCSTAT-COCREC(R1),X'80'                                         
         BO    DR                  DISPLAY RECORD ON RESTORE                    
         B     RECNOTD                                                          
         EJECT                                                                  
***********************************************************************         
* ONLINE LIST ROUTINE                                                           
***********************************************************************         
                                                                                
LR       LA    R5,LISTAR           R5=A(LIST SCREEN LINE)                       
         USING PLINED,R5                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
         LA    R3,HEADING          SET UP REPORT HEADINGS                       
         ST    R3,SPECS                                                         
         LA    R3,HDRTN                                                         
         ST    R3,HEADHOOK                                                      
         LA    R5,P                R5=A(PRINT LINE)                             
*                                                                               
LR02     LA    R6,KEY                                                           
         USING COCREC,R6                                                        
*                                                                               
LR05     OC    KEY(L'COCKEY),KEY   FIRST TIME THROUGH                           
         BNZ   LR40                                                             
         MVI   COCKTYP,COCKTYPQ    X'05' REC TYPE                               
         MVI   COCKSUB,COCKSUBQ    X'0B' REC SUB TYPE                           
         MVC   COCKAGY,AGENCY      AGENCY                                       
*                                                                               
         LA    R2,LSTNUMH          OFFICE NUMBER                                
         CLI   5(R2),0                                                          
         BE    LR10                                                             
         CLI   5(R2),3                                                          
         BH    LR10                                                             
         TM    4(R2),X'08'         IGNORE IF NOT NUMERIC                        
         BZ    LR10                                                             
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STC   R0,COCKHEX                                                       
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR40                                                             
LR20     LA    R6,KEY                                                           
         GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
LR40     CLI   COCKTYP,COCKTYPQ    RECORD TYPE                                  
         BNE   LRX                                                              
         CLI   COCKSUB,COCKSUBQ    SUB TYPE                                     
         BNE   LRX                                                              
         CLC   COCKAGY,AGENCY      AGENCY                                       
         BNE   LRX                                                              
         CLI   COCKHEX,0           BYPASS OFFICE NUMBER ZERO RECORD             
         BE    LR20                                                             
*                                                                               
         L     R6,AIO                                                           
         USING COCREC,R6                                                        
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
*                                                                               
         SR    R0,R0               OFFICE NUMBER                                
         IC    R0,COCKHEX                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LISTAR+3(3),DUB                                                  
*                                                                               
         MVI   ELCODE,COCELCDQ     GET OFFICE CODE/NAME ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   LR20                BACK FOR NEXT IF NO ELEMENT                  
         USING COCELEM,R6                                                       
         MVC   LISTAR+8(2),COCCODE                                              
         LA    R0,COCNAME-COCELEM                                               
         SR    R1,R1                                                            
         IC    R1,COCELLN                                                       
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTAR+14(0),COCNAME                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
LR65     CLI   MODE,PRINTREP       TEST LIST/REPORT MODE                        
         BE    LR70                                                             
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LR20                                                             
*                                                                               
LR70     L     R6,AIO              FORMAT REPORT LINE FOR PRINTING              
         USING COCREC,R6                                                        
         SR    R0,R0               OFFICE NUMBER                                
         IC    R0,COCKHEX                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLNUM(3),DUB                                                     
*                                                                               
         MVI   ELCODE,COCELCDQ     GET OFFICE CODE/NAME ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING COCELEM,R6                                                       
         MVC   PLCODE,COCCODE                                                   
         LA    R0,COCNAME-COCELEM                                               
         SR    R1,R1                                                            
         IC    R1,COCELLN                                                       
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLNAME(0),COCNAME                                                
*                                                                               
LR80     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
*                                                                               
LRX      B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* HEADER ROUTINE                                                      *         
***********************************************************************         
                                                                                
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,25,C'OFFICE RECORDS'                                          
         SSPEC H2,25,C'--------------'                                          
         SSPEC H1,55,AGYNAME                                                    
         SSPEC H2,55,AGYADD                                                     
         SSPEC H3,55,REPORT                                                     
         SSPEC H4,55,RUN                                                        
         SSPEC H5,55,PAGE                                                       
         DC    X'0'                                                             
*                                                                               
HDRTN    NTR1                                                                   
         LA    R4,H8                                                            
         USING PLINED,R4                                                        
         MVC   PLNUM(34),=C'NUM CODE NAME                     '                 
         LA    R4,H9                                                            
         MVC   PLNUM(34),=C'--- ---- --------------------     '                 
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
INVLFLD  MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
*                                                                               
MISSFLD  MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
*                                                                               
NOTNFLD  MVC   GERROR,=AL2(NOTNUM)                                              
         B     VSFMERR                                                          
*                                                                               
RECNOTD  MVC   GERROR,=AL2(RECNTDEL)                                            
         B     VSFMERR                                                          
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ OFFICE RECORDS TO FIND AVAILABLE HEX CODES          *         
***********************************************************************         
                                                                                
RO       NTR1                                                                   
         MVI   OFFNEXT,0           SET NEXT AVAIL OFFICE CODE                   
         MVC   SVKEY,KEY                                                        
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO2            SET I/O TO READ OFFICE RECORDS               
         LA    R6,KEY              SET KEY TO READ OFFICE RECORDS               
         USING COCREC,R6                                                        
         XC    COCKEY,COCKEY                                                    
         MVI   COCKTYP,COCKTYPQ    X'05' REC TYPE                               
         MVI   COCKSUB,COCKSUBQ    X'0B' REC SUB TYPE                           
         MVC   COCKAGY,AGENCY      AGENCY                                       
*                                                                               
         CLC   OFFUKEY,KEY         TEST IF ALREADY HAVE BUILT THIS LIST         
         BE    RO20                                                             
         MVC   OFFUKEY,KEY                                                      
         XC    OFFUSED(256),OFFUSED                                             
         XC    OFFUSED+256(256),OFFUSED+256                                     
         MVC   OFFUSED(2),=X'02FE'                                              
         MVC   OFFUSED+254(2),=X'03FE'                                          
*                                                                               
RO2      GOTO1 HIGH                FIRST RECORD                                 
         B     RO6                                                              
RO4      GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
RO6      L     R6,AIO                                                           
         USING COCREC,R6                                                        
         CLI   COCKTYP,COCKTYPQ    RECORD TYPE                                  
         BNE   RO20                                                             
         CLI   COCKSUB,COCKSUBQ    SUB TYPE                                     
         BNE   RO20                                                             
         CLC   COCKAGY,AGENCY      AGENCY                                       
         BNE   RO20                                                             
         CLI   COCKHEX,0           TEST IF ZERO HEX REC EXISTS                  
         BNE   RO8                                                              
         L     RE,AIO                                                           
         LA    RE,COCDATA-COCREC(RE)                                            
         MVC   OFFUSED(254),0(RE)                                               
         MVC   OFFUSED+254(254),254(RE)                                         
         B     RO20                                                             
*                                                                               
RO8      L     R6,AIO              POINT TO NEW RECORD                          
         USING COCREC,R6                                                        
         MVC   WORK(1),COCKHEX     SAVE HEX CODE                                
         MVC   WORK+1(2),=C'??'    INIT OFFICE CODE                             
         MVI   ELCODE,COCELCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   RO10                                                             
         USING COCELEM,R6                                                       
         MVC   WORK+1(2),COCCODE   SAVE OFFICE CODE                             
*                                                                               
RO10     SR    RF,RF               PUT OFFICE CODE IN LIST                      
         IC    RF,WORK                                                          
         SLL   RF,1                                                             
         LA    RF,OFFUSED(RF)                                                   
         MVC   0(2,RF),WORK+1                                                   
         B     RO4                 BACK FOR NEXT RECORD                         
*                                                                               
RO20     LA    RF,OFFUSED+2        GET NEXT AVAILABLE HEX CODE                  
         LA    R1,1                                                             
RO22     OC    0(2,RF),0(RF)                                                    
         BZ    RO24                                                             
         LA    R1,1(R1)                                                         
         LA    RF,2(RF)                                                         
         CHI   R1,254                                                           
         BL    RO22                                                             
         B     ROX                 EXIT WITH ZERO IF NO CODE AVAIL              
*                                                                               
RO24     STC   R1,OFFNEXT                                                       
*                                                                               
ROX      MVC   AIO,SVAIO                                                        
         MVC   KEY,SVKEY                                                        
         CLI   OFFNEXT,0           SET CC EQL IF NO OFFICE HEX FOUND            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
*&&UK                                                                           
       ++INCLUDE CTGENOFC                                                       
*&&                                                                             
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMCCD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMCDD                                                       
NLSTLNQ  EQU   (LSTSELX-LSTSEL1)/(LSTSEL2-LSTSEL1)+1                            
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE            TA0A15 STORAGE AREAS                         
SVAIO    DS    AL4                                                              
SVKEY    DS    XL25                                                             
OFFUKEY  DS    XL25                                                             
OFFUSED  DS    XL512                                                            
OFFNEXT  DS    X                                                                
OFFLASTH DS    X                                                                
OFFLASTC DS    CL2                                                              
                                                                                
PLINED   DSECT                                                                  
PLNUM    DS    CL3                                                              
         DS    CL1                                                              
PLCODE   DS    CL2                                                              
         DS    CL3                                                              
PLNAME   DS    CL20                                                             
PEND     EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009CTSFM15   05/01/02'                                      
         END                                                                    
