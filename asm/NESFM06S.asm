*          DATA SET NESFM06S   AT LEVEL 063 AS OF 05/01/02                      
*          DATA SET NESFM06C   AT LEVEL 064 AS OF 03/03/00                      
*PHASE T31C06A                                                                  
T31C06   TITLE 'NESFM06 - STATION MASTER'                                       
***********************************************************************         
T31C06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C06,R7,RR=R8                                                
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    MAIN15                                                           
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    MAIN15                                                           
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    NOTAUTH                                                          
         CLI   ACTNUM,ACTCHA       ACTION CHANGE?                               
         BE    NOTAUTH                                                          
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         BNE   MAIN15                                                           
         B     NOTAUTH                                                          
*                                                                               
MAIN15   BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,SETFILE        SET FILE NAME                                
         BE    SF                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
DLCHK    CLI   MODE,RECDEL         DELETE RECORDS                               
         BNE   LRCHK                                                            
         BAS   RE,DL                                                            
*                                                                               
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,XRECREST       RESTORED RECORD                              
         BE    RSTRDEF                                                          
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         LA    R5,HEDSPECS                                                      
         ST    R5,SPECS                                                         
         B     LR                                                               
**************************************                                          
* SET FILE                                                                      
SF       DS    0H                                                               
         OI    GENSTAT4,NODELLST                                                
         BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF           SET FILENAME & OTHER DEFAULTS                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
DL       NTR1                                                                   
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
*&&DO                                                                           
         CLI   MODE,XRECDEL        DELETED RECORD                               
         BE    DELR                                                             
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         BE    XRP                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*&&                                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
********************************************************************            
* VALIDATE KEY                                                                  
********************************************************************            
VK       MVC   QNET,SPACES                                                      
         MVC   QCLT,ZEROES                                                      
         XC    SVQMED,SVQMED                                                    
*                                                                               
         LA    R2,MSTMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 VALIMED                                                          
         MVC   SVQMED,QMED                                                      
*                                                                               
         CLI   USERPROF+7,C'C'      TEST CANADA                                 
         BNE   VK10                                                             
         CLI   QMED,C'N'           DON'T ALLOW MEDIA N OR C                     
         BE    INVERR                                                           
         CLI   QMED,C'C'                                                        
         BE    INVERR                                                           
*                                                                               
VK10     LA    R2,MSTSTAH          STATION FIELD                                
         CLI   5(R2),0                                                          
         BNE   VK30                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+14                                                             
         XC    QNET,QNET                                                        
         B     VK50                                                             
         BE    MISSERR                                                          
*                                                                               
VK30     CLI   5(R2),4             L'INPUT S/B <=4!                             
         BH    INVERR                                                           
         CLC   =C'0000',8(R2)      DON'T ALLOW STATION 0000                     
         BE    INVERR                                                           
         TM    4(R2),X'04'                                                      
         BZ    INVERR                                                           
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   QNET(0),8(R2)                                                    
*                                                                               
VK50     LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   STAKEY(STAKEYLN),ZEROES                                          
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVC   STAKMED,QMED        MEDIA                                        
         OC    STAKCALL(4),=XL4'40404040'                                       
         MVC   STAKCALL+4(1),QMED                                               
         MVC   STAKCALL(4),QNET                                                 
         MVC   STAKAGY,AGENCY      AGENCY                                       
         MVC   STAKCLT,QCLT        CLIENT EXCEPTION                             
         MVC   FILENAME,=C'STATION'                                             
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
         BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF                                                        
*                                                                               
VKX      B     XIT                                                              
         DROP  R4                                                               
***************************************************************                 
*      DISPLEY REC                                                              
***********************************************************                     
DR       L     R6,AIO                                                           
         USING STAREC,R6                                                        
*                                                                               
         MVC   MSTMKT,SMKT                                                      
         OI    MSTMKTH+6,X'80'                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,ZEROES                                                       
         LA    R4,KEY                                                           
         USING MKTRECD,R4                                                       
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,SMKT                                                     
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'MKTKEY),KEYSAVE                                            
         BE    *+14                                                             
         MVC   MSTMKTN,=C'? MARKET REC NOT FOUND ?'                             
         B     *+10                                                             
*                                                                               
         MVC   MSTMKTN,MKTNAME                                                  
         OI    MSTMKTNH+6,X'80'                                                 
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   MSTSIZE,SSIZE                                                    
         OI    MSTSIZEH+6,X'80'                                                 
*                                                                               
         EDIT (B2,SNEWTAX),(6,MSTTAX),3,ZERO=BLANK,ALIGN=LEFT                   
         OI    MSTTAXH+6,X'80'                                                  
*                                                                               
DR50     MVC   MSTPREP,SPAYREP                                                  
         OI    MSTPREPH+6,X'80'                                                 
         OC    SPAYREP,SPAYREP                                                  
*        XC    REPFLD,REPFLD                                                    
         MVC   REPFLD,ZEROES                                                    
         MVC   REPFLD,SPAYREP                                                   
         BAS   RE,GETREP                                                        
         MVC   MSTPRNM,REPNAME                                                  
         OI    MSTPRNMH+6,X'80'                                                 
*                                                                               
DR80     MVC   MSTTSRP,SCONREP                                                  
         OI    MSTTSRPH+6,X'80'                                                 
*        XC    REPFLD,REPFLD                                                    
         MVC   REPFLD,ZEROES                                                    
         MVC   REPFLD,SCONREP                                                   
         BAS   RE,GETREP                                                        
         MVC   MSTTSRN,REPNAME                                                  
         OI    MSTTSRNH+6,X'80'                                                 
*                                                                               
DR120    MVC   MSTTRAF,STRFREP                                                  
         OI    MSTTRAFH+6,X'80'                                                 
         XC    REPFLD,REPFLD                                                    
         MVC   REPFLD,ZEROES                                                    
         MVC   REPFLD,STRFREP                                                   
         BAS   RE,GETREP                                                        
         MVC   MSTTRAN,REPNAME                                                  
         OI    MSTTRANH+6,X'80'                                                 
*   NO NEED TO CHECK IF NETPAK, SINCE THIS IS A NET PROGRAM                     
         MVC   MSTTYPE(1),STYPE                                                 
         MVC   MSTTYPE+1(1),SUBMEDIA                                            
         MVC   MSTTYPE+2(1),SPTYPE                                              
         OI    MSTTYPEH+6,X'80'                                                 
*                                                                               
         MVC   MSTGST,SGSTCODE                                                  
         OI    MSTGSTH+6,X'80'                                                  
*   PST CODE                                                                    
         BAS   RE,DISPPST                                                       
*                                                                               
         MVC   MSTNTIS,SNTISTA                                                  
         OI    MSTNTISH+6,X'80'                                                 
*                                                                               
         MVC   MSTCTY(1),SCOUNTRY                                               
         OI    MSTCTYH+6,X'80'                                                  
                                                                                
*                                                                               
         XC    MSTCTAX,MSTCTAX                                                  
         OI    MSTCTAXH+6,X'80'                                                 
*                                                                               
         CLI   USERPROF+7,C'C'      TEST CANADA                                 
         BNE   DR150                                                            
*  CANADA ONLY                                                                  
*                                                                               
         MVC   MSTCTY(1),SCOUNTRY                                               
         CLI   SCOUNTRY,C'C'                                                    
         BNE   *+10                                                             
         MVC   MSTCTY+1(2),=C'AN'                                               
         CLI   SCOUNTRY,C'U'                                                    
         BL    *+10                                                             
         MVC   MSTCTY+1(2),=C'SA'                                               
         OI    MSTCTYH+6,X'80'                                                  
*                                                                               
         XC    MSTCTAX,MSTCTAX                                                  
         OC    SCANTAX,SCANTAX                                                  
         BZ    DR140                                                            
         EDIT (B2,SCANTAX),(6,MSTCTAX),3,ZERO=BLANK,ALIGN=LEFT                  
DR140    OI    MSTCTAXH+6,X'80'                                                 
         MVC   MSTAFF(L'SNETWRK),SCANNTWK                                       
         B     DR160                                                            
*                                                                               
DR150    MVC   MSTAFF(L'SNETWRK),SNETWRK                                        
DR160    OI    MSTAFFH+6,X'80'                                                  
*                                                                               
         MVC   MSTFAX,SFAX                                                      
         OI    MSTFAXH+6,X'80'                                                  
*                                                                               
         MVC   MSTBOOK,SBKTYPE                                                  
         OI    MSTBOOKH+6,X'80'                                                 
*                                                                               
         MVC   MSTCHAN,SCHNL                                                    
         OI    MSTCHANH+6,X'80'                                                 
*                                                                               
         XC    MSTFEE,MSTFEE                                                    
         OC    SSVCFEE,SSVCFEE                                                  
         BZ    DR180                                                            
         EDIT  SSVCFEE,(5,MSTFEE),2                                             
         OI    MSTFEEH+6,X'80'                                                  
*                                                                               
DR180    MVC   MSTTWIX,STWIX                                                    
         OI    MSTTWIXH+6,X'80'                                                 
*                                                                               
         XC    MSTBANK,MSTBANK                                                  
         OI    MSTBANKH+6,X'80'                                                 
*                                                                               
         OC    STIMEBK,STIMEBK                                                  
         BZ    DR200                                                            
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),STIMEBK                                                   
         EDIT  (B4,STIMEBK),(10,MSTBANK),ALIGN=LEFT                             
DR200    OI    MSTBANKH+6,X'80'                                                 
         CLI   ACTNUM,ACTADD                                                    
         BE    DR220                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'STAKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DR220    BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF                                                        
*                                                                               
DRX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
***************************************************************                 
*=============== VALREC ======================================*                 
*--IN NETPAK GOALS THE DEFAULT MARKET NUMBERS ARE 7777, AND 777                 
*--BECAUSE OF THIS, THESE MARKET NUMBERS CANNOT BE ASSIGNED TO                  
*--REGULAR STATIONS.                                                            
VR       LA    R2,MSTMKTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
         CLC   8(4,R2),=C'7777'                                                 
         BE    INVERR                                                           
         CLC   8(4,R2),=C'0777'                                                 
         BE    INVERR                                                           
*                                                                               
         TM    4(R2),X'08'         NUMERIC ONLY                                 
         BZ    INVERR                                                           
*                                                                               
* =====  FORMAT MARKET TO BE 4 CHARACTER FORM ====== *                          
         GOTO1 ANY                                                              
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),WORK(0)                                                   
         NI    DUB+7,X'F0'                                                      
         OI    DUB+7,X'0C'                                                      
         CVB   RE,DUB                                                           
         XC    DUB,DUB                                                          
         STCM  RE,15,DUB                                                        
         EDIT  (B4,DUB),(4,SVQMKT),FILL=0                                       
*        UNPK  SVQMKT,DUB                                                       
*        NI    SVQMKT+3,X'0F'                                                   
*        OI    SVQMKT+3,X'F0'                                                   
*   CHECK IF MARKET EXIST                                                       
         BAS   RE,CHKMKT                                                        
         L     R6,AIO1                                                          
         USING STARECD,R6                                                       
*                                                                               
         XC    STAKLEN,STAKLEN                                                  
         MVC   STAKLEN,=H'144'                                                  
*                                                                               
VR30     CLI   ACTNUM,ACTADD                                                    
         BNE   VR50                                                             
****     CLI   SVQMED,C'N'         BAD BUG!                                     
****     BNE   VR65                                                             
         BAS   RE,DUPMKT                                                        
         B     VR65        80                                                   
*   ACTION CHANGE                                                               
VR50     CLI   SVQMED,C'N'                                                      
         BE    VR60                                                             
VR60     CLC   SVQMKT,SMKT                                                      
         BNE   CHGERR          MARKET DOESN'T MATCH ON CHANGE                   
*                                                                               
VR65     XC    SMKT,SMKT                                                        
         MVC   SMKT,SVQMKT                                                      
*   PAYING REP                                                                  
         LA    R2,MSTPREPH                                                      
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   8(3,R2),=C'000'                                                  
         MVI   5(R2),3                                                          
*                                                                               
         CLI   5(R2),3                                                          
         BNE   INVERR                                                           
*                                                                               
         XC    REPFLD,REPFLD                                                    
         MVC   REPFLD,MSTPREP                                                   
         BAS   RE,GETREP                                                        
         XC    MSTPRNM,MSTPRNM                                                  
         MVC   MSTPRNM,REPNAME                                                  
         OI    MSTPRNMH+6,X'80'                                                 
         MVC   SPAYREP,REPFLD                                                   
*    TIME SHEET REP                                                             
         LA    R2,MSTTSRPH                                                      
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   8(3,R2),=C'000'                                                  
         MVI   5(R2),3                                                          
         CLI   5(R2),3                                                          
         BNE   INVERR                                                           
*                                                                               
         XC    REPFLD,REPFLD                                                    
         MVC   REPFLD,MSTTSRP                                                   
         BAS   RE,GETREP                                                        
         XC    MSTTSRN,MSTTSRN                                                  
         MVC   MSTTSRN,REPNAME                                                  
         OI    MSTTSRNH+6,X'80'                                                 
         MVC   SCONREP,REPFLD                                                   
*   TRAFFIC REP                                                                 
         LA    R2,MSTTRAFH                                                      
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   8(3,R2),=C'000'                                                  
         MVI   5(R2),3                                                          
         CLI   5(R2),3                                                          
         BNE   INVERR                                                           
*                                                                               
         XC    REPFLD,REPFLD                                                    
         MVC   REPFLD,MSTTRAF                                                   
         BAS   RE,GETREP                                                        
         XC    MSTTRAN,MSTTRAN                                                  
         MVC   MSTTRAN,REPNAME                                                  
         OI    MSTTRANH+6,X'80'                                                 
         MVC   STRFREP,REPFLD                                                   
*                                                                               
*   CHANNEL    TV=2 CHAR, BUT NET HAS NO MEDIA T                                
         LA    R2,MSTCHANH                                                      
         CLI   5(R2),0                                                          
         BE    VR70                                                             
         CLI   5(R2),4                                                          
         BNE   INVERR                                                           
*                                                                               
         MVC   SCHNL(4),MSTCHAN                                                 
*    NETWORK AFF                                                                
VR70     LA    R2,MSTAFFH                                                       
         XC    SNETWRK,SNETWRK                                                  
         XC    SCANNTWK,SCANNTWK                                                
         CLI   5(R2),0                                                          
         BE    VR75                                                             
         TM    4(R2),X'04'     TEST ALPHA                                       
         BZ    INVERR                                                           
         CLI   USERPROF+7,C'C'                                                  
         BNE   VR78                                                             
         MVC   SCANNTWK,8(R2)                                                   
         OC    SCANNTWK,SPACES                                                  
         B     VR75                                                             
*                                                                               
VR78     CLI   5(R2),3                                                          
         BH    INVERR                                                           
         MVC   SNETWRK,8(R2)                                                    
         OC    SNETWRK,SPACES                                                   
*                                                                               
*   STATION TYPE                                                                
VR75     LA    R2,MSTTYPEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
         MVI   STYPE,C' '                                                       
         MVI   SUBMEDIA,C' '                                                    
*                                                                               
         LA    R5,MTYPETAB                                                      
VR80     CLC   8(1,R2),0(R5)                                                    
         BE    VR90                                                             
         CLI   0(R5),X'FF'                                                      
         BE    INVERR                                                           
         LA    R5,1(R5)                                                         
         B     VR80                                                             
*                                                                               
VR90     MVC   STYPE,8(R2)                                                      
*                                                                               
         CLI   5(R2),2                                                          
         BL    VR120                                                            
         CLI   9(R2),C'N'                                                       
         BE    VR100                                                            
         CLI   9(R2),C'R'                                                       
         BE    VR100                                                            
         CLI   9(R2),X'40'                                                      
         BNH   VR120                                                            
         B     INVERR                                                           
*                                                                               
VR100    MVC   SUBMEDIA,9(R2)                                                   
*                                                                               
VR120    CLI   SPTYPE,X'40'                                                     
         BNH   *+14                                                             
         CLC   SPTYPE,10(R2)                                                    
         BE    VR140                                                            
*                                                                               
         MVC   SPTYPE,STYPE                                                     
*                                                                               
         CLI   5(R2),3                                                          
         BL    VR140                                                            
         LA    R5,PTYPETAB                                                      
VR130    CLC   10(1,R2),0(R5)                                                   
         BE    VR135                                                            
         CLI   0(R5),X'FF'                                                      
         BE    INVERR                                                           
         LA    R5,1(R5)                                                         
         B     VR130                                                            
*                                                                               
VR135    MVC   SPTYPE,10(R2)                                                    
         B     VR140                                                            
*                                                                               
*                                  MEDIA TYPE TABLE FOR NETPAK                  
MTYPETAB DC    C'N'                NETWORK                                      
         DC    C'C'                CABLE                                        
         DC    C'S'                SYNDICATION                                  
         DC    C'D'                RADIO                                        
         DC    C'H'                HISPANIC                                     
         DC    C'O'                OTHER                                        
         DC    X'FFFF'                                                          
*                                  POSTING TYPE TABLE FOR NETPAK                
PTYPETAB DC    C'N'                NETWORK                                      
         DC    C'C'                CABLE                                        
         DC    C'S'                SYNDICATION                                  
         DC    C'H'                HISPANIC                                     
         DC    C'O'                OTHER                                        
         DC    X'FFFF'                                                          
*   TWIX NUMBER                                                                 
VR140    LA    R2,MSTTWIXH                                                      
         CLI   5(R2),0                                                          
         BE    VR160                                                            
         MVC   STWIX,8(R2)                                                      
*   FAX NUMBER                                                                  
VR160    LA    R2,MSTFAXH                                                       
         XC    SFAX,SFAX                                                        
         CLI   5(R2),0                                                          
         BE    VR180                                                            
         MVC   SFAX,8(R2)                                                       
*   TIME BANK                                                                   
VR180    LA    R2,MSTBANKH                                                      
         XC    STIMEBK,STIMEBK                                                  
         CLI   5(R2),0                                                          
         BE    VR200                                                            
         CLI   5(R2),9                                                          
         BH    INVERR                                                           
         TM    4(R2),X'08'                                                      
         BZ    INVERR                                                           
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,15,STIMEBK                                                    
*   NEW TAX RATE                                                                
VR200    LA    R2,MSTTAXH                                                       
         XC    SNEWTAX,SNEWTAX                                                  
         CLI   5(R2),0                                                          
         BE    VR220                                                            
*                                                                               
         ZIC   R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(3,8(R2)),(R5)                                      
         CLI   DMCB,X'FF'                                                       
         BE    INVERR                                                           
         L     R5,DMCB+4                                                        
         C     R5,=F'32767'                                                     
         BH    INVERR                                                           
         LTR   R5,R5                                                            
         BM    INVERR                                                           
         STCM  R5,3,SNEWTAX                                                     
*    STATION SIZE                                                               
VR220    LA    R2,MSTSIZEH                                                      
         CLI   5(R2),0                                                          
         BE    VR240                                                            
         CLI   8(R2),X'A'                                                       
         BL    INVERR                                                           
         CLI   8(R2),C'Z'                                                       
         BH    INVERR                                                           
         MVC   SSIZE,8(R2)                                                      
*     COUNTRY                                                                   
VR240    LA    R2,MSTCTYH                                                       
         MVI   SCOUNTRY,0                                                       
         CLI   USERPROF+7,C'C'                                                  
         BNE   VR280                                                            
         SR    RE,RE                                                            
         CLI   5(R2),0                                                          
         BE    VR280                                                            
         CLI   5(R2),3                                                          
         BH    INVERR                                                           
*                                                                               
         BCTR  RE,0                                                             
         LA    R1,COUNTTAB                                                      
VR250    CLI   0(R1),0                                                          
         BE    INVERR                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),8(R2)                                                    
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     VR250                                                            
         MVC   SCOUNTRY,8(R2)                                                   
         B     VR280                                                            
*                                                                               
COUNTTAB DC    CL3'USA'       CAN STA/CAN $                                     
         DC    CL3'CAN'       USA STA/USA $                                     
         DC    CL3'VSA'       USA STA/CAN $                                     
         DC    X'00'                                                            
*  CANADA C-58 TAX                                                              
VR280    LA    R2,MSTCTAXH                                                      
         XC    SCANTAX,SCANTAX                                                  
         CLI   USERPROF+7,C'C'                                                  
         BNE   VR300                                                            
         CLI   5(R2),0                                                          
         BE    VR300                                                            
         ZIC   R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R5)                                      
         CLI   0(R1),0                                                          
         BNE   INVERR                                                           
         L     R5,4(R1)                                                         
         C     RE,=F'32767'                                                     
         BH    INVERR                                                           
         LTR   R5,R5                                                            
         BM    INVERR                                                           
         STCM  R5,3,SCANTAX                                                     
*                                                                               
*      CANADIAN C-58 SERVICE FEE                                                
VR300    LA    R2,MSTFEEH                                                       
         XC    SSVCFEE,SSVCFEE                                                  
         CLI   USERPROF+7,C'C'                                                  
         BNE   VR320                                                            
         CLI   5(R2),0                                                          
         BE    VR320                                                            
         ZIC   R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R5)                                      
         CLI   0(R1),0                                                          
         BNE   INVERR                                                           
         L     R5,4(R1)                                                         
         C     R5,=F'32767'                                                     
         BH    INVERR                                                           
         LTR   R5,R5                                                            
         BM    INVERR                                                           
         STCM  R5,3,SSVCFEE                                                     
*                                                                               
*     NTI STATION                                                               
VR320    LA    R2,MSTNTISH                                                      
         MVC   DUB,SPACES                                                       
         CLI   5(R2),0                                                          
         BE    VR350                                                            
         XC    DUB,DUB                                                          
         MVC   DUB(4),8(R2)                                                     
         OC    DUB(4),=4X'40'                                                   
VR350    MVC   SNTISTA,DUB                                                      
*     GOODS & SERVICE TAX CODE                                                  
         LA    R2,MSTGSTH                                                       
         MVI   SGSTCODE,0                                                       
         CLI   5(R2),0                                                          
         BE    VR400                                                            
         LA    R5,GSTTAB                                                        
VR360    CLC   0(1,R5),8(R2)                                                    
         BE    VR370                                                            
         CLI   0(R5),X'FF'                                                      
         BE    INVERR                                                           
         LA    R5,1(R5)                                                         
         B     VR360                                                            
VR370    MVC   SGSTCODE,MSTGST        MOVE IN CORRECT GST CODE                  
         B     VR400                                                            
*                                                                               
GSTTAB   DC    C'S'               STANDARD                                      
         DC    C'U'                                                             
         DC    C'X'                                                             
         DC    C'Z'               ZERO                                          
         DC    X'FFFF'            END OF TABLE                                  
*   SKIP BOOK SINCE THIS IS NETPAK SO DO PST CODE                               
VR400    LA    R2,MSTPSTH                                                       
         XC    SPST,SPST                                                        
         CLI   5(R2),0                                                          
         BE    VRX                                                              
         BAS   RE,VALPST                                                        
*                                                                               
VRX      MVC   AIO,AIO1                                                         
         B     DR                                                               
         DROP  R6                                                               
*                                                                               
***************************************************************                 
*=============   DISPLAY KEY      ============================*                 
*                                                                               
DK       L     R6,AIO                                                           
         USING STARECD,R6                                                       
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    DK10                                                             
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    DK10                                                             
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BE    NOTAUTH                                                          
*                                                                               
DK10     XC    MSTMED,MSTMED                                                    
         MVC   MSTMED,STAKMED                                                   
         OI    MSTMEDH+6,X'80'                                                  
*                                                                               
         XC    MSTSTA,MSTSTA                                                    
         MVC   MSTSTA,STAKCALL                                                  
         OI    MSTSTAH+6,X'80'                                                  
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,0(R6)                                                    
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
***************************************************************                 
*=============      LIST RECS     ============================*                 
         BAS   RE,SAVEDEF                                                       
LR       BAS   RE,SETDEF                                                        
         OC    KEY,KEY                                                          
         BNZ    LR10                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*        DC    H'0'                                                             
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
LR30     L     R6,AIO                                                           
         USING STARECD,R6                                                       
*                                                                               
         CLC   0(2,R6),KEYSAVE       SAME MEDIA?                                
         BNE   LRX                                                              
         CLC   STAKAGY,AGENCY       SAME AGENCY?                                
         BNE   LR20                                                             
         CLC   STAKCLT,ZEROES       NETPAK DOESN'T SUPPORT CLIENTS              
         BNE   LR20                                                             
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL,SMKT                                                        
         XC    KEY,KEY                                                          
         MVC   KEY,0(R6)                                                        
*                                                                               
*    GET MARKET NAME                                                            
         XC    SAVEKEY2,SAVEKEY2                                                
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,ZEROES                                                       
         LA    R5,KEY                                                           
         USING MKTRECD,R5                                                       
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,FULL                                                     
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
         CLC   KEY(L'MKTKEY),KEYSAVE                                            
         BE    *+16                                                             
         XC    MKTNAME,MKTNAME                                                  
         MVC   MKTNAME(24),=C'? MARKET REC NOT FOUND ?'                         
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTNAME,MKTNAME                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY2),SAVEKEY2                                         
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'STAKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         MVC   LISTAT,STAKCALL                                                  
         MVC   LISMRKT,SMKT                                                     
         MVC   LISTNTI,SNTISTA                                                  
         MVC   LISTMTYP,STYPE                                                   
         MVC   LISTPTYP,SPTYPE                                                  
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR220                                                            
         MVC   P,SPACES                                                         
         MVC   PSTAT,STAKCALL                                                   
         MVC   PMRKT,SMKT                                                       
         MVC   PTNTI,SNTISTA                                                    
         MVC   PTMTYP,STYPE                                                     
         MVC   PTPTYP,SPTYPE                                                    
         MVC   PTNAME,LISTNAME                                                  
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR20                                                             
*                                                                               
LR220    GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
***************************************************************                 
*============= FINDS DUPLICATE MARKETS FOR ONE STATION========*                 
DUPMKT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),SVQMED                                                  
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         GOTO1 HIGH                                                             
DUPMK10  CLC   SVQMED,STAKMED                                                   
         BNE   DUPMKX                                                           
         CLC   AGENCY,STAKAGY                                                   
         BE    DUPMK20                                                          
DUPSEQ   GOTO1 SEQ                                                              
         B     DUPMK10                                                          
DUPMK20  CLC   SMKT,SVQMKT                                                      
         BNE   DUPSEQ                                                           
         B     DUPERR                                                           
DUPMKX   MVC   AIO,AIO1                                                         
         XIT1                                                                   
***************************************************************                 
*============= CHECK IF MARKET RECORD EXISTS =================*                 
CHKMKT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY,ZEROES                                                       
         LA    R4,KEY                                                           
         USING MKTRECD,R4                                                       
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,SVQMKT                                                   
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 READ                                                             
         CLI   DMCB+8,0                                                         
         BE    CHKMKT50                                                         
         B     INVERR                                                           
*                                                                               
CHKMKT50 MVC   MSTMKTN,MKTNAME                                                  
         OI    MSTMKTNH+6,X'80'                                                 
CHKMKTX  MVC   AIO,AIO1                                                         
         XIT1                                                                   
         DROP  R4                                                               
***************************************************************                 
*============= CLEAR THE UNPROTECTED FIELDS ON THE SCREEN=====*                 
CLRSCRN  NTR1                                                                   
         LA    R2,MSTMKTH                                                       
CLR10    LA    R0,MSTLAST                                                       
         CR    R2,R0                                                            
         BNL   CLRSCRNX                                                         
         TM    1(R2),X'20'     CLEAR ONLY UNPROTECTED FIELDS                    
         BO    CLR20                                                            
         ZIC   R4,0(R2)                                                         
         LA    R0,8                                                             
         SR    R4,R0                                                            
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
CLR20    ZIC   R4,0(R2)                                                         
         AR    R2,R4                                                            
         B     CLR10                                                            
CLRSCRNX XIT1                                                                   
************************************************************                    
GETREP   NTR1                                                                   
         XC    REPNAME,REPNAME                                                  
         CLC   REPFLD,=C'000'                                                   
         BE    GETREPX                                                          
         XC    KEY,KEY                                                          
         MVC   KEY,ZEROES                                                       
         LA    R4,KEY                                                           
         USING REPRECD,R4                                                       
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,QMED                                                     
         MVC   REPKREP,REPFLD                                                   
         MVC   REPKAGY,AGENCY                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+8                                                              
         B     GETREPX                                                          
*                                                                               
         CLC   KEY(L'REPKEY),KEYSAVE                                            
         BE    *+8                                                              
         B     INVERR                                                           
*                                                                               
         MVC   REPNAME,RNAME                                                    
         DROP  R4                                                               
*                                                                               
GETREPX  MVC   AIO,AIO1                                                         
         XIT1                                                                   
************************************************************                    
VALPST   NTR1                                                                   
         LA    R4,BLOCK                                                         
         USING PSTBLKD,R4                                                       
         XC    0(200,R4),0(R4)     CLEAR INTERFACE BLOCK                        
         XC    200(200,R4),200(R4)                                              
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,MSTPSTH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   ERRDISP,PSTERDSP                                                 
         CLI   PSTERR,0                                                         
         BNE   INVERR                                                           
         MVC   SPST,PSTOUT         OUTPUT                                       
         BAS   RE,DISPPST          DISPLAY PST                                  
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
DISPPST  NTR1                                                                   
         OC    SPST,SPST           IS THERE ANYTHING TO DISPLAY                 
         BZ    DPX                                                              
*                                                                               
         LA    R4,BLOCK                                                         
         USING PSTBLKD,R4                                                       
         XC    0(200,R4),0(R4)     CLEAR INTERFACE BLOCK                        
         XC    200(200,R4),200(R4)                                              
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,SPST                                                          
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   MSTPST,PSTOUT       OUTPUT                                       
         OI    MSTPSTH+6,X'80'                                                  
DPX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DEL ALLOWED                               
         OI    GENSTAT2,DISTHSPG                                                
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,63,REQUESTOR                                                  
         SSPEC H2,63,REPORT                                                     
         SSPEC H2,77,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,28,C'NETWORK (MASTER STATION) LIST'                           
         SSPEC H2,28,C'-----------------------------'                           
         SPACE 1                                                                
         SSPEC H4,2,C'STATION'                                                  
         SSPEC H5,2,C'-------'                                                  
         SSPEC H4,17,C'MKTCODE'                                                 
         SSPEC H5,17,C'-------'                                                 
         SSPEC H4,39,C'MARKET NAME'                                             
         SSPEC H5,33,C'-----------------------'                                 
         SSPEC H4,60,C'NTI STATION'                                             
         SSPEC H5,60,C'-----------'                                             
         SSPEC H4,72,C'MEDIA TYPE'                                              
         SSPEC H5,72,C'----------'                                              
         SSPEC H4,88,C'POST TYPE'                                               
         SSPEC H5,88,C'---------'                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
                                                                                
SAVEDEF  DS    0H                  SAVE DEFINITION BEFORE SETDEF                
         MVC   MYSYSDIR,SYSDIR                                                  
         MVC   MYSYSFIL,SYSFIL                                                  
         MVC   MYUSEIO,USEIO                                                    
         MVC   MYACELOP,ACTELOPT                                                
         MVC   MYLKEY,LKEY                                                      
         BR    RE                                                               
*                                                                               
SETDEF   MVC   SYSDIR,=C'STATION '      SET TO READ STATION FILE                
         MVC   SYSFIL,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVI   ACTELOPT,C'N'            NO ACTIVITY ELEMENTS                    
         MVC   LKEY,=H'15'              SET LENGTH OF STATION KEY               
         BR    RE                                                               
*                                                                               
RSTRDEF  DS    0H                  RESTORE DEFINITION AFTER SETDEF              
         MVC   SYSDIR,MYSYSDIR                                                  
         MVC   SYSFIL,MYSYSFIL                                                  
         MVC   USEIO,MYUSEIO                                                    
         MVC   ACTELOPT,MYACELOP                                                
         MVC   LKEY,MYLKEY                                                      
         BR    RE                                                               
*                                                                               
MKTERR   MVI   ERROR,BADMKT                                                     
         B     TRAPERR                                                          
*                                                                               
RECNFERR MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
CHGERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'CANNOT CHANGE MARKET ON MEDIA N'                  
         B     MSGERR                                                           
*                                                                               
DUPERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'ONLY ONE STATION IN ONE MARKET'                   
         B     MSGERR                                                           
*                                                                               
NOTAUTH  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(21),=C'ACTION NOT AUTHORIZED'                            
         B     MSGERR                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
ADDERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  OC    ERRDISP,ERRDISP     DO I NEED TO OVERRIDE CURSOR POS.            
         BZ    TRAPEND                                                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         MVC   TIOBCURI,ERRDISP+1  DISPLACMENT INTO FIELD                       
*                                                                               
TRAPEND  DS    0H                                                               
         MVI   ERROPT,0            NEVER TO RETURN                              
         GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
BADMKT   EQU   11                                                               
STNKLNQ  EQU   20                  PASSIVE RECORD LENGTH                        
STXKLNQ  EQU   20                  PASSIVE RECORD LENGTH                        
*                                                                               
         PRINT GEN                                                              
                                                                                
         DS    0F                                                               
ZEROES   DC    20C'0'                                                           
ERRDISP  DS    H                                                                
MYREP    DS    CL3                                                              
MYSYSNET DS    CL16                                                             
SVNAME   DS    CL20                                                             
RELO     DS    A                                                                
         LTORG                                                                  
                                                                                
         DROP  R7,RB                                                            
         EJECT                                                                  
*                                                                               
MSG11    DC    C'** ERROR ** XXXX IS NOT A VALID CABLE NETWORK'                 
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*        RETURN NETWORK IN WORK+20                                              
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC9D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMCAD                                                       
         EJECT                                                                  
         ORG   MSTWORK                                                          
*                                                                               
                                                                                
DDDSLIST DS    0C                                                               
         DSDDL PRINT=YES                                                        
*                                                                               
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
CBLRECD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSLST                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
* CTGENFILE (NEED CTDMREC)                                                      
       ++INCLUDE CTGENFILE                                                      
                                                                                
* SPDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
         SPACE 1                                                                
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL3                                                              
LISTAT   DS    CL4                                                              
         DS    CL5                                                              
LISMRKT  DS    CL4                                                              
         DS    CL5                                                              
LISTNAME DS    CL(L'MKTNAME)                                                    
         DS    CL5                                                              
LISTNTI  DS    CL4                                                              
         DS    CL5                                                              
LISTMTYP DS    CL1                                                              
         DS    CL6                                                              
LISTPTYP DS    CL1                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PSTAT    DS    CL4                                                              
         DS    CL10                                                             
PMRKT    DS    CL4                                                              
         DS    CL10                                                             
PTNAME   DS    CL(L'MKTNAME)                                                    
         DS    CL9                                                              
PTNTI    DS    CL4                                                              
         DS    CL8                                                              
PTMTYP   DS    CL1                                                              
         DS    CL13                                                             
PTPTYP   DS    CL1                                                              
***********************************************************************         
*===================== NESFM06 (T31C06) SAVE AREA ====================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
*                                                                               
CANSTA   DS    H                   CANADIAN STATION NUMBER                      
SAVEKEY  DS    CL(L'STAKEY)                                                     
SAVEKEY2 DS    CL(L'STAKEY)                                                     
*                                                                               
REPFLD   DS    CL3                                                              
REPNAME  DS    CL22                                                             
SVQMED   DS    CL1                                                              
SVQMKT   DS    CL4                                                              
MYMKT    DS    CL4                                                              
MYFORM   DS    CL4                                                              
QFORM    DS    CL4                                                              
PSTOUT   DS    CL64                NEED A 64 BYTE FIELD FOR OUTPUT              
SAVEADDR DS    A                                                                
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063NESFM06S  05/01/02'                                      
         END                                                                    
