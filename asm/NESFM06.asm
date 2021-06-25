*          DATA SET NESFM06    AT LEVEL 137 AS OF 01/28/20                      
*PHASE T31C06A                                                                  
T31C06   TITLE 'NESFM06 - STATION MASTER'                                       
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 135 08MAY18 <SPEC-16616> Ability to filter on Pay Rep          *         
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
         GOTO1 VTERMACC            CHECK FOR DISP/LIST ONLY TERMINALS           
         BAS   RE,SETSCRN          SET SCREEN FOR COMSCORE                      
*                                                                               
*        CLI   1(RA),C'*'          DDS TERMINAL?                                
*        BE    MAIN14                                                           
*                                                                               
*        CLI   ACTNUM,ACTADD                                                    
*        BZ    *+8                                                              
*        NI    MSTTYPEH+1,X'FF'-X'20'    UNPROTECT IT                           
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    MAIN10                                                           
         OI    MSTTTYPH+6,X'80'                                                 
         MVC   MSTTTYP(12),=C'Traffic Type'                                     
         NI    MSTTRAH+1,X'FF'-X'20'     UNPROTECT IT                           
         CLI   1(RA),C'*'                DDS TERMINAL?                          
         BE    MAIN10                                                           
         OI    MSTTRAH+1,X'20'           PROTECT IT                             
*                                                                               
MAIN10   TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    MAIN15                                                           
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    NOTAUTH                                                          
         CLI   ACTNUM,ACTCHA       ACTION CHANGE?                               
         BE    NOTAUTH                                                          
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         BNE   MAIN15                                                           
         B     NOTAUTH                                                          
*                                                                               
******** MAIN14   NI    MSTTYPEH+1,X'FF'-X'20'    UNPROTECT IT                  
MAIN15   BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,XRECADD        WAS A RECORD JUST ADDED?                     
         BE    CHKNTI                                                           
         CLI   MODE,XRECPUT        WAS A RECORD JUST CHANGED?                   
         BE    CHKNTI                                                           
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
* CHECK NTI STATIONS                                                            
********************************************************************            
CHKNTI   DS    0H                                                               
         CLI   QMED,C'N'           NETWORK                                      
         BNE   CHKNTIX                                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    CHKN100             ADD NEW NTI RECORD                           
*                                                                               
*        CLC   SVNTIOLD,SVNTINEW   DID USER CHANGE NTI STATIONS?                
*        BE    CHKNTIX             NO - EXIT                                    
*                                                                               
CHKN100  DS    0H                                                               
         L     R5,AIO3                                                          
         USING SLSRECD,R5                                                       
*                                                                               
         XC    0(50,R5),0(R5)      CLEAR UP SOME IO AREA TO BUILD REC           
*                                                                               
         MVC   0(2,R5),=X'0D75'                                                 
         MVC   SLSKAGMD,BAGYMD     AGENCY/MEDIA                                 
         MVC   SLSKSTA,SAVEKEY+2   STATION CALL LETTERS                         
*!!!     MVC   SLSKCLT,SAVEKEY+7   CLIENT                                       
*                                                                               
         XC    SLSKCLT,SLSKCLT                                                  
         CLC   SAVEKEY+9(3),=C'000'                                             
         BE    CHKN110                                                          
         GOTO1 CLPACK,DMCB,SAVEKEY+9,SLSKCLT                                    
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
CHKN110  MVC   SLSLEN,=X'002B'     REC LENGTH                                   
*                                                                               
         MVI   SLSEL01,X'01'       ELEM CODE                                    
         MVI   SLSEL01+1,SLSEL01Q  ELEM LENGTH                                  
         MVC   SLSNTI,SVNTINEW     NTI STATION                                  
         MVI   SLSEL02,X'02'       ELEM CODE                                    
         MVI   SLSEL02+1,SLSEL02Q  ELEM LENGTH                                  
         MVC   SLSCSNN,SVCSSTA     COMSCORE NETWORK NUMBER                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R5)                                                    
*                                                                               
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMRDHI'),=C'SPTDIR  ',KEY,KEY,0           
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     DOES THIS NTI REC EXIST?                     
         BE    CHKN150                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R5)                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'SPTFILE ',KEY+14,AIO3,DMWORK          
         TM    DMCB+8,X'FD'                                                     
         BZ    CHKNTIX                                                          
         DC    H'0'                                                             
*                                                                               
CHKN150  DS    0H                  RECORD EXISTS - JUST CHANGE                  
         L     R5,AIO3                                                          
         MVC   AIO,AIO3                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'SPTFILE ',KEY+14,    X        
               AIO3,DMWORK                                                      
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SLSLEN,=X'002B'     REC LENGTH                                   
         MVI   SLSEL01,X'01'       ELEM CODE                                    
         MVI   SLSEL01+1,X'07'     ELEM LENGTH                                  
         MVC   SLSNTI,SVNTINEW     NTI STATION                                  
         MVI   SLSEL02,X'02'       ELEM CODE                                    
         MVI   SLSEL02+1,SLSEL02Q  ELEM LENGTH                                  
         MVC   SLSCSNN,SVCSSTA     COMSCORE NETWORK NUMBER                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFILE ',KEY+14,AIO3,DMWORK          
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKNTIX  DS    0H                                                               
         B     XIT                                                              
         DROP  R5                                                               
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
         CLI   5(R2),3             L'INPUT CAN'T BE < 3                         
         BL    INVERR                                                           
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
         MVC   MSTTAL,STALTYP                                                   
         OI    MSTTALH+6,X'80'                                                  
         MVC   MSTTRA,STRTYPE                                                   
         OI    MSTTRAH+6,X'80'                                                  
*                                                                               
         MVC   MSTGST,SGSTCODE                                                  
         OI    MSTGSTH+6,X'80'                                                  
*   PST CODE                                                                    
         BAS   RE,DISPPST                                                       
*                                                                               
         MVI   MSTLOCK,C'N'                                                     
         TM    SFLAG1,SLOCK                                                     
         BZ    *+8                                                              
         MVI   MSTLOCK,C'Y'                                                     
         OI    MSTLOCKH+6,X'80'                                                 
*                                                                               
         MVC   MSTNTIS,SNTISTA                                                  
         OI    MSTNTISH+6,X'80'                                                 
*                                                                               
         MVC   MSTTALN,SLSTCNET                                                 
         OI    MSTTALNH+6,X'80'                                                 
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
         MVC   MSTBOOK,SOVBKTYP                                                 
         OI    MSTBOOKH+6,X'80'                                                 
*                                                                               
******   MVC   MSTCHAN,SCHNL                                                    
******   OI    MSTCHANH+6,X'80'                                                 
*                                                                               
         XC    MSTFEE,MSTFEE                                                    
         OC    SSVCFEE,SSVCFEE                                                  
         BZ    DR180                                                            
         EDIT  SSVCFEE,(5,MSTFEE),2                                             
         OI    MSTFEEH+6,X'80'                                                  
*                                                                               
DR180    DS    0H                                                               
*&&DO                                                                           
         MVC   MSTTWIX,STWIX                                                    
         OI    MSTTWIXH+6,X'80'                                                 
*&&                                                                             
         MVC   MSTOWN,SOWNER                                                    
         OI    MSTOWNH+6,X'80'                                                  
*                                                                               
         MVC   MSTPAR,SPARENT                                                   
         OI    MSTPARH+6,X'80'                                                  
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
DR220    DS    0H                                                               
         LA    R5,MSTOPTS                                                       
         XC    MSTOPTS,MSTOPTS                                                  
         OI    MSTOPTSH+6,X'80'                                                 
*                                                                               
         TM    SFLAG1,SMIRRORS                                                  
         BZ    DR222                                                            
         MVC   0(8,R5),=C'SPMIR=Y,'                                             
         LA    R5,8(R5)                                                         
*                                                                               
DR222    TM    SFLAG1,SMIDAS                                                    
         BZ    DR238                                                            
         MVC   0(6,R5),=C'MIDAS,'                                               
         LA    R5,6(R5)                                                         
*                                                                               
DR238    S     R5,=F'1'                                                         
         MVI   0(R5),X'40'          REMOVE FINAL COMMA                          
*                                                                               
DR240    BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF                                                        
*                                                                               
         XC    MSTCSN,MSTCSN                                                    
         OI    MSTCSNH+6,X'80'                                                  
         CLI   STAKLEN+1,STACRLNQ                                               
         JL    DR250                                                            
         MVC   MSTCSN,SCSSTA                                                    
         MVC   SVCSSTA,SCSSTA                                                   
*                                                                               
DR250    MVI   MSTDIGI,C'N'                                                     
         OI    MSTDIGIH+6,X'80'                                                 
         CLI   STAKLEN+1,STACRLNQ                                               
         JNH   DR260                                                            
         TM    SFLAG2,S2DIGIQ                                                   
         JZ    DR260                                                            
         MVI   MSTDIGI,C'Y'                                                     
*                                                                               
DR260    DS    0H                                                               
         CLI   STNETINV,C' '                                                    
         BNH   DRX                                                              
         MVC   MSTNET,STNETINV                                                  
         OI    MSTNETH+6,X'80'                                                  
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
VR10     CLC   8(4,R2),=C'7777'                                                 
         BE    INVERR                                                           
         CLC   8(4,R2),=C'0774'    SYNDICATION                                  
         BE    INVERR                                                           
         CLC   8(4,R2),=C'0775'    CABLE                                        
         BE    INVERR                                                           
         CLC   8(4,R2),=C'0777'    NETWORK                                      
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
         CLI   ACTNUM,ACTADD                                                    
         JNE   VR20                                                             
         XC    STAKLEN,STAKLEN                                                  
         MVC   STAKLEN,=AL2(SCBLSQNQ)                                           
         J     VR30                                                             
*                                                                               
VR20     CLC   STAKLEN,=AL2(SCBLSQNQ)   ALREADY NEW SIZE?                       
         JE    VR30                                                             
         SR    RF,RF                                                            
         ICM   RF,3,STAKLEN        GET CURRENT LENGTH                           
         L     R6,AIO1                                                          
         AR    R6,RF               END OF RECORD                                
         XCEF  (R6),1000                                                        
*                                                                               
         L     R6,AIO1                                                          
         MVC   STAKLEN,=AL2(SCBLSQNQ)   SET NEW LENGTH                          
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
****     LA    R2,MSTCHANH                                                      
****     CLI   5(R2),0                                                          
****     BE    VR70                                                             
****     CLI   5(R2),4                                                          
****     BNE   INVERR                                                           
*                                                                               
****     MVC   SCHNL(4),MSTCHAN                                                 
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
*   STATION TYPE (ADD ONLY, UNLESS DDS TERMINAL)                                
VR75     CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    VR76                                                             
         CLI   ACTNUM,ACTADD       ACTION CHANGE?                               
         BNE   VR140                                                            
         TM    4(R2),X'80'         USER CHANGED THIS?                           
         BO    NOTAUTH                                                          
*                                                                               
VR76     LA    R2,MSTTYPEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
VR77     MVI   STYPE,C' '                                                       
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
         CLI   9(R2),C'T'                                                       
         BE    VR100                                                            
         CLI   9(R2),C'V'                                                       
         BE    VR100                                                            
         CLI   9(R2),C'I'                                                       
         BE    VR100                                                            
         CLI   9(R2),C'B'                                                       
         BE    VR100                                                            
         CLI   9(R2),C'U'                                                       
         BE    VR100                                                            
         CLI   9(R2),C'G'                                                       
         BE    VR100                                                            
         CLI   9(R2),C'J'                                                       
         BE    VR100                                                            
         CLI   9(R2),C'K'                                                       
         BE    VR100                                                            
         CLI   9(R2),C'M'                                                       
         BE    VR100                                                            
         CLI   9(R2),X'40'                                                      
         BNH   VR120                                                            
         B     INVERR                                                           
*                                                                               
VR100    MVC   SUBMEDIA,9(R2)                                                   
*                                                                               
VR120    CLI   STRTYPE,0                                                        
         BNE   *+10                                                             
         MVC   STRTYPE,SPTYPE      DEFAULT TRAFFIC TYPE IS POST                 
*                                                                               
         CLI   SPTYPE,X'40'                                                     
         BNH   *+14                                                             
         CLC   SPTYPE,10(R2)                                                    
         BE    VR140                                                            
*                                                                               
*        MVC   SPTYPE,STYPE                                                     
*                                                                               
         CLI   5(R2),3                                                          
         BL    INVERR                                                           
         LA    R5,PTYPETAB                                                      
VR130    CLC   10(1,R2),0(R5)                                                   
         BE    VR135                                                            
         CLI   0(R5),X'FF'                                                      
         BE    INVERR                                                           
         LA    R5,1(R5)                                                         
         B     VR130                                                            
*                                                                               
VR135    MVC   SPTYPE,10(R2)                                                    
         MVC   STRTYPE,10(R2)      DEFAULT TRAFFIC TYPE IS POST                 
         B     VR140                                                            
*                                                                               
*                                  MEDIA TYPE TABLE FOR NETPAK                  
MTYPETAB DC    C'N'                NETWORK                                      
         DC    C'C'                CABLE                                        
         DC    C'S'                SYNDICATION                                  
         DC    C'D'                RADIO                                        
         DC    C'V'                VIDEO (DIGITAL)                              
*******  DC    C'H'                HISPANIC                                     
         DC    C'O'                OTHER                                        
         DC    X'FFFF'                                                          
*                                  POSTING TYPE TABLE FOR NETPAK                
PTYPETAB DC    C'N'                NETWORK                                      
         DC    C'C'                CABLE                                        
         DC    C'S'                SYNDICATION                                  
         DC    C'H'                HISPANIC                                     
         DC    C'O'                OTHER                                        
         DC    X'FFFF'                                                          
*                                  TRAFFIC TYPE TABLE FOR NETPAK                
TTYPETAB DC    C'N'                NETWORK                                      
         DC    C'C'                CABLE                                        
         DC    C'S'                SYNDICATION                                  
         DC    C'H'                HISPANIC                                     
         DC    C'O'                OTHER                                        
         DC    C'V'                VIDEO                                        
         DC    X'FFFF'                                                          
*   TWIX NUMBER                                                                 
VR140    DS    0H                                                               
*&&DO                                                                           
         LA    R2,MSTTWIXH                                                      
         CLI   5(R2),0                                                          
         BE    VR160                                                            
         MVC   STWIX,8(R2)                                                      
*&&                                                                             
*   FAX NUMBER                                                                  
VR160    LA    R2,MSTFAXH                                                       
         XC    SFAX,SFAX                                                        
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   SFAX,8(R2)                                                       
*                                                                               
*   BOOK TYPE (OVERRIDE POSTING TYPE)                                           
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    VR170                                                            
         CLI   ACTNUM,ACTADD       ACTION CHANGE?                               
         BNE   VR180                                                            
         TM    4(R2),X'80'         USER CHANGED THIS?                           
         BO    NOTAUTH                                                          
*                                                                               
VR170    LA    R2,MSTBOOKH                                                      
         XC    SOVBKTYP,SPTYPE                                                  
         CLI   5(R2),0                                                          
         BE    VR180                                                            
         LA    R5,PTYPETAB                                                      
VR173    CLC   8(1,R2),0(R5)                                                    
         BE    VR175                                                            
         CLI   0(R5),X'FF'                                                      
         BE    INVERR                                                           
         LA    R5,1(R5)                                                         
         B     VR173                                                            
*                                                                               
VR175    MVC   SOVBKTYP,0(R5)                                                   
*                                                                               
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
*     STATION LOCK                                                              
VR320    DS    0H                                                               
         NI    SFLAG1,X'FF'-SLOCK                                               
         LA    R2,MSTLOCKH                                                      
         CLI   5(R2),0                                                          
         BE    VR330                                                            
         CLI   8(R2),C'N'                                                       
         BE    VR330                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
         OI    SFLAG1,SLOCK                                                     
*                                                                               
*     NTI STATION                                                               
VR330    DS    0H                                                               
         XC    SVNTIOLD,SVNTIOLD                                                
         XC    SVNTINEW,SVNTINEW                                                
         CLI   ACTNUM,ACTADD                                                    
         BE    *+10                                                             
         MVC   SVNTIOLD,SNTISTA    SAVE AWAY PREVIOUS NTI                       
*                                                                               
         LA    R2,MSTNTISH                                                      
         MVC   DUB,SPACES                                                       
         CLI   5(R2),0                                                          
         BE    VR350                                                            
         XC    DUB,DUB                                                          
         MVC   DUB(4),8(R2)                                                     
         OC    DUB(4),=4X'40'                                                   
VR350    MVC   SNTISTA,DUB                                                      
         MVC   SVNTINEW,SNTISTA    SAVE AWAY NEW NTI                            
*                                                                               
*     TALENT CNET STATION                                                       
         LA    R2,MSTTALNH                                                      
         MVC   SLSTCNET,SPACES                                                  
         CLI   5(R2),0             NOTHING ENTERED                              
         BE    VR355               SKIP                                         
*                                                                               
VR353    MVC   SLSTCNET(4),8(R2)                                                
         OC    SLSTCNET(4),=4X'40'                                              
*                                                                               
*     GOODS & SERVICE TAX CODE                                                  
VR355    LA    R2,MSTGSTH                                                       
         MVI   SGSTCODE,0                                                       
         CLI   5(R2),0                                                          
         BE    VR380                                                            
         LA    R5,GSTTAB                                                        
VR360    CLC   0(1,R5),8(R2)                                                    
         BE    VR370                                                            
         CLI   0(R5),X'FF'                                                      
         BE    INVERR                                                           
         LA    R5,1(R5)                                                         
         B     VR360                                                            
VR370    MVC   SGSTCODE,MSTGST        MOVE IN CORRECT GST CODE                  
         B     VR380                                                            
*                                                                               
GSTTAB   DC    C'S'               STANDARD                                      
         DC    C'U'                                                             
         DC    C'X'                                                             
         DC    C'Z'               ZERO                                          
         DC    X'FFFF'            END OF TABLE                                  
*                                                                               
*     OWNER STATION                                                             
VR380    DS    0H                                                               
         XC    SOWNER,SOWNER                                                    
         LA    R2,MSTOWNH                                                       
         CLI   5(R2),0                                                          
         BE    VR390                                                            
*                                                                               
         MVC   DUB(4),8(R2)                                                     
         OC    DUB,SPACES                                                       
         BAS   RE,VALOTAB                                                       
         MVC   SOWNER,8(R2)                                                     
         OC    SOWNER,SPACES                                                    
*                                                                               
*     PARENT STATION                                                            
VR390    DS    0H                                                               
         XC    SPARENT,SPARENT                                                  
         LA    R2,MSTPARH                                                       
         CLI   5(R2),0                                                          
         BE    VR400                                                            
*                                                                               
         MVC   DUB(4),8(R2)                                                     
         OC    DUB,SPACES                                                       
         BAS   RE,VALOTAB                                                       
         MVC   SPARENT,8(R2)                                                    
         OC    SPARENT,SPACES                                                   
*                                                                               
*   SKIP BOOK SINCE THIS IS NETPAK SO DO PST CODE                               
VR400    LA    R2,MSTPSTH                                                       
         XC    SPST,SPST                                                        
         CLI   5(R2),0                                                          
         BE    VR402                                                            
         BAS   RE,VALPST                                                        
*                                                                               
VR402    LA    R2,MSTTALH           TALENT MEDIA TYPE                           
         MVI   STALTYP,0                                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
         CLI   MSTTAL,C'N'          SAME AS MTYPETAB                            
         BE    VR405                                                            
         CLI   MSTTAL,C'C'                                                      
         BE    VR405                                                            
         CLI   MSTTAL,C'S'                                                      
         BE    VR405                                                            
         CLI   MSTTAL,C'D'                                                      
         BE    VR405                                                            
         CLI   MSTTAL,C'H'                                                      
         BE    VR405                                                            
         CLI   MSTTAL,C'O'                                                      
         BNE   INVERR                                                           
*                                                                               
VR405    MVC   STALTYP,8(R2)                                                    
*                                                                               
VR410    LA    R2,MSTTRAH           TRAFFIC TYPE                                
         CLI   5(R2),0                                                          
         BE    VR420                                                            
*                                                                               
         LA    R5,TTYPETAB                                                      
VR412    CLC   8(1,R2),0(R5)                                                    
         BE    VR415                                                            
         CLI   0(R5),X'FF'                                                      
         BE    INVERR                                                           
         LA    R5,1(R5)                                                         
         B     VR412                                                            
*                                                                               
VR415    MVC   STRTYPE,8(R2)                                                    
*                                                                               
VR420    DS    0H                                                               
         NI    SFLAG1,X'FF'-SMIRRORS                                            
         LA    R2,MSTOPTSH                                                      
         CLI   5(R2),0                                                          
         BE    VR470                                                            
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(12,AIO3)                                      
         CLI   DMCB+4,0                                                         
         BE    INVERR                                                           
         L     R5,AIO3                                                          
         ZIC   R3,DMCB+4            N'ENTRIES                                   
*                                                                               
VR430    DS    0H                  ANY MORE OPTIONS?                            
         CLI   0(R5),0                                                          
         BE    VR470                                                            
*                                                                               
*  CHECK FOR MIRROR FLAG                                                        
VR435    CLC   =C'SPMIR',12(R5)                                                 
         BNE   VR438                                                            
         CLI   22(R5),C'N'                                                      
         BE    VR470                                                            
         CLI   22(R5),C'Y'                                                      
         BNE   INVERR                                                           
         OI    SFLAG1,SMIRRORS                                                  
         B     VR460                GET NEXT ENTRY                              
*                                                                               
*  CHECK FOR MIDAS FLAG                                                         
VR438    CLC   =C'MIDAS',12(R5)                                                 
         BNE   INVERR                                                           
         OI    SFLAG1,SMIDAS                                                    
*                                                                               
VR460    LA    R5,32(R5)           GET TO NEXT ENTRY IN BLOCK                   
         BCT   R3,VR435                                                         
*                                                                               
VR470    XC    SCSSTA,SCSSTA                                                    
         LA    R2,MSTCSNH          COMSCORE STATION                             
         CLI   5(R2),0                                                          
         JE    VR480                                                            
         ZIC   R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(0,8(R2)),(R5)                                      
         CLI   DMCB,X'FF'                                                       
         BE    INVERR                                                           
*        TM    4(R2),X'08'         NUMERIC?                                     
*        JZ    INVERR                                                           
         MVC   SCSSTA,8(R2)                                                     
         MVC   SVCSSTA,SCSSTA                                                   
*                                                                               
VR480    NI    SFLAG2,X'FF'-S2DIGIQ                                             
         LA    R2,MSTDIGIH         DIGINET                                      
         CLI   5(R2),0                                                          
         JE    VR490                                                            
         CLI   8(R2),C'N'                                                       
         JE    VR490                                                            
         CLI   8(R2),C'Y'                                                       
         JNE   INVERR                                                           
         OI    SFLAG2,S2DIGIQ                                                   
*                                                                               
VR490    DS    0H                                                               
         LA    R2,MSTNETH          NET INVOICES                                 
         CLI   5(R2),0                                                          
         JE    VRX                                                              
         CLI   8(R2),C'N'                                                       
         JE    *+12                                                             
         CLI   8(R2),C'Y'                                                       
         JNE   INVERR                                                           
         MVC   STNETINV,8(R2)                                                   
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
LR       DS    0H                                                               
         MVI   BYTE,0                                                           
         LA    R2,MSLOPTNH                                                      
         GOTO1 SCANNER,DMCB,(R2),(12,AIO3)                                      
         CLI   DMCB+4,0                                                         
         BE    LR5                                                              
*                                                                               
         L     RF,AIO3                                                          
         CLI   0(RF),0                                                          
         BE    LR5                                                              
         CLC   =C'LOCK',12(RF)                                                  
         BNE   LR2                                                              
         MVI   BYTE,1                                                           
         CLI   22(RF),C'N'                                                      
         BE    LR5                                                              
         CLI   22(RF),C'Y'                                                      
         BNE   INVERR                                                           
         MVI   BYTE,2                                                           
         B     LR5                                                              
*                                                                               
LR2      CLC   =C'PAY',12(RF)                                                   
         BNE   INVERR                                                           
         MVI   BYTE,3                                                           
         MVC   SVPAY,22(RF)                                                     
         CLC   SVPAY,SPACES                                                     
         BNH   INVERR                                                           
*                                                                               
LR5      BAS   RE,SETDEF                                                        
         OC    KEY,KEY                                                          
         BNZ   LR10                                                             
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
         CLI   BYTE,3              PAY REP FILTER?                              
         BNE   LR40                                                             
         CLC   SPAYREP,SVPAY       PAY REP MATCHED?                             
         BNE   LR20                                                             
         B     LR50                                                             
*                                                                               
LR40     CLI   BYTE,1              LOCK=N FILTER?                               
         BNE   *+12                                                             
         TM    SFLAG1,SLOCK                                                     
         BO    LR20                                                             
         CLI   BYTE,2              LOCK=Y FILTER?                               
         BNE   *+12                                                             
         TM    SFLAG1,SLOCK                                                     
         BZ    LR20                                                             
*                                                                               
LR50     XC    FULL,FULL                                                        
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
         MVC   PTTTYP,STRTYPE      TRAFFIC TYPE                                 
         MVC   PTBTYP,SBKTYPE       BOOK TYPE                                   
         CLI   SOVBKTYP,X'40'       OVERRIDE BK TYPE                            
         BNH   *+10                                                             
         MVC   PTBTYP,SOVBKTYP                                                  
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
*        READTAL                                                                
***********************************************************************         
*&&DO                                                                           
READTAL  NTR1                                                                   
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   ASWITCH,CSWITCH                                                  
         DROP  RF                                                               
*                                                                               
         MVI   ERROR,0                                                          
         BAS   RE,SAVETAL                                                       
         BAS   RE,SWTAL            SWITCH TO TALENT                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING TLMTD,R1                                                         
         MVI   TLMTCD,TLMTCDQ                                                   
         MVI   TLMTTYPE,C'N'       NETWORK                                      
         MVC   TLMTCODE(4),8(R2)                                                
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'TALDIR  ',KEY,KEY,0                   
         CLC   KEY(TLMTINUM-TLMTD),KEYSAVE                                      
         BE    READTAL5                                                         
         MVI   ERROR,INVALID                                                    
*                                                                               
READTAL5 BAS   RE,RSTRTAL                                                       
         MVC   DMCB(1),SVSYS       SET TO SWITCH BACK TO ORIG. SYSTEM           
         MVC   LSTATUS,=H'1'                                                    
         BAS   RE,SWSYS                                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLI   ERROR,0                                                          
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
*              ROUTINES TO CONTROL SYSTEM SWITCHING                             
*                                                                               
SWTAL    DS    0H                                                               
         MVC   SYSDIR,=CL8'TALDIR  '                                            
         MVC   SYSFIL,=CL8'TALFIL  '                                            
         MVI   USEIO,C'N'                                                       
         MVC   LKEY,=H'32'                                                      
         MVI   DMCB,X'10'          SET TO SWITCH TO TAL1                        
         B     SWSYS                                                            
*                                                                               
SWBACK   DS    0H                                                               
         MVC   SYSDIR,SVSYSDIR     RESTORE CALLER'S FILES, ETC.                 
         MVC   SYSFIL,SVSYSFIL                                                  
         MVC   DMFILE,SVSYSDIR                                                  
         MVC   LKEY,SVLKEY                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DMCB(1),SVSYS       SET TO SWITCH BACK TO ORIG. SYSTEM           
         B     SWSYS                                                            
*                                                                               
SWSYS    NTR1                                                                   
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 ASWITCH,DMCB,,0                                                  
         CLI   4(R1),0                                               **         
         BE    *+6                                                              
         DC    H'0'                SWITCH ERROR                                 
SWX      B     XIT                                                              
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
         SSPEC H4,101,C'TRAFFIC TYPE'                                           
         SSPEC H5,101,C'------------'                                           
         SSPEC H4,114,C'BOOK TYPE'                                              
         SSPEC H5,114,C'---------'                                              
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
SAVEDEF  DS    0H                  SAVE DEFINITION BEFORE SETDEF                
         MVC   MYSYSDIR,SYSDIR                                                  
         MVC   MYSYSFIL,SYSFIL                                                  
         MVC   MYUSEIO,USEIO                                                    
         MVC   MYACELOP,ACTELOPT                                                
         MVC   MYLKEY,LKEY                                                      
         BR    RE                                                               
*                                                                               
SAVETAL  DS    0H                  SAVE DEFINITION BEFORE SWITCH TO TAL         
         MVC   TLSVSDIR,SYSDIR                                                  
         MVC   TLSVSFIL,SYSFIL                                                  
         MVC   TLSVUIO,USEIO                                                    
         MVC   TLSVALOP,ACTELOPT                                                
         MVC   TLSVLKEY,LKEY                                                    
         MVC   TLSVKEY,SAVEKEY                                                  
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
RSTRTAL  DS    0H                  RESTORE DEFINITION AFTER SETTAL              
         MVC   SYSDIR,TLSVSDIR                                                  
         MVC   SYSFIL,TLSVSFIL                                                  
         MVC   FILENAME,SYSFIL                                                  
         MVC   DMFILE,SYSFIL                                                    
         MVC   USEIO,TLSVUIO                                                    
         MVC   ACTELOPT,TLSVALOP                                                
         MVC   LKEY,TLSVLKEY                                                    
         XC    KEY,KEY                                                          
         XR    RF,RF                                                            
         IC    RF,LKEY                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),TLSVKEY                                                   
*                                                                               
         MVC   KEY,TLSVKEY                                                      
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
* VALIDATE STATION AGAINST OWNER TABLE (DUB HAS 4 CHAR STATION)                 
*                                                                               
VALOTAB  NTR1                                                                   
         LA    RF,OWNERTAB                                                      
*                                                                               
VALO10   DS    0H                                                               
         CLI   0(RF),X'FF'         NOT IN TABLE?                                
         BE    INVERR                                                           
*                                                                               
         CLC   DUB(4),0(RF)                                                     
         BE    VALOX                                                            
         LA    RF,23(RF)           L'STATION + L'DESCRIPTION (4+20)             
         B     VALO10                                                           
*                                                                               
VALOX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
*********************************************************************           
*        SET SCREEN FOR COMSCORE                                                
*********************************************************************           
SETSCRN  NTR1                                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JE    XIT                                                              
*                                                                               
         XC    MSTCSNN,MSTCSNN                                                  
         MVI   MSTCSNNH+5,0                                                     
*                                                                               
         TM    USRIDFLG,USRRNTKQ   USER HAS ACCESS TO COMSCORE?                 
         JZ    SSCRNX                                                           
         MVC   MSTCSNN,=C'comScore Station'                                     
         NI    MSTCSNH+1,X'FF'-X'20' TURN OFF PROTECT BIT                       
*                                                                               
SSCRNX   OI    MSTCSNNH+6,X'80'                                                 
         OI    MSTCSNH+6,X'80'                                                  
         J     XIT                                                              
         LTORG                                                                  
       ++INCLUDE NEGENOTAB                                                      
*                                                                               
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
SVPAY    DS    CL3                                                              
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
* TAGENFILE                                                                     
       ++INCLUDE TAGENFILE                                                      
         EJECT                                                                  
* COMFACSD                                                                      
       ++INCLUDE DDCOMFACS                                                      
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
         DS    CL13                                                             
PTTTYP   DS    CL1                                                              
         DS    CL13                                                             
PTBTYP   DS    CL1                                                              
***********************************************************************         
*===================== NESFM06 (T31C06) SAVE AREA ====================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE+220                                                     
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
*                                                                               
CANSTA   DS    H                   CANADIAN STATION NUMBER                      
SAVEKEY  DS    CL(L'STAKEY)                                                     
SAVEKEY2 DS    CL(L'STAKEY)                                                     
SVAGYORG DS    CL8                                                              
*                                                                               
REPFLD   DS    CL3                                                              
REPNAME  DS    CL22                                                             
*                                                                               
SVNTIOLD DS    CL4                 SAVED NTI STATION (ORIGINAL)                 
SVNTINEW DS    CL5                 SAVED NTI STATION (NEW)                      
SVCSSTA  DS    CL10                COMSCORE NETWORK NUMBER                      
NTIFLAG  DS    XL1                                                              
NTIDEL   EQU   X'01'               REMOVED NTI FROM SCREEN                      
NTIADD   EQU   X'02'               ADDED NEW MASTER REC                         
NTICHG   EQU   X'04'               CHANGED NTI REC ON SCREEN                    
*                                                                               
SVQMED   DS    CL1                                                              
SVQMKT   DS    CL4                                                              
MYMKT    DS    CL4                                                              
MYFORM   DS    CL4                                                              
QFORM    DS    CL4                                                              
PSTOUT   DS    CL64                NEED A 64 BYTE FIELD FOR OUTPUT              
SAVEADDR DS    A                                                                
ASWITCH  DS    V                                                                
TLSVKEY  DS    CL(L'KEY)                                                        
TLSVSDIR DS    CL(L'SYSDIR)                                                     
TLSVSFIL DS    CL(L'SYSFIL)                                                     
TLSVUIO  DS    CL(L'USEIO)                                                      
TLSVALOP DS    CL(L'ACTELOPT)                                                   
TLSVLKEY DS    CL(L'LKEY)                                                       
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'137NESFM06   01/28/20'                                      
         END                                                                    
