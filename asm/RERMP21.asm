*          DATA SET RERMP21    AT LEVEL 086 AS OF 02/11/10                      
*PHASE T81021C                                                                  
         TITLE 'RERMP21 - EDIT FOR O/N TRANSFER - INIT'                         
***********************************************************************         
*                                                                     *         
*        RERMP21 - EDIT FOR O/N TRANSFER - INITIALIZATION             *         
*                                                                     *         
* 28FEB02 (BU ) --- SET TWAWHEN = 5 FOR UPDATIVE SOON                 *         
*                                                                     *         
* 13APR09 (KUI) --- NEW INVENTORY KEY SUPPORT                         *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
T81021   CSECT                                                                  
         NMOD1 0,T81021**,RR=RE                                                 
*                                                                               
         L     RC,0(R1)            ESTABLISH GENCON WORKAREA                    
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             ESTABLISH SCREEN                             
         USING CONHEADH-64,RA                                                   
*                                                                               
         L     R9,ASYSD            ESTABLISH SYSTEM WORKAREA                    
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD          ESTABLISH SPOOL WORKAREA                     
         USING SPOOLD,R8                                                        
*                                                                               
         ST    RE,RELO21           SAVE RE-LOCATION FACTOR                      
*                                                                               
         TITLE 'RERMP21 - EDIT FOR O/N TRANSFER - INITMODE'                     
***********************************************************************         
*                                                                     *         
*        RERMP21 - EDIT FOR O/N TRANSFER - DETERMINE MODE             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INITMODE DS    0H                                                               
*                                                                               
*        PROCESS MODES HANDLED BY THIS OVERLAY                                  
*                                                                               
         CLI   MODE,VALREC         VALREC                                       
         BE    VREC                                                             
*                                                                               
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         TITLE 'RERMP21 - EDIT FOR O/N TRANSFER - VREC'                         
***********************************************************************         
*                                                                     *         
*        RERMP21 - EDIT FOR O/N TRANSFER - EDIT INPUT                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREC     DS    0H                                                               
*                                                                               
*        VALIDATE RATINGS SERVICE                                               
*                                                                               
         GOTO1 VALAGY              VALIDATE REP                                 
*                                                                               
         LA    R2,TITSRCEH         SOURCE FIELD                                 
         GOTO1 VALISVC             VALIDATE RATINGS SERVICE                     
*                                                                               
*        VALIDATE RATING BOOK                                                   
*                                                                               
         MVI   MAX,1               MAXIMUM 1 BOOK                               
         LA    R2,TITBOOKH         BOOK FIELD                                   
         GOTO1 VALIBOK             VALIDATE RATINGS BOOK                        
*                                                                               
         TM    CBOOKS,X'A6'        DISALLOW SUPPRESS CPM OPTION AND             
         BZ    VRBKX                        EST, PROJ AND SPECIAL BOOKS         
*                                                                               
VRBKER   DS    0H                                                               
*                                                                               
         MVC   RERROR,=AL2(INVBOK) INVALID DAYPART                              
         GOTO1 MYERROR                                                          
*                                                                               
VRBKX    DS    0H                                                               
*                                                                               
*        VALIDATE STATION                                                       
*                                                                               
         LA    R2,TITSTATH         STATION FIELD                                
*                                                                               
         MVC   TITMKT,SPACES       CLEAR OLD MARKET NAME                        
         OI    TITMKTH+6,X'80'     FORCE FIELD TRANSMISSION                     
*                                                                               
         GOTO1 =A(VALSTA),RR=RELO21 VALIDATE STATION FIELD                      
*                                                                               
         B     VSTAX                                                            
*                                                                               
         GOTO1 VALISTA             VALIDATE STATION                             
*                                                                               
         MVC   CSTAT,WORK                                                       
         MVI   CSTAT+4,C'T'                                                     
         CLI   WORK+4,C' '                                                      
         BE    *+10                                                             
         MVC   CSTAT+4(1),WORK+4                                                
         CLI   WORK+40,C' '                                                     
         BE    *+10                                                             
         MVC   CSTAT+4(1),WORK+40 CHECK SATTELITE                               
*                                                                               
         MVC   TITMKT,CMKTNAM      SHOW MARKET NAME                             
*                                                                               
*        VALIDATE STATION/MARKET COMBINATION                                    
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK                            
*                                                                               
         MVC   DBSELAGY,CPARREP                                                 
         MVC   DBFILE,=C'TPT'                                                   
*                                                                               
         MVC   DBAREC,AIO2             A(DEMO WORK AREA)                        
*                                                                               
         MVI   DBFUNCT,DBVLSTBK    VALIDATE STATION BOOK                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELSRC,TITSRCE    SET SOURCE FIELD                             
         MVC   DBSELBK,CBOOKS+1    SET BOOK                                     
         MVC   DBSELSTA,CSTAT                                                   
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         MVC   DBBTYPE,CBOOKS+3    SET BOOK TYPE                                
*                                                                               
         CLI   DBSELSTA+4,C'T'     CHANGE CALL LETTER SUFFIX                    
         BE    *+8                                                              
         CLI   DBSELSTA+4,C'2'     FOR A PS/1 STATION                           
         BE    *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCK,0,0                                           
         CLI   DBERROR,0           EXIT ON ERROR                                
         BE    VRBKSTAX                                                         
*                                                                               
         LA    R2,TITBOOKH         PUT CURSOR AT BOOK                           
         MVC   RERROR,=AL2(INVBKSTA) INVALID BOOK FOR STATION                   
         GOTO1 MYERROR                                                          
*                                                                               
VRBKSTAX DS    0H                                                               
*                                                                               
VSTAX    DS    0H                                                               
*                                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *          DAYPART(S)            *           
*                                  *                                *           
*                                  **********************************           
DPTVAL   LA    R2,TITDPTH          VALIDATE DAYPART                             
         GOTO1 ANY                                                              
*                                                                               
         XC    DPLIST,DPLIST       INIT DAYPART LIST                            
         XC    DPMENU,DPMENU       INIT DAYPART MENU CODE                       
*                                                                               
         CLI   5(R2),0             IF NOT ENTERED                               
         BE    *+10                                                             
         CLC   8(3,R2),=C'ALL'     OR ALL                                       
         BNE   *+14                                                             
         MVC   DPMENU,=C'ALL '        USE MENU 'ALL '                           
         B     DPTMENU                                                          
*                                                                               
         CLC   =C'M=',8(R2)        MENU IF IT STARTS 'M='                       
         BNE   DPT05                                                            
*                                                                               
         MVC   DPMENU,10(R2)       SAVE MENU CODE                               
         OC    DPMENU,SPACES       SPACE FILL                                   
*                                                                               
         B     DPTMENU                                                          
*                                                                               
DPT05    DS    0H                                                               
*                                                                               
         ZIC   RF,5(R2)            NUMBER OF DAYPARTS                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),8(R2)     SAVE DAYPART LIST                            
*                                                                               
         B     DPTMENUX                                                         
*                                                                               
*        READ SET RECORD FOR DAYPART MENU                                       
*                                                                               
DPTMENU  DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH SET RECORD KEY                     
         USING RSETKEY,R4                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   SET AS SET RECORD RECORD                     
         MVC   RSETKREP,CPARREP    SET REP CODE                                 
         MVC   RSETKSET,=C'DP'     SET SET CODE                                 
         MVC   RSETKID,DPMENU      SET SET IDENTIFIER                           
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTMENUE                                                         
*                                                                               
         GOTO1 GETREC              READ IN SET RECORD                           
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,RSETMCDQ     FIND MEMBERS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DPTMENUE            MUST FIND ELEMENT                            
*                                                                               
         USING RSETMEMD,R6         ESTABLISH MEMBERS ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETMTOV)     DECREMENT BY OVERHEAD LENGTH                 
         BNP   DPTMENUE            MUST HAVE SOME MEMBERS                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),RSETMEMB  COPY DAYPARTS                                
*                                                                               
DPTMENUX DS    0H                                                               
*                                                                               
*        VALIDATE INDIVIDUALLY ENTERED DAYPARTS                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH DAYPART RECORD KEY                 
         USING RRDPKEY,R4                                                       
*                                                                               
         MVI   RRDPKTYP,RRDPKIDQ   SET AS RESEARCH DAYPART RECORD               
         MVC   RRDPKREP,CPARREP    SET REP CODE                                 
*                                                                               
         LA    R5,DPLIST           START OF INPUT                               
*                                                                               
         LA    R0,L'DPLIST         MAX NUMBER OF DAYPARTS IN LIST               
*                                                                               
         LA    R3,DPTBL            ESTABLISH DAYPART TABLE                      
         USING DPTBLD,R3                                                        
         XC    DPTBLD(DPTBLL),DPTBLD   INIT FIRST ENTRY                         
*                                                                               
DPTLOOP  DS    0H                                                               
*                                                                               
         CLI   0(R5),C' '          DONE IF END OF LIST REACHED                  
         BNH   DPTDONE                                                          
*                                                                               
         MVC   RRDPKDPT,0(R5)      SET NEXT DAYPART IN KEY                      
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTINVE                                                          
*                                                                               
         CLI   ACTNUM,ACTREP       READ RECORD IF DOING REPORT                  
         BNE   DPTCONT                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,IO               POINT TO FOUND RECORD                        
         MVI   ELCODE,X'01'        SEARCH FOR DAYPART ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DPTCONT             IGNORE IF NOT FOUND                          
*                                                                               
         USING RRDPELEM,R6         ESTABLISH DAYPART ELEMENT                    
*                                                                               
         MVC   DPTBCODE,RRDPKDPT   SAVE DAYPART CODE                            
         MVC   DPTBSNAM,RRDPSNAM   SAVE SHORT NAME                              
         MVC   DPTBLNAM,RRDPLNAM   SAVE LONG NAME                               
*                                                                               
         LA    R3,DPTBLL(R3)       BUMP TO NEXT ENTRY IN DPTBL                  
         XC    DPTBLD(DPTBLL),DPTBLD  INIT NEXT ENTRY                           
*                                                                               
DPTCONT  DS    0H                                                               
*                                                                               
         LA    R5,1(R5)            BUMP TO NEXT ENTERED DAYPART                 
         BCT   R0,DPTLOOP                                                       
*                                                                               
DPTDONE  DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    DPTNUMX                                                          
*                                                                               
         TM    WHEN,X'20'          SKIP IF NOT SOON REPORT                      
         BNO   DPTNUMX                                                          
*                                                                               
         CLI   DPLIST+1,C' '       MAX ONE DPT FOR SOON REQUEST                 
         BH    DPTSOONE                                                         
*                                                                               
DPTNUMX  DS    0H                                                               
*                                                                               
         B     DPTVALX                                                          
*                                                                               
DPTMENUE DS    0H                  INVALID DAYPART MENU ID                      
*                                                                               
         MVC   RERROR,=AL2(DPTMNNF)                                             
         B     DPTERR                                                           
*                                                                               
DPTSOONE DS    0H                  MAX ONE DPT ON SOON REQUEST                  
*                                                                               
         MVC   RERROR,=AL2(MAXDPTSN)                                            
         B     DPTERR                                                           
*                                                                               
DPTINVE  DS    0H                                                               
*                                                                               
         MVC   RERROR,=AL2(INVDP)  INVALID DAYPART                              
*                                                                               
DPTERR   DS    0H                                                               
*                                                                               
         GOTO1 MYERROR                                                          
*                                                                               
DPTVALX  DS    0H                                                               
         TITLE 'RERMP21 - EDIT FOR O/N TRANSFER - INVRVAL'                      
***********************************************************************         
*                                                                     *         
*        RERMP21 - EDIT FOR O/N TRANSFER - INVENTORY RANGE            *         
*                                                                     *         
*        VALIDATE INVENTORY RANGE - NNNN-NNNN - SECOND NOT NEEDED     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INVRVAL  DS    0H                                                               
*                                                                               
         LA    R2,TITINVRH         POINT TO INVENTORY RANGE                     
*                                                                               
         XC    INVLIST,INVLIST     INIT INVENTORY SAVEAREA                      
*                                                                               
         LA    R0,INVMAX           INIT INV COUNTER                             
*                                                                               
         CLI   5(R2),0             IF THERE IS NO INPUT                         
         BNE   INVR10                                                           
*                                                                               
         CLC   STMENU,SPACES          INV REQ'D IF USING STATION MENU           
         BH    INVRREQE                                                         
*                                                                               
         CLI   TITDPTH+5,0            WE MUST HAVE A DAYPART ENTERED            
         BE    INVRNOTE                                                         
*                                                                               
         B     INVRVALX               NO INPUT OKAY                             
*                                                                               
INVR10   DS    0H                                                               
*                                                                               
         GOTO1 ANY                 READ IN INVENTORY NUMBERS                    
*                                                                               
         LH    R1,=Y(BUFF-SYSD)    GET START OF BUFFER ADDRESS                  
         LA    R1,SYSD(R1)                                                      
         ST    R1,SBUFF            SAVE A(BUFFER START)                         
*                                                                               
         MVC   DMCB+8(4),=C',=,-'  SCAN FOR SINGLE AND RANGES                   
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),('INVMAX',SBUFF)                               
*                                                                               
         CLI   DMCB+4,0            MUST HAVE AT LEAST ONE INV NO.               
         BNH   INVRNOTV                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4         NUMBER OF SCANNED ENTRIES                    
*                                                                               
         CH    R0,=Y(INVMAX)       MAKE SURE IT IS NOT TOO MANY                 
         BH    INVRMAXE                                                         
*                                                                               
         L     R4,SBUFF            POINT TO SCANNER BLOCK                       
         LA    R5,INVLIST          POINT TO INVENTORY SAVEAREA                  
*                                                                               
         CH    R0,=H'1'            IF ONLY ONE INVENTORY NUMBER                 
         BH    INVR30                                                           
*                                                                               
         CLI   0(R4),3                OF LENGTH 3                               
         BNE   INVR30                                                           
*                                                                               
         CLC   =C'ALL',12(R4)         COULD BE 'ALL'                            
         BE    INVRDONE               AND IS ALLOWED                            
*                                                                               
INVR30   DS    0H                                                               
*                                                                               
INVRLOOP DS    0H                                                               
*                                                                               
         MVC   0(4,R5),SPACES      INIT SAVED INVENTORY NUMBER                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)          LENGTH OF FIRST INVENTORY NUMBER             
         BZ    INVRDONE            END OF INPUT                                 
*                                                                               
         CH    RF,=AL2(L'RINVKINV) CHECK ON MAX LENGTH                          
         BH    INVRLNGE                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),12(R4)      FIRST INVENTORY NUMBER                       
*                                                                               
         MVC   4(4,R5),0(R5)       COPY INV NUMBER                              
*                                                                               
         ICM   RF,1,1(R4)          LENGTH OF SECOND INVENTORY NUMBER            
         BZ    INVRLP10            SINGLE INVENTORY NUMBER                      
*                                                                               
         CH    RF,=AL2(L'RINVKINV) CHECK ON MAX LENGTH                          
         BH    INVRLNGE                                                         
*                                                                               
         MVC   4(4,R5),SPACES      INIT SECOND INVENTORY NUMBER                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R5),22(R4)      SECOND INVENTORY NUMBER                      
*                                                                               
         CLC   0(4,R5),4(R5)       SECOND MUST BE HIGHER THAN FIRST             
         BH    INVRERR2                                                         
*                                                                               
INVRLP10 DS    0H                                                               
*                                                                               
         CLI   MENUCNT,1           SKIP IF MORE THAN ONE STATION                
         BH    INVRCONT                                                         
*                                                                               
*        READ FOR INVENTORY MASTER POINTER                                      
*                                                                               
         XC    KEY,KEY             ESTABLISH INVENTORY RECORD KEY               
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
*                                                                               
         MVI   RINVKTYP,RINVKTYQ   SET INVENTORY RECORD TYPE                    
         MVC   RINVKREP,AGENCY     USE SIGN ON REP                              
         LA    R1,STLIST           POINT TO STATION LIST                        
         MVC   RINVKSTA,STLSSTAC-STLISTD(1)   ACTIVE STATION                    
         MVC   RINVKINV,0(R5)      USE STARTING INVENTORY NUMBER                
*                                                                               
         GOTO1 HIGH                READ FOR POINTER                             
*                                                                               
         CLC   RINVKEY(RINVKINV-RINVKEY),KEYSAVE   SAME STATION                 
         BNE   INVRNFE                                                          
*                                                                               
         CLC   RINVKINV,0(R5)      INV NO. MUST BE IN RANGE                     
         BL    INVRNFE                                                          
*                                                                               
         CLC   RINVKINV,4(R5)      INV NO. MUST BE IN RANGE                     
         BH    INVRNFE                                                          
*                                                                               
INVRCONT DS    0H                                                               
*                                                                               
         LA    R4,32(R4)           NEXT SCANNED BLOCK                           
         LA    R5,8(R5)            NEXT SAVEAREA                                
         BCT   R0,INVRLOOP                                                      
*                                                                               
INVRDONE DS    0H                  ALL INV NOS. VALID                           
*                                                                               
         B     INVRX                                                            
*                                                                               
*        INVENTORY VALIDATION ERROR MESSAGES                                    
*                                                                               
INVRERR2 DS    0H                  ILLEGAL INVENTORY RANGE                      
         MVC   RERROR,=AL2(INVRSEQE)                                            
         B     INVRERR                                                          
*                                                                               
INVRNFE  DS    0H                  INVENTORY NUMBER NOT FOUND                   
         MVC   RERROR,=AL2(INVRNTFD)                                            
         B     INVRERR                                                          
*                                                                               
INVRLNGE DS    0H                  INVENTORY NUMBER TOO LONG                    
         MVC   RERROR,=AL2(INVRLNGQ)                                            
         B     INVRERR                                                          
*                                                                               
INVRMAXE DS    0H                  TOO MANY INVENTORY NUMBERS                   
         MVC   RERROR,=AL2(INVRMAXQ)                                            
         B     INVRERR                                                          
*                                                                               
INVRREQE DS    0H                  STATIONS MENU REQUIRES INV ENTRY             
         MVC   RERROR,=AL2(INVRMNRQ)                                            
         B     INVRERR                                                          
*                                                                               
INVRNOTE DS    0H                  DAYPART REQUIRED                             
         LA    R2,TITDPTH          PUT CURSOR AT DAYPART FIELD                  
         MVC   RERROR,=AL2(MISSING) MISSING INPUT                               
         B     INVRERR                                                          
*                                                                               
INVRNOTV DS    0H                  INVALID INPUT                                
         MVC   RERROR,=AL2(INVALID)                                             
         B     INVRERR                                                          
*                                                                               
INVRERR  DS    0H                                                               
*                                                                               
         ST    R2,FADDR            A(FIELD IN ERROR)                            
*                                                                               
         LA    RF,INVMAX           MAXIMUM ALLOWED INV NOS.                     
         SR    RF,R0                                                            
         LA    RF,1(RF)            NUMBER OF INV NO. IN ERROR                   
         STC   RF,FADDR            PASS ITEM IN ERROR NUMBER                    
*                                                                               
         GOTO1 MYERROR                                                          
*                                                                               
INVRX    DS    0H                                                               
*                                                                               
*                                                                               
INVNOTVE EQU   690                 INVENTORY ID NOT VALID                       
INVRNGER EQU   691                 INVALID RANGE                                
INVRNTFD EQU   797                 INVENTORY NOT FOUND                          
INVRLNGQ EQU   798                 INVENTORY NUMBER TOO LONG                    
INVRMAXQ EQU   799                 TOO MANY INVENTORY NUMBERS                   
INVRMNRQ EQU   800                 INVENTORY REQ'D WITH STATION MENU            
INVRSEQE EQU   691                 INVENTORY RANGE ILLEGAL                      
*                                                                               
INVRVALX DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'RERMP21 - EDIT FOR O/N TRANSFER - VSTRT'                        
***********************************************************************         
*                                                                     *         
*        RERMP21 - EDIT FOR O/N TRANSFER - START DATE                 *         
*                                                                     *         
*        VALIDATE START DATE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VSTRT    DS    0H                                                               
*                                                                               
         LA    R2,TITSTRTH         POINT TO START DATE                          
         XC    STRTOPT,STRTOPT     INIT START DATE OPTIONS                      
*                                                                               
         CLI   5(R2),0             OKAY IF NO START DATE ENTERED                
         BE    VRSTRTX                                                          
*                                                                               
         CLC   8(3,R2),=C'ALL'     OKAY IF 'ALL' ENTERED                        
         BE    VRSTRTX                                                          
*                                                                               
         GOTO1 DATVAL,PARAS,(0,8(R2)),WORK   VALIDATE ENTERED DATE              
*                                                                               
         CLI   PARAS+3,0                                                        
         BNE   VRSTRT1                                                          
*                                                                               
         MVC   RERROR,=AL2(INVDATE) ERROR - INVALID DATE                        
         GOTO1 MYERROR                                                          
*                                                                               
VRSTRT1  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,PARAS,(0,WORK),(3,STRTOPT)  SAVE START DATE               
*                                                                               
VRSTRTX  DS    0H                                                               
*                                                                               
*        VALIDATE END DATE                                                      
*                                                                               
VREND    LA    R2,TITENDH          POINT TO END DATE                            
         MVC   ENDOPT,=X'FFFFFF'   INIT INTERNAL END DATE                       
*                                                                               
         CLI   5(R2),0             OKAY IF NO END DATE ENTERED                  
         BE    VRENDX                                                           
*                                                                               
         CLC   8(3,R2),=C'ALL'     OKAY IF 'ALL' ENTERED                        
         BE    VRENDX                                                           
*                                                                               
         GOTO1 DATVAL,PARAS,(0,8(R2)),WORK  VALIDATE ENTERED DATE               
         CLI   PARAS+3,0                                                        
         BNE   VREND1                                                           
*                                                                               
         MVC   RERROR,=AL2(INVDATE) ERROR - INVALID DATE                        
         GOTO1 MYERROR                                                          
*                                                                               
VREND1   DS    0H                                                               
*                                                                               
         GOTO1 DATCON,PARAS,(0,WORK),(3,ENDOPT) SAVE END DATE                   
*                                                                               
VRENDX   DS    0H                                                               
*                                                                               
*        LOCK STATIONS IF SOON PROCESSING                                       
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    VRLCKX                                                           
*                                                                               
         TM    WHEN,X'20'          SKIP IF NOT SOON REPORT                      
         BNO   VRLCKX                                                           
*                                                                               
VRLOCK   DS    0H                                                               
*                                                                               
         MVI   TWAWHEN,5                SET FOR UPDATIVE SOON                   
*                                                                               
         LA    R4,WORK                                                          
         USING LKKEYD,R4                                                        
*                                                                               
         XC    WORK,WORK                                                        
         L     R6,ACOMFACS                                                      
*                                                                               
         L     RF,CGETFACT-COMFACSD(,R6)                                        
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         MVC   LOCKSE,FASYS-FACTSD(RE)                                          
*                                                                               
         MVC   LOCKAGY,AGENCY                                                   
         MVC   LOCKRTY,=CL2'RI'                                                 
         XC    LOCKKEY+5(5),LOCKKEY+5                                           
*                                                                               
*        FIND V(LOCKET)                                                         
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLOCKET                                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,DMCB             A(LOCKET)                                    
*                                                                               
         LA    R3,STLIST           POINT TO STATION LIST                        
         USING STLISTD,R3          ESTABLISH STATION LIST ENTRY                 
*                                                                               
VRLOCKLP DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD DONE AT END OF LSIT                     
         BZ    VRLOCKDN                                                         
*                                                                               
         MVC   LOCKKEY(5),STLSSTAC SET CALL LETTERS                             
*                                                                               
         GOTO1 (R5),DMCB,('LKLOCKQ',LKKEYD),(R6),0                              
*                                                                               
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   VRLCKMSG            LOCK STATION                                 
*                                                                               
VRLOCKCN DS    0H                                                               
         LA    R3,STLISTL(R3)      NEXT STATION IN LIST                         
         B     VRLOCKLP                                                         
*                                                                               
VRLOCKDN DS    0H                                                               
*                                                                               
         B     VRLCKX                                                           
*                                                                               
*        ERROR - STATION LOCKED ALREADY                                         
*                                                                               
VRLCKMSG DS    0H                                                               
*                                                                               
*        UNLOCK ALL STATIONS ALREADY LOCKED                                     
*                                                                               
         ST    R3,FULL             SAVE A(STATION IN ERROR)                     
*                                                                               
         LA    R3,STLIST           POINT TO STATION LIST                        
         USING STLISTD,R3          ESTABLISH STATION LIST ENTRY                 
*                                                                               
VRUNLKLP DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD DONE AT END OF LSIT                     
         BZ    VRUNLKDN                                                         
*                                                                               
         C     R3,FULL             STOP IF STATION IN ERROR REACHED             
         BE    VRUNLKDN                                                         
*                                                                               
         MVC   CSTAT,STLSSTAC      SET CALL LETTERS                             
*                                                                               
         GOTO1 (R5),(R1),('LKUNLKQ',LKKEYD),(R6)                                
*                                                                               
VRUNLKCN DS    0H                                                               
         LA    R3,STLISTL(R3)      NEXT STATION IN LIST                         
         B     VRUNLKLP                                                         
*                                                                               
VRUNLKDN DS    0H                                                               
*                                                                               
*        BUILD STATION CALL LETTERS FOR ERROR MESSAGE                           
*                                                                               
         L     R3,FULL             POINT TO STATION ALREADY LOCKED              
*                                                                               
         MVC   WORK,SPACES                                                      
         LA    R1,WORK             POINT TO WORKAREA                            
         STCM  R1,7,RTXTADR        PASS A(MESSGE) TO GETTXT                     
*                                                                               
         MVC   0(4,R1),STLSSTAC    CALL LETTERS                                 
*                                                                               
         LA    R1,3(R1)            END OF CALL LETTERS                          
*                                                                               
         CLI   0(R1),C' '          IF IT ENDS IN BLANK                          
         BH    *+6                                                              
         BCTR  R1,0                   BACK UP                                   
*                                                                               
         LA    R1,1(R1)            NEXT AVAIL PRINT POSITION                    
*                                                                               
         CLI   STLSSTAC+4,C'T'     SKIP IF MEDIA T OR BLANK                     
         BE    *+8                                                              
         CLI   STLSSTAC+4,C' '                                                  
         BNH   *+18                                                             
         MVI   0(R1),C'-'          PRINT MEDIA                                  
         MVC   0(1,R1),STLSSTAC+4                                               
         LA    R1,2(R1)            BUMP TO NEXT PRINT POSITION                  
*                                                                               
         LA    RF,WORK                                                          
         SR    R1,RF               TEXT LENGTH                                  
         STC   R1,RTXTLEN          PASS TEXT LENGTH TO GETTXT                   
*                                                                               
         LA    R2,TITSTATH         CURSOR TO STATION FIELD                      
*                                                                               
         MVC   RERROR,=AL2(STALOCK)  STATION LOCKED ALREADY                     
         GOTO1 MYERROR                                                          
*                                                                               
STALCKD  EQU   547                 STATION LOCKED                               
STALOCK  EQU   795                 STATION LOCKED                               
*                                                                               
VRLCKX   DS    0H                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
*        ISSUE LTRANS REQUEST IF ON-LINE AND REP WANTS IT                       
*                                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    VRLTRNX                                                          
*                                                                               
         TM    WHEN,X'70'          SKIP IF NOT A REPORT                         
         BZ    VRLTRNX                                                          
*                                                                               
         LA    R3,STLIST           POINT TO STATION LIST                        
         USING STLISTD,R3          ESTABLISH STATION LIST ENTRY                 
*                                                                               
VRLTRNLP DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD DONE AT END OF LSIT                     
         BZ    VRLTRNDN                                                         
*                                                                               
         MVC   CSTAT(5),STLSSTAC SET CALL LETTERS                               
*                                                                               
         GOTO1 VLTRANS             GO CHECK IF LTRANS TO BE ISSUED              
*                                                                               
VRLTRNCN DS    0H                                                               
         LA    R3,STLISTL(R3)      NEXT STATION IN LIST                         
         B     VRLTRNLP                                                         
*                                                                               
VRLTRNDN DS    0H                                                               
*                                                                               
VRLTRNX  DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
*        VALIDATE RATING BOOK                                                   
*                                                                               
         LA    R2,TITBOOKH         POINT TO RATING BOOK                         
*                                                                               
VRECX    DS    0H                                                               
*                                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
*        GETEL MACRO                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T81021 --- RERMP21 --- OVERNIGHT TRANSFERS - 1'                 
********************************************************************            
*                                                                  *            
*        VALIDATE STATION                                          *            
*                                                                  *            
*        LIST OF STATIONS OR A MENU ID DESIGNATED AS M=XXXX        *            
*                                                     *XXXX        *            
*                                                                  *            
*        R2 ==> STATION FIELD                                      *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VALSTA   NTR1  BASE=*,LABEL=*                                                   
*                                  **********************************           
*                                  *                                *           
*                                  *           STATION              *           
*                                  *                                *           
*                                  **********************************           
*                                                                               
         USING GEND,RC                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         XC    STMENU,STMENU       INIT STATION MENU CODE                       
         XC    STMENUNM,STMENUNM   INIT STATION MENU NAME                       
*                                                                               
         LA    R5,STLIST           ESTABLISH STATION LIST                       
         USING STLISTD,R5                                                       
         XC    STLISTD(STLISTL),STLISTD   INIT FIRST ENTRY IN LIST              
*                                                                               
         GOTO1 ANY                 INPUT REQUIRED                               
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(16,BUFF),0,0 SCAN INPUT                       
*                                                                               
         MVC   ACTUAL,DMCB+4       SAVE NUMBER OF ENTRIES                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ACTUAL           NUMBER OF ENTRIES IN FIELD                   
*                                                                               
         LA    R4,BUFF             START OF SCAN BLOCK ENTRIES                  
*                                                                               
*        IF ENTRY STARTS WITH '*', MUST BE A MENU ID                            
*                                                                               
         CLI   12(R4),C'*'         MENU INDICATED                               
         BNE   VSTAMN20                                                         
*                                                                               
         CLI   ACTUAL,1            MAX 1 SCANNED ENTRY ALLOWED                  
         BH    VSTAMN1E                                                         
*                                                                               
         CLI   1(R4),5             MENU ID MAX 4 LONG (ID PLUS *)               
         BH    VSTAMNXE                                                         
*                                                                               
         MVC   STMENU,13(R4)       SAVE MENU CODE                               
         OC    STMENU,SPACES       SPACE FILL                                   
*                                                                               
         B     VSTAMN50                                                         
*                                                                               
VSTAMN20 DS    0H                                                               
*                                                                               
*        IF ENTRY IS 'M' THEN MUST BE A MENU ID                                 
*                                                                               
         CLI   0(R4),1             IF ENTRY IS 1 LONG                           
         BNE   VSTAMNUN                                                         
         CLI   12(R4),C'M'         AND MENU INDICATED                           
         BNE   VSTAMNUN                                                         
*                                                                               
         CLI   ACTUAL,1            MAX 1 SCANNED ENTRY ALLOWED                  
         BH    VSTAMN1E                                                         
*                                                                               
         CLI   1(R4),4             MENU ID MAX 4 LONG                           
         BH    VSTAMNXE                                                         
*                                                                               
         MVC   STMENU,22(R4)       SAVE MENU CODE                               
         OC    STMENU,SPACES       SPACE FILL                                   
*                                                                               
VSTAMN50 DS    0H                                                               
*                                                                               
*        READ MARKET STATIONS LIST                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY              ESTABLISH SET RECORD KEY                     
         USING RSETKEY,R3                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   SET AS SET RECORD RECORD                     
         MVC   RSETKREP,CPARREP    SET REP CODE                                 
         MVC   RSETKSET,=C'MS'     SET RECORD ID                                
         MVC   RSETKID,STMENU      SET SET IDENTIFIER                           
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   VSTAMNNF                                                         
*                                                                               
         GOTO1 GETREC              READ IN SET RECORD                           
*                                                                               
         SR    RF,RF                                                            
         LA    R6,IO                                                            
         LA    R6,RSETELEM-RSETREC(R6) FIRST ELEMENT                            
*                                                                               
         USING RSETDESD,R6         ESTABLISH DESCRIPTIVE ELEMENT                
*                                                                               
         CLI   RSETDCDE,0          CHECK FOR END OF RECORD                      
         BE    VSTAMN10               SKIP IF ELEMENT NOT FOUND                 
         CLI   RSETDCDE,RSETDCDQ   LOOKING FOR DESCRIPTIVE ELEMENT              
         BE    *+16                                                             
         IC    RF,RSETDELN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)                                                      
         B     *-24                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETDELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETDOV)      DECREMENT BY OVERHEAD LENGTH                 
         BNP   VSTAMN10            IGNORE IF NOT THERE                          
*                                                                               
         MVC   STMENUNM,SPACES     INIT DESCRIPTION                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STMENUNM(0),RSETDESC SAVE MENU NAME                              
*                                                                               
VSTAMN10 DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         LA    R6,IO                                                            
         LA    R6,RSETELEM-RSETREC(R6) FIRST ELEMENT                            
*                                                                               
         USING RSETMEMD,R6         ESTABLISH DESCRIPTIVE ELEMENT                
*                                                                               
         CLI   RSETMCDE,0          CHECK FOR END OF RECORD                      
         BE    VSTAMNVD               MUST FIND ELEMENT                         
         CLI   RSETMCDE,RSETMCDQ   LOOKING FOR MEMBERS ELEMENT                  
         BE    *+16                                                             
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         LA    R6,0(RF,R6)                                                      
         B     *-24                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETMTOV)     DECREMENT BY OVERHEAD LENGTH                 
         BNP   VSTAMNVD            MUST HAVE SOME MEMBERS                       
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'5'            CALCULATE NUMBER OF STATIONS IN LIST         
*                                                                               
         STC   RF,MENUCNT          SET NUMBER OF STATIONS                       
         SR    R0,R0               INIT STATION SORT ORDER                      
*                                                                               
         LA    R1,RSETMEMB         POINT TO FIRST STATION IN LIST               
*                                                                               
VSTAMNLP DS    0H                                                               
*                                                                               
         MVC   STLSSTAC,0(R1)      SAVE STATION CALL LETTERS                    
*                                                                               
         CLI   STLSSTAC+4,C' '     IF NO BAND                                   
         BH    *+8                                                              
         MVI   STLSSTAC+4,C'T'        DEFAULT TO TV                             
*                                                                               
         STC   R0,STLSSTCD         SET SORT ORDER                               
*                                                                               
VSTAMNCN DS    0H                                                               
*                                                                               
         LA    R1,5(R1)            BUMP TO NEXT STATION                         
         LA    R5,STLISTL(R5)      BUMP TO NEXT STATION AREA                    
         XC    STLISTD(STLISTL),STLISTD   INIT NEXT ENTRY IN LIST               
         AH    R0,=H'1'            BUMP SORT ORDER ID                           
*                                                                               
         BCT   RF,VSTAMNLP                                                      
*                                                                               
VSTAMNDN DS    0H                                                               
*                                                                               
         B     VSTASTAX            END OF MENU LIST                             
*                                                                               
VSTAMNUN DS    0H                                                               
*                                                                               
*        BUILD LIST OF INDIVIDUALLY ENTERED STATIONS                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ACTUAL           NUMBER OF ENTRIES                            
         STC   RE,MENUCNT          NUMBER OF REQUESTED STATIONS                 
*                                                                               
         SR    R0,R0               INIT STATION SORT ORDER                      
         SR    RF,RF                                                            
*                                                                               
VSTASTLP DS    0H                                                               
*                                                                               
         OC    STLSSTAC,SPACES     INIT STATION CALL LETTERS                    
         ICM   RF,1,0(R4)          ENTRY LENGTH                                 
         BZ    VSTASTNE            ENTRY REQUIRED                               
*                                                                               
         LA    R3,12-1(RF,R4)      POINT TO LAST OF STATION ID                  
*                                                                               
         CLI   0(R3),C'-'          FIND '-'                                     
         BE    *+18                                                             
         BCTR  R3,0                BACK UP A CHARACTER                          
         BCT   RF,*-10                                                          
         IC    RF,0(R4)            USE FULL ID LENGTH                           
         B     *+6                                                              
*                                                                               
         BCTR  RF,0                RECTIFY CALL LETTERS LENGTH                  
*                                                                               
         CH    RF,=H'4'            MAX 4 CHARACTERS FOR CALL LETTERS            
         BH    VSTASTXE                                                         
*                                                                               
         MVC   STLSSTAC,SPACES     INIT CALL LETTERS                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STLSSTAC(0),12(R4)  SAVE STATION CALL LETTERS                    
*                                                                               
         MVI   STLSSTAC+4,C'T'     ASSUME TV                                    
*                                                                               
         CLI   0(R3),C'-'          IF THERE IS A BAND ENTERED                   
         BNE   *+10                                                             
         MVC   STLSSTAC+4(1),1(R3)    ADD IT TO CALL LETTERS                    
*                                                                               
         STC   R0,STLSSTCD         SET SORT ORDER                               
*                                                                               
VSTASTCN DS    0H                                                               
*                                                                               
         LA    R4,32(R4)           POINT TO NEXT STATION                        
         LA    R5,STLISTL(R5)      BUMP TO NEXT STATION AREA                    
         XC    STLISTD(STLISTL),STLISTD   INIT NEXT ENTRY IN LIST               
         AH    R0,=H'1'            BUMP SORT ORDER ID                           
*                                                                               
         BCT   RE,VSTASTLP                                                      
*                                                                               
VSTASTDN DS    0H                                                               
*                                                                               
VSTASTAX DS    0H                                                               
*                                                                               
*        VALIDATE STATIONS IN LIST                                              
*                                                                               
         LA    R5,STLIST           LIST OF STATIONS TO BE VALIDATED             
*                                                                               
VSTAVALL DS    0H                                                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD CHECK FOR END OF LIST                   
         BZ    VSTAVALD                                                         
*                                                                               
*        READ STATION FILE TO VALIDATE STATION                                  
*                                                                               
         XC    KEY,KEY             ESTABLISH STATION RECORD KEY                 
         LA    R3,KEY                                                           
         USING RSTAKEY,R3                                                       
*                                                                               
         MVI   RSTAKTYP,X'02'      RECORD TYPE                                  
         MVC   RSTAKREP,AGENCY     REP ID                                       
         MVC   RSTAKSTA,STLSSTAC   STATION                                      
*                                                                               
         CLI   RSTAKSTA+4,C'T'     MEDIA IS BLANK FOR ANY TV STATION            
         BE    VSTASTE5                                                         
         CLI   RSTAKSTA+4,X'F0'    IE. MEDIA= T,1-9                             
         BL    VSTASTE9                                                         
         CLI   RSTAKSTA+4,X'F9'                                                 
         BH    VSTASTE9                                                         
*                                                                               
VSTASTE5 DS    0H                                                               
*                                                                               
         MVI   RSTAKSTA+4,C' '                                                  
*                                                                               
VSTASTE9 DS    0H                                                               
*                                                                               
         GOTO1 HIGH                READ FOR STATION POINTER                     
*                                                                               
         CLC   RSTAKEY,KEYSAVE     MUST FIND STATION                            
         BNE   VSTASTNV                                                         
*                                                                               
*        VALIDATE STATION/MARKET COMBINATION                                    
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK                            
*                                                                               
         MVC   DBSELAGY,CPARREP                                                 
         MVC   DBFILE,=C'TPT'                                                   
*                                                                               
         MVC   DBAREC,AIO2             A(DEMO WORK AREA)                        
*                                                                               
         MVI   DBFUNCT,DBVLSTBK    VALIDATE STATION BOOK                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELSRC,TITSRCE    SET SOURCE FIELD                             
         MVC   DBSELBK,CBOOKS+1    SET BOOK                                     
         MVC   DBSELSTA,STLSSTAC   SET STATION CALL LETTERS                     
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         MVC   DBBTYPE,CBOOKS+3    SET BOOK TYPE                                
*                                                                               
         CLI   DBSELSTA+4,C'T'     CHANGE CALL LETTER SUFFIX                    
         BE    *+8                                                              
         CLI   DBSELSTA+4,C'2'     FOR A PS/1 STATION                           
         BE    *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCK,0,0                                           
         CLI   DBERROR,0           EXIT ON ERROR                                
         BNE   VRBKSTAE                                                         
*                                                                               
VSTAVALC DS    0H                                                               
*                                                                               
         LA    R5,STLISTL(R5)      NEXT STATION IN LIST                         
         B     VSTAVALL                                                         
*                                                                               
VSTAVALD DS    0H                                                               
*                                                                               
         B     VALSTAX                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VSTASTXE DS    0H                  STATION ID TOO LONG                          
VSTASTNV DS    0H                  STATION NOT ON FILE                          
         MVC   RERROR,=AL2(INVSTA)                                              
         B     VSTAERR                                                          
*                                                                               
VSTASTNE DS    0H                  STATION ENTRY NEEDED                         
*                                                                               
         MVC   RERROR,=AL2(MISSING)                                             
         B     VSTAERR                                                          
*                                                                               
VSTAMNVD DS    0H                  EMPTY MENU                                   
         MVC   RERROR,=AL2(MENUVOID)                                            
         B     VSTAERR                                                          
*                                                                               
VSTAMNNF DS    0H                  MENU NOT FOUND                               
         MVC   RERROR,=AL2(MENUNOTF)                                            
         B     VSTAERR                                                          
*                                                                               
VSTAMNXE DS    0H                  MENU ID MAX 4 LONG                           
         MVC   RERROR,=AL2(MENUBIG)                                             
         B     VSTAERR                                                          
*                                                                               
VSTAMNNE DS    0H                  NO MENU ID                                   
         MVC   RERROR,=AL2(MISSING)                                             
         B     VSTAERR                                                          
*                                                                               
VSTAMN1E DS    0H                  AT MOST ONE MENU                             
         MVC   RERROR,=AL2(MENUMANY)                                            
         B     VSTAERR                                                          
*                                                                               
VRBKSTAE DS    0H                                                               
*                                                                               
         MVC   RERROR,=AL2(INVBKSTA) INVALID BOOK FOR STATION                   
         B     VSTAERR                                                          
*                                                                               
VSTAERR  DS    0H                                                               
*                                                                               
         ST    R2,FADDR            A(FIELD IN ERROR)                            
*                                                                               
         LA    RF,STLIST           START OF STATIONS                            
         SR    R5,RF                                                            
         LR    RF,R5                                                            
         SR    RE,RE                                                            
         D     RE,=A(STLISTL)      RELATIVE NUMBER OF CURRENT STATION           
*                                                                               
         LA    RF,1(RF)            ABSOLUTE NUMBER                              
         STC   RF,FADDR            SET ITEM NUMBER                              
*                                                                               
VSTAERR1 DS    0H                                                               
*                                                                               
         GOTO1 =A(MYCURS),RR=RELO21                                             
*                                                                               
VALSTAX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        ERROR EQUATES                                                          
*                                                                               
MENUVOID EQU   791                 MENU HAS NO MEMBERS                          
MENUNOTF EQU   792                 MENU NOT ON FILE                             
MENUBIG  EQU   793                 MENU ID TOO BIG                              
MENUMANY EQU   794                 ONLY ONE MENU ALLOWED                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T81021 --- RERMP21 --- OVERNIGHT TRANSFERS - MYCURS'            
********************************************************************            
*                                                                  *            
*        POSITION CURSOR TO CORRECT FIELD IN ERRORS                *            
*                                                                  *            
*        INPUT : FADDR = AL1(FLD NUMBER),AL3(SCREEN HEADER)        *            
*                                                                  *            
*        R2 ==> STATION FIELD                                      *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
MYCURS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    MYCURSX                                                          
*                                                                               
         L     R2,FADDR            POINT TO FIRST FIELD IN ERROR                
*                                                                               
         L     R1,ATIOB            ESTABLISH TIOB                               
         USING TIOBD,R1                                                         
*                                                                               
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
*                                                                               
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
*                                                                               
         CLI   FADDR,0             APPLICATION MUST SET FIELD NUMBER            
         BE    MYCURSX                                                          
*                                                                               
         LA    RE,8(R2)            START OF FIRST FIELD                         
*                                                                               
         ZIC   RF,FADDR            NUMBER OF FIELD IN ERROR                     
         BCT   RF,*+8              RELATIVE FIELD NUMBER                        
         B     MYCURS0D            FIRST FIELD IS ONE IN ERROR                  
*                                                                               
         SR    R0,R0                                                            
*                                                                               
MYCURS0L DS    0H                                                               
*                                                                               
         IC    R0,5(R2)            R0 HAS FIELD LENGTH                          
*                                                                               
MYCURS1L DS    0H                                                               
*                                                                               
         CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   MYCURS1C                                                         
*                                  IF COMMA FOUND                               
         BCT   RF,MYCURS1C            DECREMENT FIELD COUNTER                   
*                                     IF FIELD FOUND                            
         LA    RE,1(RE)                  BUMP TO START OF NEXT ITEM             
         B     MYCURS0D                  DONE                                   
*                                                                               
MYCURS1C DS    0H                                                               
*                                                                               
         LA    RE,1(RE)                                                         
         BCT   R0,MYCURS1L                                                      
*                                                                               
MYCURS1D DS    0H                  END OF DATA IN SCREEN FIELD                  
*                                                                               
MYCURS0C DS    0H                                                               
*                                                                               
         IC    R0,0(R2)            SCREEN FIELD LENGTH                          
         AR    R2,R0               BUMP TO NEXT SCREEN FIELD                    
*                                                                               
         BCT   RF,MYCURS0L         DECREMENT FIELD COUNTER                      
*                                     IF FIELD FOUND                            
         LA    RE,8(R2)                  POINT TO FIRST IN FIELD                
*                                                                               
MYCURS0D DS    0H                                                               
*                                                                               
         LA    RF,8(R2)            START OF FIELD                               
         SR    RE,RF               DISPLACEMENT TO POSITION IN FIELD            
         STC   RE,TIOBCURI                                                      
*                                                                               
MYCURSX  DS    0H                                                               
         GOTO1 MYERROR                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'RERMP21 --- OVERNIGHT TRANSFERS - INCLUDED BOOKS'               
********************************************************************            
*                                                                  *            
*        INCLUDED PANBOOKS                                         *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
       ++INCLUDE RERMPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPE1D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
* RERMPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE RERMPWORKD                                                     
         PRINT ON                                                               
         TITLE 'RERMP21 - OVERLAY WORKING STORAGE'                              
***********************************************************************         
*                                                                     *         
*        OVERLAY WORKING STORAGE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         ORG   SYSSPARE                                                         
STRTOPT  DS    CL3                                                              
ENDOPT   DS    CL3                                                              
DPMENU   DS    CL4                 DAYPART MENU ID                              
DPLIST   DS    CL20                DAYPART TABLE                                
RELO21   DS    F                   RE-LOCATION FACTOR OVLY 21                   
RELO23   DS    F                   RE-LOCATION FACTOR OVLY 23                   
FADDR    DS    A                   A(FIELD IN ERROR)                            
*                                                                               
DPTBL    DS    XL(24*DPTBLL)       DAYPART TABLE                                
*                                                                               
STMENU   DS    CL4                 STATION MENU CODE                            
STMENUNM DS    CL60                STATION MENU NAME                            
*                                                                               
STLIST   DS    XL(24*STLISTL)      STATIONS LIST                                
*                                                                               
INVLIST  DS    XL((INVMAX+1)*2*L'RINVKINV)  INVENTORY NUMBERS LIST              
*                                                                               
INVMAX   EQU   30                  MAXIMUM NUMBER OF INVENTORY NUMBERS          
*                                                                               
SBUFF    DS    A                   A(BUFFER)                                    
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
INVDP    EQU   234                 INVALID DAYPART                              
INVBKSTA EQU   639                 INVALID BOOK FOR STATION                     
MAXDPTSN EQU   654                 MAX 1 DPT FOR SOON REQUEST                   
DPTMNNF  EQU   655                 DAYPART MENU NOT FOUND                       
*                                                                               
         TITLE 'T81021 --- RERMP21 --- OVERNIGHT TRANSFERS - DPTBL'             
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DAYPART CODES AND NAMES                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTBLD   DSECT                                                                  
DPTBCODE DS    CL(L'RRDPCODE)      DAYPART CODE                                 
DPTBSNAM DS    CL(L'RRDPSNAM)      DAYPART SHORT NAME                           
DPTBLNAM DS    CL(L'RRDPLNAM)      DAYPART LONG NAME                            
DPTBLL   EQU   *-DPTBLD            LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T81021 --- RERMP21 --- OVERNIGHT TRANSFERS - STLISTD'           
***********************************************************************         
*                                                                     *         
*        DSECT FOR LIST OF STATIONS                                   *         
*                                                                     *         
***********************************************************************         
STLISTD  DSECT                                                                  
STLSSTCD DS    CL1                 STATION SORT CODE                            
STLSSTAC DS    CL5                 STATION CALL LETTERS                         
STLISTL  EQU   *-STLISTD           LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'RERMP21 --- OVERNIGHT TRANSFERS - INCLUDED BOOKS'               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* RERMPPROF                                                                     
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
* DDTWADCOND                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDTWADCOND                                                     
         PRINT ON                                                               
* DMREQHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMREQHDR                                                       
         PRINT ON                                                               
* FALOCKETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FALOCKETD                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* REGENRDP                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENRDP                                                       
         PRINT ON                                                               
* REGENINV                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENINV                                                       
         PRINT ON                                                               
* REGENSET                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENSET                                                       
         PRINT ON                                                               
* REGENSTA                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086RERMP21   02/11/10'                                      
         END                                                                    
