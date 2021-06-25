*          DATA SET CTMAD0E    AT LEVEL 003 AS OF 05/01/02                      
*          DATA SET CTMAD0E    AT LEVEL 196 AS OF 08/11/98                      
*PHASE TA0C0EC,*                                                                
         TITLE 'TA0C0E - $MAD NETWORK BUY UPLOAD'                               
**********************************************************************          
*   HISTORY OF CHANGES *                                                        
**********************************************************************          
*   04/20/92   (NS ) --- ORIGINAL ENTRY                              *          
**********************************************************************          
TA0C0E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C0E,RA,RR=R2                                                
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
*                                                                               
*                                                                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
*        INITIALIZE OVERLAY WIDE REGISTERS                                      
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
*                                                                               
         DROP  R7                  DROP 1ST APPLIC COMMON STORAGE               
*                                                                               
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    R2,RELO             SAVE RELOCATION ADDRESS                      
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
*                                                                               
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         MVC   MDLAST,PCLAST       LAST FRAME INDICATOR                         
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'INIT' INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                        
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'PROCSTRT' PROCESSES THE START MODE.  IT PROCESSES THE REQUEST FOR            
* DEMOGRAPHIC INFORMATION FROM THE PC AND RETURNS THE FIRST SCREENFUL           
* OF INFORMATION.  IT ALSO SETS THE INTERNAL CONTROL TO BEGIN THE               
* SEND OF SUBSEQUENT DATA.                                                      
*                                                                               
PROCSTRT NTR1                                                                   
         BAS   RE,PROCINIT         INITIALIZE WORK SCRATCH AREA                 
         BAS   RE,PROCRQST         PROCESS REQUEST                              
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* 'PROCMID' PROCESSES MIDDLE MODE.  IT PROCESSES AND SENDS THE 2ND              
*  THRU NTH SCREENS OF DATA.                                                    
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
         BAS   RE,PROCRQST         PROCESS REQUEST                              
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* 'PROCEND' PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.                 
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    INITIALIZES ALL VALUES IN WORK SCRATCH SPACE ON THE                        
*      START-UP PASS                                                            
*                                                                               
PROCINIT NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
         LA    RE,SCRATCH          A(SCRATCH WORK SPACE)                        
         LA    RF,LSCRATCH         L(SCRATCH WORK SPACE)                        
         XCEF  (RE),(RF)           INITIALIZE THE AREA                          
*--LOAD CORE RESIDENT PHASES                                                    
         LA    R2,CORETAB                                                       
         LA    R0,CORES                                                         
         LA    R3,COREFACS                                                      
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
PIN0100  MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB                                                     
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,PIN0100                                                       
*                                                                               
PIN0099  B     XIT                                                              
CORETAB  DS    0X                                                               
         DC    X'1732'                                                          
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    ESTABLISHES THE REQUEST IN THE WORK AREA.                                  
*    SETS POINTERS                                                              
*    MAKES 2ND THRU NTH CALL RESTART TRANSPARENT                                
*                                                                               
*                                                                               
PROCRQST NTR1                                                                   
*                                                                               
PR0002   EQU   *                                                                
         GOTO1 GETITEM             RETRIEVE ITEM FROM INPUT FRAME               
*                                                                               
         BNE   EXIT                ERROR CONDITION EXIT                         
*                                                                               
         CLI   EIFFLAG,C'Y'                                                     
         BE    PR0099                                                           
*                                                                               
         L     R1,TYPENUM          DETERMINE TYPE OF ITEM                       
         LA    R2,ITPAKHED                                                      
         CR    R1,R2               PACKAGE HEADER?                              
         BNE   PR0004              NO                                           
         MVI   MADERR,0            RESET PACKAGE ERROR SWITCH                   
         BAS   RE,PACKINFO         YES                                          
         CLI   MADERR,0                                                         
         BE    PR0002                                                           
         GOTO1 WRITERR,DMCB,(C'K',DUB)  WRITE PACKAGE ERROR                     
         B     PR0002                                                           
PR0004   EQU   *                                                                
         LA    R2,ITPRGREC                                                      
         CR    R1,R2               PROGRAM RECORD?                              
         BNE   PR0010              NO                                           
         BAS   RE,PROGINFO         YES                                          
         B     PR0002                                                           
PR0010   EQU   *                                                                
         LA    R2,ITPAKTRL                                                      
         CR    R1,R2               PACKAGE TRAILER                              
         BNE   PR0020              NO                                           
         BAS   RE,PACKUPDT         YES                                          
         B     PR0002                                                           
PR0020   EQU   *                                                                
         LA    R2,ITEOD                                                         
         CR    R1,R2               END OF DATA?                                 
         BNE   PR0014              NO                                           
         B     PR0099              YES                                          
PR0014   EQU   *                                                                
*   NO PROCESSING FOR AN UNRECOGNIZED TYPE                                      
         B     PR0002                                                           
PR0099   EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    CHECKS THE PACKAGE RECORD AGAINST THE FILE TO MAKE SURE ALL                
*    THE INFORMATION IN IN SYNC.                                                
*                                                                               
PACKINFO NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT0EIN01,R2         PACKAGE HEADER DSECT                         
         BAS   RE,SPOTSET                                                       
*                                                                               
         MVI   WRITESW,C'N'        SET UNITS WRITTEN TO NO                      
*                                                                               
         MVI   MADERR,1            AGENCY ERROR                                 
         CLC   SIGNON8,PACKAGY     CHECK AGENCY MATCH                           
         BNE   PACKIEX             YES                                          
*                                                                               
         MVI   MADERR,2            POSTING TYPE ERROR                           
         MVC   TWPSTTYP,PACKMED                                                 
         CLI   PACKMED,C'N'        NETWORK                                      
         BE    PACKI020                                                         
         CLI   PACKMED,C'C'        CABLE                                        
         BNE   *+12                                                             
         OI    TWCNTRL,X'01'                                                    
         B     PACKI020                                                         
         CLI   PACKMED,C'S'        SYNDICATION                                  
         BNE   *+12                                                             
         OI    TWCNTRL,X'02'                                                    
         B     PACKI020                                                         
         CLI   PACKMED,C'O'        SYNDICATION                                  
         BNE   PACKIEX                                                          
         OI    TWCNTRL,X'03'                                                    
*                                                                               
PACKI020 MVI   MADERR,3            INVALID AGENCY MEDIA COMBINATION             
         MVI   BYTE,C'N'                                                        
         GOTO1 VALIMED,DMCB,BYTE      VALIDATE THE MEDIA                        
         BNE   PACKIEX                                                          
         MVC   TWAGYMED,BAGYMED                                                 
*                                                                               
         MVI   MADERR,4            INVALID CLIENT                               
         GOTO1 VALICLT,DMCB,PACKCLI   VALIDATE THE CLIENT                       
         BNE   PACKIEX                                                          
         MVC   TWCLIENT,BCLT                                                    
*                                                                               
         L     RE,AIO                                                           
         USING CLTHDR,RE                                                        
         TM    COPT2,X'08'          CHECK FOR FROZEN CLIENT                     
         BO    PACKIEX                                                          
*                                                                               
         MVC   TWCLIC2,CCOST2                                                   
*                                                                               
         LA    RF,RATETAB                                                       
PACKI40  CLI   0(RF),X'40'                                                      
         BE    PACKI50                                                          
         CLC   CPROF+14(1),0(RF)                                                
         BE    PACKI45                                                          
         LA    RF,2(RF)                                                         
         B     PACKI40                                                          
*                                                                               
PACKI45  MVC   TWRATE,1(RF)        GET RATE TYPE                                
         CLI   CEXTRA+14,C'0'                                                   
         BE    *+10                                                             
         MVC   TWCVRGE,CEXTRA+14   GET COVERAGE FACTOR                          
         DROP  RE                                                               
*                                                                               
PACKI50  MVI   MADERR,5            INVALID MASTER ALLOCATION PRODUCT            
         MVI   TWMSTPRD,0          INITIALIZE MASTER PRODUCT                    
         CLI   PACKALOC,X'40'      CHECK FOR INPUT                              
         BNH   PACKI140                                                         
*--VALIDATE PRODUCT AGAINST CLIENT'S PRODUCT LIST.                              
         LA    RE,PRDLIST                                                       
         LA    RF,220                                                           
PACKI100 CLC   PACKALOC,0(RE)                                                   
         BE    PACKI120                                                         
         LA    RE,4(RE)                                                         
         BCT   RF,PACKI100                                                      
         B     PACKIEX                                                          
*                                                                               
PACKI120 MVC   TWMSTPRD,3(RE)                                                   
*                                                                               
PACKI140 MVI   MADERR,15                                                        
         GOTO1 CHKNUM,DMCB,(3,PACKEST)                                          
         BNZ   PACKIEX                                                          
*                                                                               
         MVC   QPRD(3),=CL3'POL'                                                
         CLI   PACKALOC,X'40'                                                   
         BNH   *+10                                                             
         MVC   QPRD(3),PACKALOC    QPRD NEEDED FOR EST VALIDATION               
*                                                                               
         MVI   MADERR,6            INVALID ESTIMATE                             
         GOTO1 VALIES1,DMCB,PACKEST   VALIDATE THE ESTIMATE                     
         BNE   PACKIEX                                                          
*                                                                               
         L     RE,AIO                                                           
         USING ESTHDR,RE                                                        
         MVC   TWESTC2,ECOST2                                                   
*                                                                               
         LA    RF,RATETAB                                                       
PACKI160 CLI   0(RF),X'40'                                                      
         BE    PACKI170                                                         
         CLC   ERATE,0(RF)                                                      
         BE    PACKI165                                                         
         LA    RF,2(RF)                                                         
         B     PACKI160                                                         
*                                                                               
PACKI165 MVC   TWRATE,1(RF)        GET RATE TYPE                                
         MVC   TWCVRGE,ERATECST    GET COVERAGE                                 
         DROP  RE                                                               
*                                                                               
*--CHECK RECORDS START END DATES MATCH THE ESTIMATES                            
PACKI170 MVI   MADERR,16                                                        
         GOTO1 CHKNUM,DMCB,(16,PACKESST)                                        
         BNZ   PACKIEX                                                          
*                                                                               
*  CONVERT INPUT DATES TO YEAR 2000 FORMAT                                      
         GOTO1 DATCON,DMCB,(0,PACKESST+2),(0,WORKAREA)                          
         GOTO1 DATCON,DMCB,(0,PACKESEN+2),(0,WORKAREA+6)                        
*                                                                               
         MVI   MADERR,7            INVALID ESTIMATE DATE RANGE                  
         CLC   ESTSTRT,WORKAREA                                                 
         BNE   PACKIEX                                                          
         CLC   ESTEND,WORKAREA+6                                                
         BNE   PACKIEX                                                          
         MVC   TWEST,BEST                                                       
*                                                                               
         MVI   MADERR,8            INVALID STATION                              
         XC    DUB,DUB                                                          
         MVC   DUB(4),PACKNET                                                   
         MVI   DUB+4,C'N'                                                       
         GOTO1 VALISTA,DMCB,DUB       VALIDATE THE STATION                      
         BNE   PACKIEX                                                          
         MVC   TWNET,PACKNET                                                    
         MVC   TWMKT,BMKTSTA                                                    
*--VALIDATE THE DEMO BASE V(PH) I(MPRESSION)                                    
         MVI   MADERR,9            INVALID DEMO BASE                            
         MVI   TWBASE,C'V'                                                      
         CLI   PACKBASE,X'40'      CHECK FOR INPUT                              
         BNH   PACKI200                                                         
         CLI   PACKBASE,C'V'                                                    
         BE    PACKI200                                                         
         MVI   TWBASE,C'I'                                                      
         CLI   PACKBASE,C'I'                                                    
         BE    PACKI200                                                         
         B     PACKIEX                                                          
*--VALIDATE THE PACKAGE DEMOS AGAINST THE ESTIMATE RECORD DEMO LIST             
PACKI200 BAS   RE,SETDEMO                                                       
*                                                                               
*--VALIDATE PACKAGE COST                                                        
         MVI   MADERR,17                                                        
         GOTO1 CHKNUM,DMCB,(10,PACKCOST)                                        
         BNZ   PACKIEX                                                          
*                                                                               
         PACK  DUB,PACKCOST                                                     
         CVB   RE,DUB                                                           
         STCM  RE,15,TWPCOST                                                    
*--VALIDATE PACKAGE RECORD                                                      
         MVI   MADERR,18                                                        
         GOTO1 CHKNUM,DMCB,(3,PACKPCOD)                                         
         BNZ   PACKIEX                                                          
*                                                                               
         PACK  DUB,PACKPCOD                                                     
         CVB   RE,DUB                                                           
         STC   RE,TWPKG                                                         
*-SET SYSTEM TO UNIT FILE                                                       
         MVI   MADERR,10           PACKAGE RECORD NOT THERE                     
         BAS   RE,UNITSET          SET UP TO READ UNIT FILE                     
*                                                                               
         LA    R3,KEY                                                           
         USING NPRECD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,TWAGYMED                                                   
         MVC   NPKCLT,TWCLIENT                                                  
         MVC   NPKNET,TWNET                                                     
         MVC   NPKEST,TWEST                                                     
         MVC   NPKPACK,TWPKG                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   PACKIEX                                                          
*                                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   TWPKEY,KEY          SAVE THE PACKAGE KEY                         
*--CHECK FOR NO SHOW PACKAGE                                                    
         TM    NPAKSTAT,X'01'                                                   
         BO    PACKIEX                                                          
*--MOVE STATUS INFO                                                             
         MVC   TWSTATUS,NPAKSTAT                                                
         OI    TWSTATUS,X'20'      SET LOCKED STATUS                            
*--MOVE DAYPART INFO                                                            
         MVC   TWDYPT,NPAKDP                                                    
*--MOVE SREP INFO                                                               
         MVC   TWSREP,NPAKSREP                                                  
*--MOVE UNIVERSE INFO                                                           
         MVC   TWUNIV,NPAKUNCD                                                  
*--MOVE HUT TYPE                                                                
         MVC   TWHUTTYP,NPAKHTYP                                                
*--MOVE FILTER INFO                                                             
         XC    TWFILTER,TWFILTER                                                
         GOTO1 GETELEM,DMCB,8      GET ELEMENT (X'08')                          
         BNE   *+10                                                             
         MVC   TWFILTER,0(R6)                                                   
*--CHECK MASTER ALLOCATION AGAINST PACKAGE RECORD                               
*--IF MASTER ALLOCATION NOT INPUTTED DEFAULT TO PACKAGE                         
*--RECORDS MASTER ALLOCATION.                                                   
         CLI   TWMSTPRD,0                                                       
         BNE   *+10                                                             
         MVC   TWMSTPRD,NPAKMAST                                                
*                                                                               
         MVI   MADERR,11           MASTER PRODUCT DOES NOT MATCH PKG            
         CLC   NPAKMAST,TWMSTPRD                                                
         BNE   PACKIEX                                                          
*--CHECK COST AGAINST PACKAGE RECORD                                            
         MVI   MADERR,12           COST DOES NOT MATCH PKG                      
         CLC   NPAKCOST,TWPCOST                                                 
*        BNE   PACKIEX                                                          
*--SEE IF PACKAGE HAS UNITS UNDER IT                                            
*        CLI   PACKOVR,C'Y'        BYPASS UNITS UNDER PACKAGE EDIT              
*        BE    PACKI280                                                         
         MVI   MADERR,14                                                        
         TM    NPAKCNTL,X'20'                                                   
         BZ    PACKIEX                                                          
*--CHECK DEMO BASE AGAINST PACKAGE RECORD                                       
PACKI280 MVI   MADERR,13           DEMO BASE DOES NOT MATCH PKG                 
         TM    NPAKCNTL,X'40'                                                   
         BZ    PACKI300                                                         
         CLI   TWBASE,C'I'                                                      
         BE    PACKIGD                                                          
         B     PACKIEX                                                          
*                                                                               
PACKI300 CLI   TWBASE,C'V'                                                      
         BE    PACKIGD                                                          
         B     PACKIEX                                                          
*                                                                               
PACKIGD  MVI   MADERR,0                                                         
*                                                                               
PACKIEX  B     XIT                                                              
         DROP  R2,R3                                                            
*                                                                               
RATETAB  DC    CL9'2F8C9WYY '                                                   
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    VALIDATES AND CONVERTS THE DEMOS INTO 3 BYTE                               
*    FORMAT.  THEN COMPARES THE PACKAGE DEMOS TO THE                            
*    ESTIMATE RECORD DEMOS, AND REMOVES THE DEMOS THAT                          
*    ARE NOT ON BOTH LISTS.                                                     
*                                                                               
SETDEMO  NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT0EIN01,R2         PACKAGE HEADER DSECT                         
*                                                                               
         XC    WORKAREA(200),WORKAREA                                           
*                                                                               
         LA    RE,WORKAREA+8                                                    
         LA    RF,PACKDEMS                                                      
         LA    R1,19                                                            
*                                                                               
STDM020  CLI   0(RF),X'40'                                                      
         BNH   STDM100                                                          
         MVC   0(7,RE),0(RF)                                                    
         CLI   0(RE),C'P'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C'V'          CHANGE PEOPLE TO VIEWERS                     
*                                                                               
         LA    R6,7                                                             
STDM040  CLI   0(RE),X'40'                                                      
         BNH   STDM060                                                          
         LA    RE,1(RE)                                                         
         BCT   R6,STDM040                                                       
*                                                                               
STDM060  LA    RF,7(RF)                                                         
         CLI   0(RF),X'40'                                                      
         BNH   STDM100                                                          
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         BCT   R1,STDM020                                                       
*--CALCULATE NUMBER OF DEMO FIELDS                                              
STDM100  MVI   0(RE),X'FF'                                                      
*                                                                               
         LA    RE,20                                                            
         SR    RE,R1                                                            
         LTR   RE,RE                                                            
         BZ    XIT                 NO DEMOS PROCESSED EXIT                      
*                                                                               
         STCM  RE,1,NFLDS                                                       
*                                                                               
*--CALCULATE THE LENGTH OF THE DEMOS                                            
         LA    RE,WORKAREA+8                                                    
         SR    RF,RF                                                            
STDM120  CLI   0(RE),X'FF'                                                      
         BE    STDM140                                                          
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         B     STDM120                                                          
*                                                                               
STDM140  STCM  RF,1,WORKAREA+5     FIELD LENGTH                                 
         LA    RF,8(RF)                                                         
         STCM  RF,1,WORKAREA                                                    
         MVI   0(RE),0                                                          
*                                                                               
*--CALL DEMOVAL CONVERT TO 3 BYTE FORMAT                                        
         XC    TWDEMOS,TWDEMOS                                                  
         LA    R6,DBDEMOB                                                       
         USING DEMOD,R6                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVC   DBFILE,=C'NTI'                                                   
         GOTO1 DEMOVAL,DMCB,(0,WORKAREA),(19,TWDEMOS),(0,(R6))                  
*                                                                               
         CLI   TWDEMOS,X'FF'       ANY DEMOS                                    
         BE    XIT                 NO EXIT                                      
*                                                                               
*--REMOVE DEMOS FROM TWDEMOS THAT ARE NOT ON THE ESTIMATE                       
         LA    RE,TWDEMOS                                                       
STDM160  LA    RF,ESTDEM25                                                      
         LA    R1,24                                                            
*                                                                               
STDM180  OC    0(3,RF),0(RF)       END OF EST LIST                              
         BZ    STDM200                                                          
         CLC   0(1,RE),0(RF)                                                    
         BNE   STDM190                                                          
         CLC   2(1,RE),2(RF)                                                    
         BE    STDM220                                                          
STDM190  LA    RF,3(RF)                                                         
         BCT   R1,STDM180                                                       
STDM200  XC    0(3,RE),0(RE)       WIPE THAT DEMO OUT                           
*                                                                               
STDM220  LA    RE,3(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   STDM160                                                          
*                                                                               
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    BUILDS THE UNIT AND THE PROGRAM RECORDS.                                   
*                                                                               
PROGINFO NTR1                                                                   
         L     R2,ADATA            A(ITEM)                                      
         USING CT0EIN02,R2                                                      
*                                                                               
         BAS   RE,CALCPROG         VALIDATE THE PROGRAM DATA                    
         CLI   MADERR,0            ANY ERRORS FOUND                             
         BE    PRGI020                                                          
*                                                                               
         GOTO1 WRITERR,DMCB,(C'P',DUB)  WRITE PROGRAM ERROR                     
         B     XIT                                                              
*                                                                               
PRGI020  MVI   MADERR,99                                                        
         GOTO1 WRITERR,DMCB,(C'C',DUB)  PASS THE PROGRAM CODE                   
         BAS   RE,BLDUNITS         BUILD A BASIC UNIT RECORD                    
         BAS   RE,ADDUNITS         CALCULATE THE LINE NUMBERS                   
*                                  WRITE THE UNIT RECORDS                       
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    FINDS A PROGRAM RECORD THAT MATCHES ON PROGRAM NAME,                       
*    PROGRAM ROTATION, AND PROGRAM TIME. IF A MATCH IS                          
*    THE ROUTINE CHECKS TO SEE THAT THE BUYS CAN FIT ON                         
*    THE PROGRAM. IF THE BUYS CAN FIT THE ROUTINE IS OVER.                      
*    IF THE BUYS CAN'T FIT A NEW PROGRAM RECORD IS CREATED.                     
*                                                                               
*   INPUT=R2 POINTS TO MAD PROGRAM RECORD                                       
*                                                                               
CALCPROG NTR1                                                                   
*                                                                               
*--SAVE PROGRAM NAME                                                            
         MVC   TWPRNAME,PROGPNAM                                                
*--SAVE ADU FLAG                                                                
         MVC   TWADU,PROGADU                                                    
*--CALCULATE PROGRAM LENGTH                                                     
         MVI   MADERR,30                                                        
         GOTO1 CHKNUM,DMCB,(3,PROGLEN)                                          
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB,PROGLEN                                                      
         CVB   RE,DUB                                                           
         STCM  RE,1,TWPLEN                                                      
*                                                                               
         CLI   TWPLEN,0            ZERO LENGTH INVALID                          
         BE    XIT                                                              
*--CALCULATE PROGRAM COST                                                       
         MVI   MADERR,31                                                        
         GOTO1 CHKNUM,DMCB,(10,PROGRATE)                                        
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB,PROGRATE                                                     
         CVB   RE,DUB                                                           
         STCM  RE,15,TWPRCST                                                    
*--CALCULATE HOMES IMPRESSION                                                   
         MVI   MADERR,32                                                        
         GOTO1 CHKNUM,DMCB,(10,PROGHIMP)                                        
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB,PROGHIMP                                                     
         CVB   RE,DUB                                                           
         STCM  RE,15,TWHOMES                                                    
*                                                                               
*--VALIDATE THE PROGRAM COUNT                                                   
         TM    PROGCNT,X'F0'                                                    
         BNO   CALCP020                                                         
*                                                                               
         PACK  DUB,PROGCNT                                                      
         CVB   RE,DUB                                                           
         STCM  RE,01,TWPRCT                                                     
         CLI   TWPRCT,0                                                         
         BNE   *+8                                                              
CALCP020 MVI   TWPRCT,192                                                       
*                                                                               
*--VALIDATE THE PROGRAM DEMOS                                                   
         MVI   MADERR,33                                                        
         GOTO1 CHKNUM,DMCB,(133,PROGDEMS)                                       
         BNZ   XIT                                                              
*                                                                               
*--CALCULATE PROGRAM CODE                                                       
         MVI   TWPRCODE+3,X'FF'                                                 
         XC    TWPRCODE(3),TWPRCODE                                             
         LA    RE,TWPRNAME                                                      
         LA    RF,TWPRCODE                                                      
         LA    R1,16                                                            
*                                                                               
*--FIRST AREA CHECKS FOR BYPASS NAMES                                           
CALCP030 LA    R4,NAMETAB                                                       
CALCP050 CLI   0(R4),X'FF'         END OF NAME TABLE                            
         BE    CALCP100            YES CHECK FOR VOWELS                         
         ZIC   R6,8(R4)            MOVE IN LENGTH OF COMPARE                    
         EX    R6,NAMECOMP                                                      
         BNE   CALCP070                                                         
         LA    R6,1(R6)                                                         
         AR    RE,R6               BUMP PROGRAM NAME TO NEXT WORD               
         SR    R1,R6               SUBTRACT LOOP COUNT BY WORD BUMP             
         LTR   R1,R1                                                            
         BZ    CALCP260            END OF LITERAL EXIT ROUTINE                  
         BP    CALCP030            CHECK NEXT WORD                              
         DC    H'0'                WENT PAST LITERAL                            
CALCP070 LA    R4,9(R4)                                                         
         B     CALCP050                                                         
*                                                                               
*--THIS AREA CHECKS FOR TO BYPASS VOWELS AND SPECIAL VALUES                     
CALCP100 LA    R4,VOWELLST                                                      
CALCP120 CLI   0(RE),X'40'         CHECK FOR WORD BREAK                         
         BNE   CALCP140                                                         
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         BCTR  R1,0                SUBTRACT FROM COUNT                          
         LTR   R1,R1                                                            
         BZ    CALCP260            END OF LITERAL EXIT ROUTINE                  
         B     CALCP030            CHECK AGAINST WORD TABLE                     
*                                                                               
CALCP140 CLI   0(RE),X'C1'         CHECK CHECK CHARACTER FOR ALPHA/NUM          
         BL    CALCP180                                                         
         CLI   0(RE),X'F9'                                                      
         BH    CALCP180                                                         
*                                                                               
CALCP160 CLI   0(R4),X'FF'         END OF VOWEL TABLE                           
         BE    CALCP200            MOVE LETTER TO CODE                          
         CLC   0(1,RE),0(R4)                                                    
         BE    CALCP180            LETTER IS A VOWEL, BYPASS                    
         LA    R4,1(R4)                                                         
         B     CALCP160                                                         
*                                                                               
CALCP180 LA    RE,1(RE)                                                         
         BCT   R1,CALCP100                                                      
         B     CALCP260                                                         
*                                                                               
CALCP200 CLI   0(RF),X'FF'                                                      
         BE    CALCP260            CODE FIELD IS FILLED                         
         MVC   0(1,RF),0(RE)       MOVE NEXT LETTER OF CODE IN                  
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,CALCP100                                                      
*                                                                               
*--THE LITERAL PORTION OF THE PROGRAM CODE MUST BE                              
*--1 POSITIONS LONG. IF IT IS NOT AN ERROR CODE IS                              
*--AND THE PROGRAM BYPASSES THE UNITS UNDER THAT CODE.                          
CALCP260 MVC   TWPRCODE+3(3),=XL3'404040'                                       
         CLI   TWPRCODE,X'00'                                                   
         BNE   CALCP280                                                         
         MVI   MADERR,34           INVALID PROGRAM NAME                         
         B     XIT                                                              
*                                                                               
CALCP280 MVC   TWPRCODE+3(3),=XL3'F0F0F1'                                       
         CLI   TWPRCODE+2,0                                                     
         BNE   CALCP300                                                         
         MVI   TWPRCODE+2,X'F0'                                                 
         CLI   TWPRCODE+1,0                                                     
         BNE   CALCP300                                                         
         MVI   TWPRCODE+1,X'F0'                                                 
*                                                                               
*--CONVERT THE MAD RECORDS ROTATION INTO PROGRAM FORMAT                         
CALCP300 XC    TWPRROT(4),TWPRROT  CLEAR THE DAY FIELDS                         
         LA    R4,ROTTABLE                                                      
         LA    RE,PROGROTN                                                      
CALCP320 CLI   0(R4),X'FF'                                                      
         BE    CALCP360                                                         
         CLI   0(RE),C'Y'                                                       
         BNE   CALCP340                                                         
         OC    TWPRROT,0(R4)                                                    
         NI    TWPRROTN,X'F0'      CLEAR END DAT NUMBER                         
         OC    TWPRROTN,2(R4)      END DAY NUMBER                               
         TM    TWPRROTN,X'F0'                                                   
         BNZ   CALCP340                                                         
         NI    TWPRROTN,X'0F'      CLEAR START DAY NUMBER                       
         OC    TWPRROTN,1(R4)      START DAY NUMBER                             
         MVC   TWDAYNO,2(R4)      START DAY NUMBER (NUMERIC)                    
         MVC   TWDAYHEX,0(R4)     START DAY NUMBER (HEX)                        
CALCP340 LA    R4,3(R4)                                                         
         LA    RE,1(RE)                                                         
         B     CALCP320                                                         
*                                                                               
*--CONVERT THE START AND END TIMES TO BINARY                                    
*                                                                               
CALCP360 MVI   MADERR,35                                                        
         GOTO1 CHKNUM,DMCB,(4,PROGSTIM)                                         
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB(8),PROGSTIM(4)  START TIME                                   
         CVB   RE,DUB                                                           
         STCM  RE,3,TWPRSTIM                                                    
*                                                                               
         MVI   MADERR,35                                                        
         GOTO1 CHKNUM,DMCB,(4,PROGETIM)                                         
         BNZ   XIT                                                              
*                                                                               
         PACK  DUB(8),PROGETIM(4)  START TIME                                   
         CVB   RE,DUB                                                           
         STCM  RE,3,TWPRETIM                                                    
*-CHECK START AND END FOR GREATER THEN 2400                                     
         MVI  MADERR,36                                                         
         CLC  TWPRETIM,=XL2'0960'                                               
         BH   XIT                                                               
*                                                                               
         CLC  TWPRSTIM,=XL2'0960'                                               
         BH   XIT                                                               
*-CHECK START AND END FOR ZERO                                                  
         MVI  MADERR,37                                                         
         OC   TWPRETIM,TWPRETIM                                                 
         BZ   XIT                                                               
*                                                                               
         OC   TWPRSTIM,TWPRSTIM                                                 
         BZ   XIT                                                               
*--GET START QUARTER HOUR                                                       
         BAS   RE,GETSQH                                                        
*                                                                               
*--READ A PROGRAM RECORD SEE IF IT ALREADY EXISTS                               
*                                                                               
         BAS   RE,READPROG         READ HIGH CALL FOR PROGRAM                   
         CLC   KEY(8),KEYSAVE                                                   
         BNE   CALCP540                                                         
         B     CALCP440                                                         
*                                                                               
CALCP400 GOTO1 SEQ                                                              
*                                                                               
CALCP420 CLC   KEY(8),KEYSAVE                                                   
         BNE   CALCP500                                                         
*                                                                               
*--IF RECORD DOES EXIST CHECK UPLOAD BIT, NAME, DAY,ROTATION TO SEE IF          
*--THIS RECORD MATCHES THE UNITS TO BE ADDED.                                   
CALCP440 L     R3,AIO                                                           
         USING NPGRECD,R3                                                       
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 GETELEM,DMCB,146    GET ELEMENT (X'92')                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGEL92,R6                                                       
*                                                                               
         CLI   NPGUPLD,C'Y'                                                     
         BNE   CALCP400                                                         
         MVC   TWPRCODE+3(3),KEY+8  LINE COUNT NUMBER                           
*                                                                               
         OC    NPGNAME,=16X'40'                                                 
         CLC   NPGNAME,TWPRNAME                                                 
         BNE   CALCP400                                                         
*                                                                               
         CLC   TWPRROT,NPGROT                                                   
         BNE   CALCP400                                                         
*                                                                               
         CLC   TWPRSTIM(4),NPGTIME                                              
         BNE   CALCP400                                                         
         B     CALCP700            YES VALID PROGRAM RECORD FOUND               
*                                                                               
         DROP  R3,R6                                                            
*                                                                               
*--CREATE A NEW PROGAM RECORD                                                   
*--TAKE LAST PROGRAM LINE COUNT IN FULL, ADD 1                                  
*--RELOAD BACK INTO FULL USE THAT AS THE NEW                                    
*--PROGRAM LINE COUNT.                                                          
*                                                                               
CALCP500 BAS   RE,SPOTSET                                                       
         PACK  DUB(2),TWPRCODE+3(3) CREATE A NEW PROGRAM LINE COUNT             
         AP    DUB(2),=PL1'1'                                                   
         UNPK  TWPRCODE+3(3),DUB(2)                                             
         OI    TWPRCODE+5,X'F0'                                                 
*                                                                               
*--BUILD NEW PROGRAM CODE                                                       
CALCP540 L     R3,AIO2                                                          
         XCEF  (R3),2000                                                        
         USING NPGRECD,R3                                                       
*                                                                               
         MVC   NPGKTYP,=XL2'0D20'                                               
         MVC   NPGKAM,TWAGYMED                                                  
         MVC   NPGKNET,TWMKT                                                    
         MVC   NPGKPROG,TWPRCODE                                                
         MVC   NPGKEND,=XL2'F19F'  DEC31/20                                     
*                                                                               
         MVC   NPGRLEN,=XL2'0021'                                               
*                                                                               
         MVC   NPGMAINL(2),=XL2'0108'                                           
         GOTO1 DATCON,DMCB,(5,0),(3,NPGACTD)                                    
         MVI   NPGACT,C'A'                                                      
*                                                                               
*--BUILD 5D ELEMENT (FOR DEMOS)                                                 
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUBKD,RE                                                         
*                                                                               
         MVC   NUBKEL(2),=XL2'5D07'                                             
         MVC   NUBKFMS(3),=CL3'EVN'                                             
         MVC   NUBKBOOK(2),=XL2'580E'                                           
         DROP  RE                                                               
*                                                                               
         MVC   AIO,AIO2            ADD X'5D' ELEMENT                            
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
*                                                                               
*--BUILD 92 ELEMENT                                                             
         XC    WORKAREA(100),WORKAREA                                           
         LA    RE,WORKAREA                                                      
         USING NPGEL92,RE                                                       
*                                                                               
         MVC   NPGELEM(2),=XL2'9250'                                            
         MVI   NPGDAY,X'7F'                                                     
         MVC   NPGTIME,TWPRSTIM                                                 
         MVC   NPGNAME,TWPRNAME                                                 
         MVC   NPGROT,TWPRROT                                                   
         MVC   NPGROTNO,TWPRROTN                                                
         MVI   NPGDAYNO,X'17'                                                   
         MVI   NPGUPLD,C'Y'        SET UPLOAD SWITCH TO YES                     
         DROP  RE                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
*--WRITE THE RECORD OUT                                                         
         GOTO1 ADDREC                                                           
*                                                                               
         B     CALCPEX                                                          
*                                                                               
*--CHECK TO SEE THAT ALL UNITS REQUESTED HAVE THE                               
*--ROOM TO BE ADDED UNDER THE PROGRAM.                                          
CALCP700 BAS   RE,UNITSET          SET UP TO READ UNIT FILE                     
*                                                                               
         LA    R4,PROGUNIT                                                      
*                                                                               
         LA    R3,KEY                                                           
         USING NURECD,R3                                                        
*                                                                               
CALCP720 CLI   0(R4),X'40'                                                      
         BNH   CALCPEX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,TWAGYMED                                                  
         MVC   NUKPCLT,TWCLIENT                                                 
         MVC   NUKPNET,TWNET                                                    
         MVC   NUKPPROG,TWPRCODE                                                
         MVC   NUKPEST,TWEST                                                    
*-SET DATE TO FIRST DAY OF ROTATION                                             
         MVC   DUB(6),2(R4)                                                     
         ZIC   R6,TWDAYNO                                                       
         BCTR  R6,0                                                             
         GOTO1 ADDAY,DMCB,DUB,DUB,(R6)                                          
*-CONVERT THE WEEK DATE                                                         
         GOTO1 DATCON,DMCB,(0,DUB),(2,NUKPDATE)                                 
*-CONVERT THE LINE NUMBER                                                       
         CLC   8(3,R4),PROGCNT     COMP TO MAXIMUM PROGRAM COUNT                
         BH    CALCP780            TOO MANY LINES BEING UPLOADED                
         B     CALCP740                                                         
*                                  BYPASS THAT ELEMENT                          
CALCP740 PACK  DUB(8),8(3,R4)                                                   
         CVB   RE,DUB                                                           
         ZIC   RF,TWPRCT           MAXIMUM PROGRAM COUNT ALLOWED                
         LA    RF,1(RF)                                                         
         SR    RF,RE                                                            
         LTR   RF,RF                                                            
         BP    CALCP760                                                         
         B     CALCP800                                                         
*                                                                               
         STC   RF,NUKPSUB                                                       
*-READ UNIT 84 KEY IF FOUND THIS MEANS THERE IS                                 
*-NOT ENOUGH ROOM ON THE PROGRAM TO ADD THE UNITS.                              
*-AND A NEW PROGRAM RECORD WILL BE CREATED.                                     
CALCP760 STCM  RF,1,NUKPSUB                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    CALCP800                                                         
CALCP780 LA    R4,15(R4)           GO TO NEXT UNIT ELEMENT                      
         B     CALCP720                                                         
*                                                                               
*-NO ROOM ON THIS PROGRAM TO ADD THE UNITS                                      
*-MUST GO BACK AND FIND ANOTHER PROGRAM TO                                      
*-ADD THESE UNITS TO.                                                           
CALCP800 BAS   RE,READPROG                                                      
         B     CALCP400                                                         
*                                                                               
*-ALL UNITS PROGRAMS VALID XIT                                                  
CALCPEX  MVI   MADERR,0                                                         
         B     XIT                                                              
*                                                                               
NAMECOMP CLC   0(0,RE),0(R4)       FOR TEST ON NAME TABLE                       
         DROP  R3                                                               
*                                                                               
NAMETAB  DC    CL8'THE     ',XL1'03'                                            
         DC    X'FF'                                                            
VOWELLST DC    CL5'AEIOU'                                                       
         DC    X'FF'                                                            
ROTTABLE DC    XL3'401001'         MON                                          
         DC    XL3'202002'         TUE                                          
         DC    XL3'103003'         WED                                          
         DC    XL3'084004'         THU                                          
         DC    XL3'045005'         FRI                                          
         DC    XL3'026006'         SAT                                          
         DC    XL3'017007'         SUN                                          
         DC    X'FF'                                                            
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    BUILDS THE BASIC INFORMATION NEEDED TO BUILD                               
*    A UNIT RECORD.                                                             
*                                                                               
*   INPUT=R2 POINTS TO MAD PROGRAM RECORD                                       
*                                                                               
BLDUNITS NTR1                                                                   
*                                                                               
*                                                                               
         L     R3,AIO2                                                          
         USING NURECD,R3                                                        
         XCEF  (R3),2000                                                        
         BAS   RE,UNITSET          SET UP TO READ UNIT FILE                     
*--04 KEY                                                                       
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,TWAGYMED                                                   
         MVC   NUKCLT,TWCLIENT                                                  
         MVC   NUKTIME,TWQTRHR                                                  
         MVC   NUKNET,TWNET                                                     
         MVC   NUKPROG,TWPRCODE                                                 
         MVC   NUKEST,TWEST                                                     
         MVC   NUKDP,TWDYPT                                                     
         MVC   NUSREP,TWSREP                                                    
         MVC   NUUNCODE,TWUNIV                                                  
         MVC   NUHUTTYP,TWHUTTYP                                                
*                                                                               
         MVI   NURLEN+1,X'6C'      RECORD LENGTH 108                            
*--SET 01 ELEMENT                                                               
         MVC   NUMAINEL(2),=XL2'0150'                                           
         MVC   NUPACK,TWPKG                                                     
         MVC   NUPACKST,TWSTATUS                                                
*--CHECK FORMASTER ALLOCATION PRODUCT                                           
         CLI   TWMSTPRD,0                                                       
         BE    *+10                                                             
         MVC   NUPRD,TWMSTPRD                                                   
*                                                                               
         MVC   NUPROGNM,TWPRNAME                                                
         MVC   NULEN,TWPLEN                                                     
         MVC   NUDAY,TWDAYHEX                                                   
         MVC   NUTIME,TWPRSTIM                                                  
         OI    NUUNITST,X'20'       NET COST INPUT BIT                          
         CLI   TWADU,C'Y'          BYPASS COST IF ADU UNIT                      
         BE    *+10                                                             
         MVC   NUACTUAL,TWPRCST                                                 
         BAS   RE,CALCOST2          SEE IF SECOND COST NEEDED                   
BLDUN040 OI    NUACTWHY,X'80'       NEW BUY                                     
         MVC   NUMARKET,TWMKT                                                   
         MVC   NUALPHA,SIGNON2C                                                 
*                                                                               
*--BUILD 02 ELEMENT                                                             
         XC    WORKAREA(50),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUSDRD,RE                                                        
*                                                                               
         MVC   NUSDREL(2),=XL2'0214'                                            
         OI    NUSDST3,X'04'       CABLE UPLOAD UNIT                            
         MVC   NUSTATYP,TWPSTTYP                                                
         MVC   NUSDROT,TWPRROT                                                  
         MVC   NUPOSTYP,TWPSTTYP                                                
         MVC   NUSDSRT,TWRATE                                                   
         MVC   NUSDRTCV,TWCVRGE                                                 
         CLI   TWADU,C'Y'                                                       
         BNE   *+8                                                              
         OI    NUSDST3,X'02'       SET ADU FLAG                                 
*                                                                               
         MVC   AIO,AIO2            ADD X'02' ELEMENT                            
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
         DROP  RE                                                               
*                                                                               
*--BUILD 08 ELEMENT PACKAGE FILTER                                              
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
*                                                                               
         CLI   TWFILTER,0                                                       
         BE    BLDUN100                                                         
         MVC   WORKAREA(11),TWFILTER                                            
*                                                                               
         MVC   AIO,AIO2            ADD X'08' ELEMENT                            
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
*                                                                               
*--BUILD 21 ELEMENT (FOR TRAFFIC)                                               
BLDUN100 XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUCMLEL,RE                                                       
*                                                                               
         MVC   NUCMLEID(2),=XL2'2134'                                           
*                                                                               
         MVC   AIO,AIO2            ADD X'21' ELEMENT                            
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
         DROP  RE                                                               
*                                                                               
*--BUILD 5D ELEMENT (FOR DEMOS)                                                 
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUBKD,RE                                                         
*                                                                               
         MVC   NUBKEL(2),=XL2'5D07'                                             
         MVC   NUBKBOOK(2),=XL2'580E'                                           
         MVC   NUBKFMS(3),=CL3'EIN'                                             
         CLI   TWBASE,C'V'                                                      
         BNE   *+10                                                             
         MVC   NUBKFMS(3),=CL3'EVN'                                             
*                                                                               
         MVC   AIO,AIO2            ADD X'5D' ELEMENT                            
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
         DROP  RE                                                               
*                                                                               
*--BUILD 99 ELEMENT                                                             
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUACTD,RE                                                        
*                                                                               
         MVC   NUACTEL(2),=XL2'990C'                                            
         GOTO1 DATCON,DMCB,(5,0),(3,NUACTADT)                                   
*                                                                               
         MVC   AIO,AIO2            ADD X'99' ELEMENT                            
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
         DROP  RE                                                               
*                                                                               
*--BUILD DD ELEMENT                                                             
         XC    WORKAREA(50),WORKAREA                                            
         USING NUOVD,RE                                                         
         LA    R4,TWDEMOS                                                       
         LA    R5,PROGDEMS                                                      
         LA    R6,19                                                            
*                                                                               
BLDUN300 LA    RE,WORKAREA                                                      
         XC    WORKAREA(50),WORKAREA                                            
         CLI   0(R4),X'FF'         END OF LIST                                  
         BE    BLDUN400                                                         
*                                                                               
         MVC   NUOVEL(2),=XL2'DD0C'                                             
         CLI   1(R4),0             SEE IF DEMO SHOULD BE USED                   
         BE    BLDUN360                                                         
         PACK  DUB,0(7,R5)                                                      
         CVB   RF,DUB                                                           
         STCM  RF,15,NUOVVAL       MOVE OUT THE VALUE                           
         MVC   NUOVCAT,0(R4)       THE CATEGORY NUMBER                          
         CLI   NUOVCAT,0           CHECK FOR NAD                                
         BE    *+8                                                              
         OI    NUOVFLG,X'80'                                                    
         MVC   NUOVNUM,2(R4)       DEMO NUMBER                                  
         MVI   NUOVMOD,C'T'                                                     
         MVI   NUOVPRE,X'42'                                                    
         CLI   TWBASE,C'I'         CHECK FOR IMPRESSION BASE                    
         BE    BLDUN340                                                         
         MVI   NUOVMOD,C'V'        SET VPH DEFAULTS                             
         MVI   NUOVPRE,X'40'                                                    
*                                                                               
BLDUN340 MVC   AIO,AIO2            ADD X'DD' ELEMENT                            
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
*        BAS   RE,BLDIMP                                                        
*                                                                               
BLDUN360 LA    R4,3(R4)                                                         
         LA    R5,7(R5)                                                         
         BCT   R6,BLDUN300                                                      
*--BUILD HOMES IMPRESSION DEMO ELEMENT                                          
BLDUN400 CLI   TWBASE,C'V'                                                      
         BE    BLDUN450                                                         
         LA    RE,WORKAREA                                                      
         XC    WORKAREA(50),WORKAREA                                            
*                                                                               
         MVC   NUOVEL(2),=XL2'DD0C'                                             
         MVC   NUOVCAT(3),=XL3'00E301'                                          
         MVI   NUOVPRE,X'42'                                                    
         MVC   NUOVVAL,TWHOMES                                                  
*                                                                               
         MVC   AIO,AIO2            ADD X'DD' ELEMENT                            
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
*                                                                               
BLDUN450 B     XIT                                                              
         DROP  RE,R3                                                            
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    SEE IF A SECOND COST PERCENTAGE WAS SET UP ON THE                          
*    CLIENT OR ESTIMATE RECORD AND IF IT WAS CALCULATES THE                     
*    SECOND COST AND INSERTS IT IN THE ASSIGNED COST FIELD                      
*                                                                               
*                                                                               
CALCOST2 NTR1                                                                   
         L     R3,AIO2                                                          
         USING NURECD,R3                                                        
         MVC   FULL,TWCLIC2        MOVE CLIENT PCT.                             
         OC    TWESTC2,TWESTC2     WAS ESTIMATE LEVEL COST INPUTTED             
         BZ    *+10                                                             
         MVC   FULL,TWESTC2                                                     
         OC    FULL,FULL           WAS ANY COST PCT INPUTTED                    
         BZ    XIT                 NO, EXIT                                     
*                                                                               
         NI    NUUNITST,X'F7'                                                   
         XC    NUASSIGN,NUASSIGN                                                
         TM    FULL,X'80'           WAS COST PCT SET TO ZERO                    
         BZ    CL2CS50              NO CALCULATE                                
         OI    NUUNITST,X'08'                                                   
         B     XIT                                                              
*                                                                               
CL2CS50  ZAP   WORK(16),=PL1'0'                                                 
         ICM   R1,15,NUACTUAL       COST                                        
         CVD   R1,WORK+8                                                        
         ICM   R1,15,FULL           PERCENTAGE X.XXXXXX                         
         CVD   R1,DUB                                                           
         MP    WORK(16),DUB+4(4)    MULT COST BY PERCENTAGE                     
         AP    WORK(16),=PL4'500000'  ROUND                                     
         DP    WORK(16),=PL4'1000000'                                           
         CVB   R1,WORK+4                                                        
         STCM  R1,15,NUASSIGN                                                   
         OC    NUASSIGN,NUASSIGN                                                
         BNZ   XIT                                                              
         OI    NUUNITST,X'08'                                                   
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    BUILDS A CORRESPONDING IMPRESSION OVERRIDE ELEMENT                         
*    FOR EVERY VPH ELEMENT.                                                     
*                                                                               
*   INPUT WORKAREA = VPH OVERIDE ELEMENT                                        
*                                                                               
BLDIMP   NTR1                                                                   
         LA    RE,WORKAREA                                                      
         USING NUOVD,RE                                                         
*                                                                               
         CLI   NUOVMOD,C'V'        CHECK FOR VPH                                
         BNE   BLDIMPEX                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R1,15,TWHOMES                                                    
         ICM   RF,15,NUOVVAL                                                    
*                                                                               
         MR    R0,RF               HOMES*VPH                                    
         M     R0,=F'10'                                                        
         D     R0,=F'1000'                                                      
         A     R1,=F'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
*                                                                               
         STCM  R1,15,NUOVVAL                                                    
         MVI   NUOVMOD,C'T'                                                     
         MVI   NUOVPRE,X'42'                                                    
*                                                                               
         MVC   AIO,AIO2            ADD X'DD' ELEMENT                            
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
*                                                                               
BLDIMPEX B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    FINDS THE UNIT WITH THE HIGHEST LINE NUMBER AND                            
*    STARTS ADDING A UNIT RECORDS THEREAFTER. IT WILL                           
*    SET AN INDICATOR SAYING THE UNIT WAS ADDED THROUGH                         
*    THE UPLOAD.                                                                
*                                                                               
*   INPUT=R2 POINTS TO MAD PROGRAM RECORD                                       
*                                                                               
ADDUNITS NTR1                                                                   
*                                                                               
         LA    R4,PROGUNIT         START OF BUY ELEMENTS                        
         USING PROGDET,R4                                                       
*                                                                               
ADDUN050 XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING NURECD,R3                                                        
*--BUILD THE BUY KEY                                                            
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,TWAGYMED                                                  
         MVC   NUKPCLT,TWCLIENT                                                 
         MVC   NUKPNET,TWNET                                                    
         MVC   NUKPPROG,TWPRCODE                                                
*--CONVERT THE DATE                                                             
         MVI   MADERR,60           CHECK DATE FOR NUMERIC                       
         GOTO1 CHKNUM,DMCB,(8,PROGWEEK)                                         
         BNZ   ADDUNERR                                                         
*-CHECK DATE FOR MONDAY                                                         
         MVI   MADERR,61           CHECK DATE FOR NUMERIC                       
         MVC   DUB(6),2(R4)                                                     
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLI   0(R1),1                                                          
         BNE   ADDUNERR                                                         
*-SET DATE TO FIRST DAY OF ROTATION                                             
         ZIC   R6,TWDAYNO                                                       
         BCTR  R6,0                                                             
         GOTO1 ADDAY,DMCB,DUB,DUB,(R6)                                          
*-CHECK DATE WITHIN ESTIMATE RANGE                                              
         MVI   MADERR,67           CHECK DATE WITHIN ESTIMATE                   
         CLC   DUB(6),ESTSTRT                                                   
         BL    ADDUNERR                                                         
         CLC   DUB(6),ESTEND                                                    
         BH    ADDUNERR                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,DUB),(2,NUKPDATE)                                 
         MVC   TWPDATE,NUKPDATE    SAVE THE BUY DATE                            
*                                                                               
         MVC   NUKPEST,TWEST                                                    
         MVI   NUKPSUB,1                                                        
*                                                                               
         L     R3,AIO2                                                          
*--CREATE UNIVERSE ELEMENT                                                      
         BAS   RE,BLDUNIV                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 DELELEM,DMCB,49     DELETE ELEMENT (X'31')                       
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
*--CALCULATE HOMES RATING IF VPH BASED                                          
         CLI   TWBASE,C'I'                                                      
         BE    ADDUN060                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RF,15,TWHOMES       HOMES IMPRESSION                             
         ICM   R1,7,WORKAREA+66   HOMES UNIVERSE                                
*                                                                               
         M     RE,=F'10000'                                                     
         DR    RE,R1                                                            
         STCM  RF,15,DUB                                                        
*--BUILD HOMES RATING DEMO ELEMENT                                              
         XC    WORKAREA(50),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUOVD,RE                                                         
*                                                                               
         MVC   NUOVEL(2),=XL2'DD0C'                                             
         MVC   NUOVCAT(3),=XL3'00D901'                                          
         MVI   NUOVPRE,X'82'                                                    
         SR    R0,R0                                                            
         L     R1,DUB                                                           
         A     R1,=F'5'                                                         
         D     R0,=F'10'                                                        
         STCM  R1,15,NUOVVAL                                                    
*                                                                               
         MVC   AIO,AIO2            ADD X'DD' ELEMENT                            
         MVC   FULL(4),=XL4'0000D901'                                           
         PRINT GEN                                                              
         GOTO1 SRCHDEL,DMCB,221,(4,FULL)   DEL ELEM (X'DD') FOR HOMES           
         PRINT NOGEN                                                            
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
         DROP  RE                                                               
*--BUILD 35 ELEMENT (FOR VPH)                                                   
         XC    WORKAREA(70),WORKAREA                                            
         LA    RE,WORKAREA                                                      
         USING NUEHD,RE                                                         
*                                                                               
         MVC   NUEHEL(3),=XL3'350902'                                           
         SR    R0,R0                                                            
         L     R1,DUB                                                           
         A     R1,=F'5'                                                         
         D     R0,=F'10'                                                        
         STCM  R1,3,NUEHRTG                                                     
*                                                                               
         MVC   AIO,AIO2            ADD X'35' ELEMENT                            
         GOTO1 DELELEM,DMCB,53     DELETE ELEMENT (X'35')                       
         GOTO1 ADDELEM,DMCB,WORKAREA                                            
         DROP  RE                                                               
*                                                                               
ADDUN060 MVI   TWLNCT,1                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BNE   ADDUN160                                                         
         B     ADDUN120                                                         
*                                                                               
ADDUN070 GOTO1 SEQ                                                              
*                                                                               
ADDUN100 CLC   KEY(17),KEYSAVE                                                  
         BNE   ADDUN140                                                         
ADDUN120 MVC   KEYSAVE(18),KEY                                                  
         B     ADDUN070                                                         
*                                                                               
*--GET NEXT UNUSED SUB-LINE NUMBER                                              
ADDUN140 ZIC   RE,KEYSAVE+17                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,1,TWLNCT                                                      
*--CONVERT THE DATE                                                             
ADDUN160 MVC   NUKDATE,TWPDATE                                                  
*--CONVERT NUMBER OF UNITS AND NUMBER OF BILLBOARDS TO BINARY                   
         MVI   MADERR,62                                                        
         GOTO1 CHKNUM,DMCB,(3,PROGNUNT)                                         
         BNZ   ADDUNERR                                                         
*                                                                               
         PACK  DUB,PROGNUNT(3)                                                  
         CVB   RE,DUB                                                           
         STCM  RE,1,TWUNTCT                                                     
*                                                                               
         MVI   MADERR,63                                                        
         CLI   TWUNTCT,192                                                      
         BH    ADDUNERR                                                         
*                                                                               
*        MVI   MADERR,66                                                        
         CLI   TWUNTCT,0                                                        
         BE    ADDUN380                                                         
*                                                                               
         MVI   MADERR,64                                                        
         GOTO1 CHKNUM,DMCB,(2,PROGBLBD)                                         
         BNZ   ADDUNERR                                                         
*                                                                               
         PACK  DUB,PROGBLBD                                                     
         CVB   RE,DUB                                                           
         STCM  RE,1,TWBLBCT                                                     
*                                                                               
         MVI   MADERR,65                                                        
         CLC   TWBLBCT,TWUNTCT                                                  
         BH    ADDUNERR                                                         
*--UNLOCK THE UNIT FILE                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMUNLK'),=CL8'UNTFILE'                        
         BAS   RE,UNITSET                                                       
*--WRITE THE UNIT LOOP                                                          
*--R3 POINTS TO AIO2                                                            
         ZIC   R2,TWUNTCT          NUMBER OF UNITS                              
         ZIC   R5,TWBLBCT          NUMBER OF BILLBOARDS                         
         ZIC   R6,TWLNCT           START LINE NUMBER                            
*                                                                               
ADDUN300 STCM  R6,1,NUKSUB                                                      
         STCM  R6,1,TWLNCT         CURRENT LINE COUNT                           
         CR    R2,R5                                                            
         BNE   ADDUN360                                                         
*--BUILD BILLBOARD RECORD                                                       
*                                                                               
         GOTO1 GETELEM,DMCB,33     GET ELEMENT (X'21')                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUCMLEID,R6                                                      
         OI    NUCMLFLG,X'04'                                                   
         DROP  R6                                                               
         ICM   R6,1,TWLNCT         RESET THE LINE COUNT                         
************************************                                            
ADDUN360 MVC   KEY(20),NUKEY                                                    
         OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
         OC    NURSTAT,TWCNTRL     SET POSTING TYPE IN CONTROL                  
         BAS   RE,WRITBREC                                                      
         LA    R6,1(R6)                                                         
         BCT   R2,ADDUN300                                                      
*                                                                               
         GOTO1 GETELEM,DMCB,33     GET ELEMENT (X'21')                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUCMLEID,R6                                                      
         MVI   NUCMLFLG,0                                                       
         DROP  R6                                                               
*                                                                               
ADDUN380 LA    R4,15(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    ADDUN050                                                         
*                                                                               
         MVI   MADERR,0                                                         
         B     ADDUNEX                                                          
*                                                                               
ADDUNEX  B     XIT                                                              
         DROP  R4                                                               
         SPACE 3                                                                
*--ROUTINE WRITES A RECORD TO THE PC TELLING IT                                 
*--THAT THERE IS AN ERROR IN THE UNIT PORTION OF                                
*--THE RECORD                                                                   
ADDUNERR GOTO1 WRITERR,DMCB,(C'U',DUB)                                          
         LA    R4,15(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    ADDUN050                                                         
         B     ADDUNEX                                                          
         EJECT                                                                  
*-THIS ROUTINE SETS UP THE SPOTFILE                                             
*-SETS UP THE PROGRAM KEY DOES A READ                                           
*-HIGH FOR THE PROGRAM RECORD.                                                  
READPROG NTR1                                                                   
         BAS   RE,SPOTSET          SETUP TO READ SPOT FILE                      
         LA    R3,KEY                                                           
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,TWAGYMED                                                  
         MVC   NPGKNET,TWMKT                                                    
         MVC   NPGKPROG(6),TWPRCODE                                             
*                                                                               
         GOTO1 HIGH                                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    CHECKS TO SEE IF ANY UNITS WRITTEN IF YES CHANGES THE CONTROL              
*    BIT TO SAY PACKAGE HAS UNITS UNDER IT.                                     
*                                                                               
PACKUPDT NTR1                                                                   
         CLI   WRITESW,C'Y'                                                     
         BNE   PACKUPEX                                                         
*                                                                               
         MVI   WRITESW,C'N'                                                     
*                                                                               
*--UNLOCK THE UNIT FILE                                                         
         BAS   RE,UNITSET                                                       
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
*                                                                               
         MVC   KEY(20),TWPKEY                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         USING NPRECD,R3                                                        
*                                                                               
         NI    NPAKCNTL,X'DF'      UNITS EXIST UNDER PACKAGE                    
         NI    NPAKCNTL,X'F7'      CABLE LOCK IS OFF                            
         OI    NPAKCNTL,X'10'      CABLE UPLOAD PACKAGE                         
         OI    NPAKSTAT,X'20'      PACKAGE IS LOCKED                            
         GOTO1 PUTREC                                                           
*                                                                               
PACKUPEX MVI   RDUPDATE,C'N'       READ FOR UPDATE IS OFF                       
         B     XIT                                                              
         EJECT                                                                  
*--THIS ROUTINE WRITES THE BUY RECORD TO THE FILE.                              
*                                                                               
WRITBREC NTR1                                                                   
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
*                                  IF RECORD ISN'T ALREADY THERE                
         CLC   KEY(20),KEYSAVE                                                  
         BE    WBR10                                                            
*                                                                               
         MVC   AIO,AIO2            THEN ADD IT                                  
         GOTO1 ADDREC                                                           
         MVC   TWDSKADR,KEY        SAVE THE DISK ADDRESS                        
         MVI   WRITESW,C'Y'        SET UNITS WRITTEN TO YES                     
*                                                                               
         BAS   RE,BLDKEY84         BUILD 84 KEY                                 
         MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
         GOTO1 ADD                                                              
*                                                                               
         BAS   RE,BLDKEY94         BUILD 94 KEY                                 
         MVC   KEY+21(4),TWDSKADR  SET DISK ADDRESS                             
         GOTO1 ADD                                                              
*                                                                               
         B     WBRX                                                             
*                                                                               
WBR10    MVC   AIO,AADDIO          TEMPORARY IO AREA (SINCE NO AIO3)            
         TM    KEY+20,X'80'        IF DELETED                                   
         BZ    WBR20                                                            
*                                  THEN UNDELETE IT AND WRITE RECORD            
         NI    KEY+20,X'FF'-X'83'                                               
         OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
         GOTO1 WRITE                                                            
*                                                                               
WBR20    BAS   RE,BLDKEY84                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    KEY+20,X'80'        IF DELETED                                   
         BZ    WBR30                                                            
*                                  THEN UNDELETE IT AND WRITE RECORD            
         NI    KEY+20,X'FF'-X'83'                                               
         OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
         GOTO1 WRITE                                                            
*                                                                               
WBR30    BAS   RE,BLDKEY94                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    KEY+20,X'80'        IF DELETED                                   
         BZ    WBR40                                                            
*                                  THEN UNDELETE IT AND WRITE RECORD            
         NI    KEY+20,X'FF'-X'83'                                               
         OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
         GOTO1 WRITE                                                            
*                                                                               
WBR40    GOTO1 GETREC              READ OLD RECORD                              
*                                                                               
         MVC   AIO,AIO2            WRITE OUR RECORD IN ITS PLACE                
         GOTO1 PUTREC                                                           
         MVI   WRITESW,C'Y'        SET UNITS WRITTEN TO YES                     
*                                  DON'T READ DELETED RECORDS                   
WBRX     NI    DMINBTS,X'FF'-X'08'                                              
         MVI   RDUPDATE,C'N'       RESET READ FOR UPDATE                        
         B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
         EJECT                                                                  
*--THIS ROUTINE BUILDS THE UNIVERSE ELEMENT                                     
*--ELEMENT RETURNED IN WORKAREA                                                 
*                                                                               
BLDUNIV  NTR1                                                                   
*                                                                               
         L     R5,AIO2                                                          
         USING NURECD,R3                                                        
*                                                                               
         LA    R2,DBDEMOB                                                       
         USING GUVD,R2                                                          
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVAGY,SIGNON2C                                                  
         MVC   GUVCODE,TWUNIV                                                   
         OC    GUVCODE,GUVCODE     TEST FOR UNIVERSE CODE                       
         BNZ   *+10                HAVE ONE                                     
         MVC   GUVDATE,TWPDATE     ELSE USE AIR DATE                            
         XC    WORKAREA(200),WORKAREA                                           
         LA    R3,WORKAREA                                                      
         ST    R3,GUVAOUT          OUTPUT ELEMENT ADDRESS                       
         L     R0,AIO1                                                          
         ST    R0,GUVAREC          SET AND CLEAR AREA FOR UNIV. RECORD          
         LA    R1,2000                                                          
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   GUVCMFCS,ACOMFACS                                                
         MVC   GUVNETWK,VNETWEEK                                                
         NI    NUUNST2,X'FF'-X'08' TURN OFF CABLE UNIV                          
         GOTO1 VGETNUN,DMCB,(R2)                                                
         CLI   GUVERROR,0          TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT FOR NOW                           
         CLI   GUVUTYPE,C'C'                                                    
         BNE   *+8                                                              
         OI    NUUNST2,X'08'                                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*-SUB-ROUTINE TO CHECK CHARACTERS FOR NUMERIC                                   
*                                                                               
CHKNUM   NTR1                                                                   
         ZIC   R4,0(R1)            NUMBER OF BYTES                              
         L     R6,0(R1)            ADDRESS OF LOCATION                          
         LTR   R4,R4                                                            
         BZ    CHKNERR                                                          
*                                                                               
CHKN020  CLI   0(R6),C'0'                                                       
         BL    CHKNERR                                                          
         CLI   0(R6),C'9'                                                       
         BH    CHKNERR                                                          
         LA    R6,1(R6)                                                         
         BCT   R4,CHKN020                                                       
*                                                                               
         SR    R6,R6               SET ZERO RETURN CODE                         
*                                                                               
CHKNERR  LTR   R6,R6                                                            
         B     XIT                                                              
         EJECT                                                                  
*-SUB-ROUTINE TO CONVERT MILITARY TIME TO START QUARTER HOUR                    
*                                                                               
GETSQH   NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,3,TWPRSTIM       START TIME                                   
         SR    R0,R0                                                            
         D     R0,=F'100'          R1=HOURS, R0=REMAINDER MINUTES               
         MH    R1,=H'60'           CONVERT HOURS TO MINUTES                     
         AR    R1,R0               SUM TOTAL MINUTES                            
         CH    R1,=H'360'          TEST FOR LESS THAN 6 AM                      
         BNL   *+8                                                              
         AH    R1,=Y(60*24)        ADD MINUTES OF 24 HOURS                      
         SH    R1,=H'360'          SUBTRACT 6 HOURS TO BASE OFF 6AM             
         SR    R0,R0                                                            
         D     R0,=F'15'           DIVIDE BY MINUTES IN A QUARTER HOUR          
         STC   R1,TWQTRHR                                                       
         B     XIT                                                              
         EJECT                                                                  
*-SUB-ROUTINE TO BUILD THE 84 KEY                                               
*                                                                               
BLDKEY84 NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING NURECD,RE                                                        
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,TWAGYMED                                                  
         MVC   NUKPCLT,TWCLIENT                                                 
         MVC   NUKPNET,TWNET                                                    
         MVC   NUKPPROG,TWPRCODE                                                
         MVC   NUKPDATE,TWPDATE                                                 
         MVC   NUKPEST,TWEST                                                    
         MVC   NUKPSUB,TWLNCT                                                   
         MVC   NUKPDP,TWDYPT                                                    
         OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
         B     XIT                                                              
         DROP  RE                                                               
         SPACE 2                                                                
*-SUB-ROUTINE TO BUILD THE 94 KEY                                               
*                                                                               
BLDKEY94 NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING NURECD,RE                                                        
         MVI   NUKDTYPE,X'94'                                                   
         MVC   NUKDAM,TWAGYMED                                                  
         MVC   NUKDCLT,TWCLIENT                                                 
         MVC   NUKDEST,TWEST                                                    
         MVC   NUKDNET,TWNET                                                    
         MVC   NUKDDAY,TWDAYNO                                                  
         MVC   NUKDTIME,TWQTRHR                                                 
         MVC   NUKDPROG,TWPRCODE                                                
         MVC   NUKDDATE,TWPDATE                                                 
         MVC   NUKDSUB,TWLNCT                                                   
         OC    KEY+20(1),TWCNTRL   SET POSTING TYPE IN CONTROL                  
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*--SET TO UNIT FILE                                                             
UNITSET  NTR1                                                                   
         GOTO1 SETSYS,DMCB,=C'NETWORK',=CL8'UNTDIR',=CL8'UNTFILE'               
         B     XIT                                                              
         SPACE 2                                                                
*--SET TO SPOT FILE                                                             
SPOTSET  NTR1                                                                   
         GOTO1 SETSYS,DMCB,=C'NETWORK',=CL8'SPTDIR',=CL8'SPTFIL'                
         B     XIT                                                              
         EJECT                                                                  
*-SUB-ROUTINE TO BUILD ERROR RECORD AND DOWMNLOAD THE RECORD                    
*                                                                               
WRITERR  NTR1                                                                   
         LA    RE,WORKAREA                                                      
         USING CT0EOT01,RE                                                      
         XC    0(50,RE),0(RE)                                                   
*                                                                               
         CLI   0(R1),C'U'                                                       
         BNE   WRTER050                                                         
*-UNIT LEVEL ERROR WRITE THE DATE                                               
         MVC   ERRDATE,0(R4)                                                    
         MVC   ERRID,=CL4'UNIT'                                                 
         B     WRTER080                                                         
*                                                                               
WRTER050 MVC   ERRID,=CL4'PACK'                                                 
         CLI   0(R1),C'K'                                                       
         BE    WRTER080                                                         
         MVC   ERRID,=CL4'PROG'                                                 
         CLI   0(R1),C'P'                                                       
         BE    WRTER080                                                         
         MVC   ERRID,=CL4'CODE'                                                 
*                                                                               
WRTER080 LA    R5,ERRTAB                                                        
*                                                                               
WRTER090 CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID ERROR CODE                           
         CLC   MADERR,0(R5)                                                     
         BE    WRTER100                                                         
         LA    R5,31(R5)                                                        
         B     WRTER090                                                         
*                                                                               
WRTER100 MVC   ERRMSG,1(R5)        MOVER MESSAGE OUT                            
*                                                                               
         CLI   MADERR,99                                                        
         BNE   WRTER120                                                         
         MVC   ERRMSG+13(6),TWPRCODE                                            
*                                                                               
WRTER120 L     R5,=A(ITNBYERR)                                                  
         GOTO1 PUTITEM,DMCB,(R5),42,WORKAREA                                    
         BNE   EXIT                                                             
*                                                                               
         MVI   MADERR,0                                                         
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
EXIT     L     RD,SAVEDRD                                                       
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOCAL WORKAREA FOR TABLES, ETC                                                
         DS    0D                                                               
WORKAREA DS    CL600               WORK SPACE FOR DEMO CALCS, ETC               
*                                                                               
ERRTAB   DS    0F                                                               
*                                                                               
*-PACKAGE ERRORS                                                                
         DC    AL1(01),CL30'INVALID AGENCY                '                     
         DC    AL1(02),CL30'INVALID POSTING TYPE          '                     
         DC    AL1(03),CL30'INVALID AGENCY MEDIA          '                     
         DC    AL1(04),CL30'INVALID CLIENT                '                     
         DC    AL1(05),CL30'INVALID PRODUCT               '                     
         DC    AL1(06),CL30'INVALID ESTIMATE              '                     
         DC    AL1(07),CL30'INVALID EST DATE RANGE        '                     
         DC    AL1(08),CL30'INVALID STATION               '                     
         DC    AL1(09),CL30'INVALID DEMO BASE             '                     
         DC    AL1(10),CL30'PACKAGE REC DOES NOT EXIST    '                     
         DC    AL1(11),CL30'PROD CODE NOT ON PACKAGE REC  '                     
         DC    AL1(12),CL30'COST NOT EQUAL TO PKG REC     '                     
         DC    AL1(13),CL30'DEMO BASE NOT EQUAL TO PKG REC'                     
         DC    AL1(14),CL30'UNITS EXIST UNDER PACKAGE     '                     
         DC    AL1(15),CL30'ESTIMATE NOT NUMERIC          '                     
         DC    AL1(16),CL30'PACKAGE DATES NOT NUMERIC     '                     
         DC    AL1(17),CL30'PACKAGE COST NOT NUMERIC      '                     
         DC    AL1(18),CL30'PACKAGE CODE NOT NUMERIC      '                     
*-PROGRAM ERRORS                                                                
         DC    AL1(30),CL30'PROG LENGTH NOT NUMERIC       '                     
         DC    AL1(31),CL30'PROGRAM RATE NOT NUMERIC      '                     
         DC    AL1(32),CL30'HOMES IMP. NOT NUMERIC        '                     
         DC    AL1(33),CL30'PROG DEMOS NOT NUMERIC        '                     
         DC    AL1(34),CL30'INVALID PROGRAM NAME          '                     
         DC    AL1(35),CL30'TIMES NOT NUMERIC             '                     
         DC    AL1(36),CL30'TIMES VALUE GREATER 2400      '                     
         DC    AL1(37),CL30'TIMES VALUE EQUALS ZERO       '                     
*-UNIT ERRORS                                                                   
         DC    AL1(60),CL30'DATE NOT NUMERIC              '                     
         DC    AL1(61),CL30'DATE NOT A MONDAY             '                     
         DC    AL1(62),CL30'UNIT COUNT NOT NUMERIC        '                     
         DC    AL1(63),CL30'UNIT COUNT GREATER 192        '                     
         DC    AL1(64),CL30'BILLBOARD COUNT NOT NUMERIC   '                     
         DC    AL1(65),CL30'BILLBOARD GREATER UNIT COUNT  '                     
         DC    AL1(66),CL30'UNIT COUNT = 0                '                     
         DC    AL1(67),CL30'UNIT DATE OUTSIDE MST. RANGE  '                     
*-PROGRAM CODE MESSAGE                                                          
         DC    AL1(99),CL30'PROGRAM CODE                  '                     
         DC    XL1'FF'                                                          
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* CT0EDI01:  PACKAGE HEADER RECORD                                              
       ++INCLUDE CT0EDI01                                                       
         SPACE 4                                                                
* CT0EDI02:  PROGRAM RECORD                                                     
         SPACE 4                                                                
       ++INCLUDE CT0EDI02                                                       
         SPACE 4                                                                
* CT0EDI03:  PACKAGE TRAILER RECORD                                             
       ++INCLUDE CT0EDI03                                                       
         SPACE 4                                                                
* CT0EDI04:  FILE TRAILER RECORD                                                
         SPACE 4                                                                
       ++INCLUDE CT0EDI04                                                       
         SPACE 4                                                                
* CT0EDO01:  ERROR MESSAGE RECORD                                               
         SPACE 4                                                                
       ++INCLUDE CT0EDO01                                                       
         SPACE 4                                                                
* CONTROL BLOCK FOR DEMOGRAPHIC MODULES                                         
         SPACE 4                                                                
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
DIVISOR  DS    F                   DIVISOR BUCKET                               
AINTEREC DS    A                   POINTER TO INTERIM RECORD (D/T)              
DEMODUB  DS    D                   EXTRA STORAGE FOR DEMUP                      
TOTSHR   DS    3F                  SHARE ACCUMULATORS                           
HOMSHR   DS    3F                                                               
         DS    0F                                                               
         SPACE 2                                                                
         DS    0F                                                               
         EJECT                                                                  
* EXTRA CONTROL BLOCK FOR DEMOGRAPHIC MODULES                                   
         SPACE 4                                                                
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
*                                                                               
*     FOLLOWING FIELDS ARE DEVELOPED FROM THE REQUEST SENT                      
*     FROM THE PC.                                                              
*                                                                               
SCRATCH  DS    0CL1                SCRATCH SPACE                                
*--KEY FIELDS                                                                   
TWAGYMED DS    CL1                 AGENCY/MEDIA                                 
TWCLIENT DS    CL2                 CLIENT                                       
TWCLIC2  DS    CL4                 2ND CLIENT COST                              
TWNET    DS    CL4                 NETWORK                                      
TWMKT    DS    CL2                 MARKET NUMBER                                
TWEST    DS    CL1                 ESTIMATE                                     
TWESTST  DS    CL6                 ESTIMATE START DATE                          
TWESTEN  DS    CL6                 ESTIMATE END DATE                            
TWESTC2  DS    CL4                 2ND ESTIMATE COST                            
TWPKG    DS    CL1                 PACKAGE                                      
TWRATE   DS    CL1                 RATE TYPE                                    
TWCVRGE  DS    CL1                 COVERAGE                                     
TWPSTTYP DS    CL1                 POSTING TYPE                                 
TWCNTRL  DS    CL1                 CONTROL BYTE                                 
TWHOMES  DS    CL4                 HOMES IMPRESSION                             
TWDEMOS  DS    CL58                DEMO CODES 3 BYTE FORMAT                     
TWBASE   DS    CL1                 DEMO BASE V(PH) I(MPRESSION)                 
TWPCOST  DS    CL4                 PACKAGE COST                                 
TWPLEN   DS    CL1                 LENGTH                                       
TWPRCST  DS    CL4                 ACTUAL COST                                  
TWPKEY   DS    CL20                PACKAGE KEY                                  
TWMSTPRD DS    CL1                 MASTER ALLOCATION PRODUCT                    
*                                                                               
TWDYPT   DS    CL1                 DAYPART                                      
TWUNIV   DS    CL2                 UNIVERSE CODE                                
TWSREP   DS    CL2                 SPECIAL REP                                  
TWHUTTYP DS    CL1                 HUT TYPE                                     
TWFILTER DS    CL11                FILTER                                       
TWSTATUS DS    CL1                 PACKAGE STATUS                               
*                                                                               
TWPRCODE DS    CL6                 PROGRAM CODE                                 
TWPRROT  DS    CL1                 PROGRAM ROTATION                             
TWPRROTN DS    CL1                 4 BIT START END ROTATION NUMBERS             
TWDAYNO  DS    CL1                 DAY NUMBER (FOR X'94' KEY)                   
TWDAYHEX DS    CL1                 DAY NUMBER (HEX)                             
TWPRSTIM DS    CL2                 PROGRAM START TIME                           
TWPRETIM DS    CL2                 PROGRAM END TIME                             
TWPRNAME DS    CL16                PROGRAM NAME                                 
TWQTRHR  DS    CL1                 START QUARTER HOUR                           
TWPDATE  DS    CL2                 DATE FOR UNIT RECORDS                        
TWPRCT   DS    CL1                 NUMBER OF UNITS IN A PROGRAM                 
TWADU    DS    CL1                 ADU FLAG                                     
*                                                                               
TWLNCT   DS    CL1                 START OF LINE COUNT                          
TWUNTCT  DS    CL1                 NUMBER OF UNITS TO BE ADDED                  
TWBLBCT  DS    CL1                 NUMBER OF BILLBOARDS                         
TWDSKADR DS    CL4                 DISK ADDRESS OF LAST ADD                     
*                                                                               
WRITESW  DS    CL1                 Y=UNIT WAS WRITTEN TO FILE                   
*                                                                               
MADERR   DS    C                   PACKAGE ERROR NUMBER                         
*                                  ERROR1 =AGENCY ERROR                         
*                                  ERROR2 =POSTING TYPE ERROR                   
*                                  ERROR3 =INVALID AGENCY MEDIA                 
*                                  ERROR4 =INVALID CLIENT                       
*                                  ERROR5 =INV MASTER ALLOCATION PRD            
*                                  ERROR6 =INVALID ESTIMATE                     
*                                  ERROR7 =INVALID ESTIMATE DATE RANGE          
*                                  ERROR8 =INVALID STATION                      
*                                  ERROR9 =INVALID DEMO BASE                    
*                                  ERROR10=NO MATCHING PACKAGE RECORD           
*                                  ERROR11=MASTER PROD NOT ON PACKAGE           
*                                  ERROR12=COST DOES NOT MATCH PACKAGE          
*                                  ERROR13=DEMO BASE NOT MATCH PACKAGE          
*                                  ERROR14=UNITS EXIST UNDER PACKAGE            
*                                  ERROR15=EST NOT NUMERIC                      
*                                  ERROR16=PACKAGE DATES NOT NUMERIC            
*                                  ERROR17=PACKAGE COST NOT NUMERIC             
*                                  ERROR18=PACKAGE CODE NOT NUMERIC             
*                                                                               
*                                  PROGRAM ERROR NUMBER                         
*                                  ERROR30=PROGRAM LENGTH NOT NUMERIC           
*                                  ERROR31=PROGRAM RATE NOT NUMERIC             
*                                  ERROR32=HOMES IMP. NOT NUMERIC               
*                                  ERROR33=PROGRAM DEMOS NOT NUMERIC            
*                                  ERROR34=INVALID PROGRAM NAME                 
*                                  ERROR35=TIMES NOT NUMERIC                    
*                                  ERROR36=TIMES VALUE > 2400                   
*                                  ERROR37=TIMES VALUE = 0                      
*                                                                               
*                                  UNIT ERROR NUMBER                            
*                                  ERROR60=DATE NOT NUMERIC                     
*                                  ERROR61=DATE NOT A MONDAY                    
*                                  ERROR62=UNIT COUNT NOT NUMERIC               
*                                  ERROR63=NUMBER OF UNITS > 192                
*                                  ERROR64=BILLBOARD COUNT NOT NUMERIC          
*                                  ERROR65=BILLBOARD COUNT > UNIT COUNT         
*                                  ERROR66=UNIT COUNT = 0                       
*                                  ERROR67=UNIT DATE OUTSIDE EST RANGE          
*                                                                               
NFLDS    DS    C                   NUMBER OF DEMOS ON PACKAGE HEADER            
RELO     DS    A                   RELOCATION VALUE                             
*                                                                               
COREFACS DS    0F                                                               
VNETWEEK DS    V                                                                
VGETNUN  DS    V                                                                
*                                                                               
DBDEMOB  DS    CL480               AREA FOR DEMO INTRFACE MODULE                
*                                                                               
LSCRATCH EQU   *-SCRATCH                                                        
*                                                                               
*      END OF PC FIELDS       *                                                 
*                                                                               
*  UPGRADE STORAGE DSECT:                                                       
UPGSTORE DSECT                                                                  
UPGFLAG  DS    CL1                 0  =  UPGRADE HAS MONTH                      
*                                  1  =  UPGRADE HAS ABSOLUTE VALUE             
UBASEBK  DS    CL3                 BASE BOOK                                    
UPGTYPE  DS    XL1                 TYPE OF UPGRADE (NOT DEMO CODE)              
*                                  1  =  PUT (USES SHRS FROM BASE BK)           
*                                  2  =  HPT (USES SHRS FROM BASE BK)           
*                                  3  =  HUT                                    
*                                  4  =  RTG                                    
*                                  5  =  SHR                                    
*                                  6  =  NDX                                    
UPGDEMO  DS    CL3                 DEMO CODE (NOT TYPE OF UPGRADE)              
UMONABS  DS    CL3                 MONTH OR ABS (SEE UPGFLAG)                   
UOPTSHR  DS    XL3                 OPTIONAL ABSOLUTE SHARE VALUE                
*                                                                               
*                                                                               
*  INVENTORY BY DAYPART DSECT                                                   
DIDSTORE DSECT                                                                  
DIDDPT   DS    CL1         0       DAYPART CODE                                 
DIDDYFIL DS    CL1        +1       DAY FILTER: 0=M-F,1=MON,X'FF'=NONE           
DIDTMFIL DS    CL8        +2       TIME FILTER                                  
DIDSTDT  DS    CL3        +10      START DATE FILTER                            
DIDENDT  DS    CL3        +13      END DATE FILTER                              
DIDFLAGS DS    CL1        +16      FLAGS FOR DAYPART                            
*                                  BIT 0  = COMPETITIVE DATA WANTED             
*                                  BIT 1  = RATES WANTED                        
*                                  BIT 2  = DEMO FILTER 1 ENTERED               
*                                  BIT 3  = DEMO FILTER 1 HAS TWO               
*                                           VALUES FOR RANKING                  
*                                  BIT 4  = DEMO FILTER 2 ENTERED               
*                                  BIT 5  = DEMO FILTER 2 HAS TWO               
*                                           VALUES FOR RANKING                  
*                                                                               
*  OCCURRENCE OF FOLLOWING FIELDS DEPENDS ON SETTING OF BITS 2                  
*    AND 4 IN FIELD 'DIDFLAGS'                                                  
*                                                                               
DIDDEMO  DS    CL3        +17      DEMO FILTER                                  
DIDACT   DS    CL1        +20      DEMO FILTER ACTION                           
*                                  >  =  MINIMUM DEMO VALUE                     
*                                  <  =  MAXIMUM DEMO VALUE                     
*                                  B  =  MINIMUM CPP/CPM VALUE                  
*                                  T  =  MAXIMUM CPP/CPM VALUE                  
*                                  =  =  SELECTION RANGE AFTER RANKING          
DIDVAL1  DS    CL4        +21      DEMO FILTER VALUE 1                          
*                                                                               
*  FOLLOWING FIELD WILL ONLY APPEAR IF BIT 3/5 OF DIDFLAGS IS SET               
*                                                                               
DIDVAL2  DS    CL4        +25      DEMO FILTER VALUE 2                          
*                                                                               
*                                                                               
*  INVENTORY BY NUMBER DSECT                                                    
DINSTORE DSECT                                                                  
DININV#  DS    CL3                 INVENTORY #                                  
DINEFDTE DS    CL3                 EFFECTIVE START DATE                         
DINSTDT  DS    CL3                 START DATE FILTER                            
DINENDT  DS    CL3                 END  DATE FILTER                             
DINCOMP  DS    CL1                 COMPETITIVE WANTED FLAG                      
DINRATES DS    CL1                 RATES WANTED FLAG                            
DINAVGCM DS    CL1                 AVERAGE/COMBINE FLAG                         
*                                  0  =  NO ACTION                              
*                                  1  =  AVERAGE FOLLOWING ITEM                 
*                                  2  =  COMBO WITH FOLLOWING ITEM              
LDINSTOR EQU   *-DININV#           LENGTH OF REQUEST ENTRY                      
*                                                                               
         EJECT                                                                  
* DSECT TO COVER PACKAGE RECORD                                                 
*                                                                               
       ++INCLUDE NEGENPACK                                                      
*                                                                               
* DSECT TO COVER UNIT RECORD                                                    
*                                                                               
       ++INCLUDE NEGENUNIT                                                      
*                                                                               
* DSECT TO COVER PROGRAM RECORD                                                 
*                                                                               
       ++INCLUDE SPGENPROG                                                      
*                                                                               
* DSECT TO COVER CLIENT RECORD                                                  
*                                                                               
       ++INCLUDE SPGENCLT                                                       
*                                                                               
* DSECT TO COVER ESTIMATE RECORD                                                
*                                                                               
       ++INCLUDE SPGENEST                                                       
*                                                                               
* DSECT TO COVER GETNUN IO AREA                                                 
*                                                                               
       ++INCLUDE NEGETNUND                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
*  IUNRECDS:  UNIFORM DEMO DATA DSECT                                           
       ++INCLUDE IUNRECDS                                                       
         EJECT                                                                  
*  DDCOMFACS: ADDRESS ROUTINE DSECT                                             
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
*  FAFACTS:  TO GET ALLOWABLE RUN STATISTICS                                    
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTMAD0E   05/01/02'                                      
         END                                                                    
