*          DATA SET NESFM5B    AT LEVEL 111 AS OF 10/28/10                      
*PHASE T31C5BA                                                                  
         TITLE 'NESFM5B - UCOMM MAINTANANCE PROGRAMM'                           
******************************************************************              
* REGISTERS:  R0 -- WORK                                                        
*             R1 -- WORK                                                        
*             R2 -- SCREEN FIELD HEADER                                         
*             R3 -- WORK                                                        
*             R4 -- WORK                                                        
*             R5 -- WORK                                                        
*             R6 -- GETEL REGISTER                                              
*             R7 -- SECOND BASE                                                 
*             R8 -- SPOOL                                                       
*             R9 -- SYSD                                                        
*             RA -- TWA                                                         
*             RB -- FIRST BASE                                                  
*             RC -- GEND                                                        
*             RD -- SYSTEM                                                      
*             RE -- SYSTEM                                                      
*             RF -- SYSTEM                                                      
******************************************************************              
         EJECT                                                                  
******************************************************************              
*                   MAIN PROGRAM                                 *              
******************************************************************              
T31C5B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1C5B**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         OI    GENSTAT4,NODELLST   DISALLOW DELETES FROM LST SCREEN             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
**       CLI   MODE,PRINTREP       PRINT RECORDS                                
**       BE    PR                                                               
         CLI   MODE,RECDEL         ID REC THAT GENCON ABOUT TO DELETE           
         BE    RDEL                                                             
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*                         VALIDATE KEY                           *              
******************************************************************              
*                                                                               
VK       DS    0H                                                               
         LA    R3,UCKEY            BASE REG FOR MY REC                          
         USING UCOMHDRD,R3                                                      
         XC    UCKEY,UCKEY                                                      
         MVC   UCOMKTYP,=X'0D0C'                                                
         MVI   UCOMCTYP,C'U'                                                    
*                                                                               
         MVI   FLAG,0                                                           
         MVI   FLAG2,0                                                          
         BAS   RE,CLRNAME                                                       
*                                                                               
* MEDIA FIELD                                                                   
*                                                                               
         LA    R2,COMMEDH          MEDIA                                        
         TM    4(R2),X'20'         VALIDATED?                                   
         BO    *+8                                                              
         OI    FLAG2,KEYCHG                                                     
*        MVI   USEIONUM,2          READ INTO AIO2                               
         MVC   AIO,AIO2                                                         
         GOTO1 VALIMED             VALIDATE MEDIA CODE                          
         MVC   COMMEDN,MEDNM       DISPLAY NAME                                 
         OI    COMMEDNH+6,X'80'                                                 
         MVC   UCOMKAGY,BAGYMD     COPY A/M INTO KEY                            
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
* CLIENT FIELD                                                                  
*                                                                               
         LA    R2,COMCLIH          CLIENT FIELD                                 
         TM    4(R2),X'20'         VALIDATED?                                   
         BO    *+8                                                              
         OI    FLAG2,KEYCHG                                                     
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BE    VK20                                                             
*K10     MVI   USEIONUM,2          READ INTO AIO2                               
VK10     MVC   AIO,AIO2            READ INTO AIO2                               
         GOTO1 VALICLT             VALIDATE                                     
         MVC   COMCLIN,CLTNM       AND TRANSMIT THE NAME                        
         OI    COMCLINH+6,X'80'                                                 
         MVC   UCOMKCLT,BCLT       COPY CLT INTO KEY                            
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
* PRODUCT FIELD                                                                 
*                                                                               
VK20     LA    R2,COMPROH          PRODUCT FIELD                                
         TM    4(R2),X'20'         VALIDATED?                                   
         BO    *+8                                                              
         OI    FLAG2,KEYCHG                                                     
         XC    QPRD,QPRD           JUST IN CASE                                 
         CLI   COMPROH+5,0         EMPTY?                                       
         BE    VK30                EST CHECK                                    
*        MVI   USEIONUM,2          READ INTO AIO2                               
         MVC   AIO,AIO2                                                         
         GOTO1 VALIPRD             VALIDATE PROD                                
         MVC   COMPRON,PRDNM         VALIDATE PRODUCT CODE AND TRANSMIT         
         OI    COMPRONH+6,X'80'      PRODUCT NAME                               
         MVC   UCOMKPRD,QPRD                                                    
         OC    UCOMKPRD,SPACES                                                  
         OI    FLAG,PRDLEV         INDICATE PRD LEV                             
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
* ESTIMATE FIELD                                                                
*                                                                               
VK30     LA    R2,COMESTH          ESTIMATE FIELD                               
         TM    4(R2),X'20'         VALIDATED?                                   
         BO    *+8                                                              
         OI    FLAG2,KEYCHG                                                     
*                                                                               
         CLI   COMESTH+5,0         ANY EST ?                                    
         BE    VK40                                                             
         OC    QPRD,QPRD           YES, ANY PRD ?                               
         BNZ   *+12                NO, ERROR                                    
         MVI   ERROR,INVACT        HAVE TO HAVE PRD                             
         B     TRAPERR                                                          
*        MVI   USEIONUM,2          READ INTO AIO2                               
         MVC   AIO,AIO2                                                         
         GOTO1 VALIEST             VALIDATE EST                                 
         MVC   COMESTN,ESTNAME     TRANSMIT DESCRIPTION                         
         OI    COMESTNH+6,X'80'                                                 
         MVC   UCOMKEST,BEST                                                    
         OI    FLAG,ESTLEV         INDICATE EST                                 
         NI    FLAG,X'FF'-PRDLEV TURN OFF PRD LEV                               
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
VK40     TM    FLAG,PRDLEV+ESTLEV  FOR PRD OR EST LEVEL                         
         BZ    VK44                                                             
         BAS   RE,VKCKREC          CHECK CLT REC                                
         B     BLDKEY                                                           
*                                                                               
VK44     OI    FLAG,CLTLEV         CLT LEVEL UCOM REC                           
         MVC   SVFLAG,FLAG                                                      
*                                                                               
BLDKEY   MVC   KEY,UCKEY                                                        
         MVC   AIO,AIO1            JUST IN CASE                                 
*        MVI   USEIONUM,1                                                       
         BAS   RE,PROUNPRO         PROTECT DATA FILEDS                          
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*  CHECK CLIENT LEVEL RECORD EXISTS                                             
*                                                                               
VKCKREC  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST CAN BE BLANK                         
         BNE   *+10                                                             
         XC    SVCLTKEY,SVCLTKEY   CLEAR FOR LIST                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ADD                                   
         BNE   EXIT                DON'T BOTHER CHECKING                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(6),UCKEY        CLT LEVEL REC KEY                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     CLT UCOM REC FOUND ?                         
         BE    VKCKR10                                                          
         MVC   ERRNUM,=AL2(NOCLTLEV)                                            
         B     SPERREX                                                          
*                                                                               
VKCKR10  MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY                                                      
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'41'                                                     
         TM    FLAG,PRDLEV                                                      
         BO    *+8                                                              
         MVI   ELCODE,X'51'                                                     
         BAS   RE,GETEL                                                         
         BNE   VKCKR20             ERROR EXIT                                   
*                                                                               
         L     R5,AIO2                                                          
         CLC   SVCLTKEY,0(R5)      DID WE ALREADY DISPLAY                       
         BNE   VKCKR15                                                          
         CLC   SVFLAG,FLAG         DID LEVEL OR KEY CHANGE                      
         BE    EXIT                                                             
*                                                                               
VKCKR15  MVC   SVCLTKEY,0(R5)                                                   
         MVC   SVFLAG,FLAG                                                      
         BAS   RE,DCLT             THEN NEED TO DISPLAY CLT LEVEL DATA          
         B     EXIT                                                             
*                                                                               
VKCKR20  MVC   ERRNUM,=AL2(NOCLTPRD)                                            
         TM    FLAG,PRDLEV                                                      
         BO    *+10                                                             
         MVC   ERRNUM,=AL2(NOCLTEST)                                            
         B     SPERREX                                                          
*                                                                               
*******************************************************************             
*  PROTECT OR UNPROTECT ACCORDING TO LEVEL                                      
*******************************************************************             
PROUNPRO NTR1                                                                   
         LA    R2,COMLN1TH         POINT TO 1ST DATA FIELD                      
         MVI   PROSW,20            BY DEFAULT EXT 20                            
         TM    FLAG,CLTLEV         CLIENT LEVEL ?                               
         BO    PRO10               YES, GO WITH DEFAULT                         
         MVI   PROSW,30                                                         
PRO10    CLI   0(R2),0             END OF SCREEN ?                              
         BE    DONE                                                             
         TM    1(R2),X'02'         EXTENSION ?                                  
         BZ    SKIP                                                             
         ZIC   RE,0(R2)            GET LEN                                      
         SHI   RE,8                                                             
         AR    RE,R2               RE POINTS TO XTENSION                        
         CLC   0(1,RE),PROSW                                                    
         BNE   PROTECT             OTHER EXTENSION HAS TO BE PROTECTED          
         NI    1(R2),X'FF'-X'20'   THIS ONE -- UNPROTECT                        
         B     *+8                                                              
PROTECT  OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'                                                      
*                                                                               
SKIP     ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     PRO10                                                            
*                                                                               
DONE     B     EXIT                                                             
*                                                                               
*  CLEAR KEY NAME FIELDS                                                        
*                                                                               
CLRNAME  DS    0H                                                               
         XC    COMMEDN,COMMEDN                                                  
         OI    COMMEDNH+6,X'80'                                                 
         XC    COMCLIN,COMCLIN                                                  
         OI    COMCLINH+6,X'80'                                                 
         XC    COMPRON,COMPRON                                                  
         OI    COMPRONH+6,X'80'                                                 
         XC    COMESTN,COMESTN                                                  
         OI    COMESTNH+6,X'80'                                                 
         BR    RE                  RETURN                                       
*                                                                               
         EJECT                                                                  
******************************************************************              
*                         VALIDATE RECORD                        *              
******************************************************************              
*                                                                               
VR       DS    0H                                                               
         L     R3,AIO                                                           
         USING UCOMHDRD,R3                                                      
         MVC   UCOMKEY,KEY         WILL BE REBUILDING REC                       
         DROP  R3                                                               
*                                                                               
         MVI   ACTELOPT,C'N'       SET FOR NO ACTIVITY ELEM                     
*                                                                               
         TM    FLAG,CLTLEV         CLIENT LEVEL ?                               
         BZ    VR10                                                             
         CLI   ACTEQU,ACTADD       IF ADD, DO NOTHING                           
         BE    VR10                IF CHANGE                                    
*                                                                               
         BAS   RE,COUNTPE          WILL COUNT HOW MANY PRD & EST ELEMS          
*                                                                               
VR10     L     R3,AIO              DELETE ALL ELEMENTS                          
         XCEFL 24(R3),1000                                                      
         USING UCOMHDRD,R3                                                      
         MVC   UCOMLEN,DATADISP                                                 
         DROP  R3                                                               
*                                                                               
         LA    R4,10               SET # OF ROWS ON SCREEN FOR BCT              
         LA    R2,COMLN1TH                                                      
         MVI   PRDSW,X'41'         SET DEFAULT VALUES                           
         MVI   ESTSW,X'51'                                                      
*                                                                               
         TM    FLAG,CLTLEV         CLIENT LEVEL ?                               
         BZ    VROTHER             GOTO DIFFR VAL REC LOGIC                     
*                                                                               
VR5      CLI   5(R2),0             IS TYPE FIELD EMPTY ?                        
         BNE   VR20                                                             
         BAS   RE,CHKROW           YES, SEE IF WHOLE ROW IS EMPTY               
         B     VRNEXT              IF RETURNED, GOTO NXT ROW                    
*                                                                               
VR20     CLI   8(R2),C'P'                                                       
         BNE   VR30                                                             
         MVC   ELCODE,PRDSW        PUT ELCODE FOR PRODUCT                       
         ZIC   R0,PRDSW                                                         
         AHI   R0,1                GET NXT AVAILABLE ELCODE FOR PRD             
         STC   R0,PRDSW                                                         
         B     VR40                                                             
*                                                                               
VR30     CLI   8(R2),C'E'          TYPE E ?                                     
         BE    *+12                                                             
         MVI   ERROR,INVALID       IF NO, ERROR                                 
         B     TRAPERR                                                          
         MVC   ELCODE,ESTSW        YES, PUT ELCODE FOR EST                      
         ZIC   R0,ESTSW                                                         
         AHI   R0,1                                                             
         STC   R0,ESTSW                                                         
*                                                                               
         USING SUCMELEM,R6         REBUILD NEW                                  
VR40     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVC   SUCMELEM,ELCODE                                                  
*                                                                               
         ZIC   R0,0(R2)            VALIDATE EDIT FIELD                          
         AR    R2,R0                                                            
         CLI   5(R2),0             MISSING IS OK                                
         BE    VR44                                                             
         CLI   8(R2),C'C'          CHARACTER ?                                  
         BE    VR42                                                             
         CLI   8(R2),C'D'          DATE ?                                       
         BE    VR42                                                             
         CLI   8(R2),C'N'          NUMERIC ?                                    
         BE    VR42                                                             
         MVI   ERROR,INVALID       IF NO, ERROR                                 
         B     TRAPERR                                                          
VR42     MVC   SUCMEDIT,8(R2)                                                   
*                                                                               
VR44     ZIC   R0,0(R2)            VALIDATE LENGTH                              
         AR    R2,R0                                                            
         TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+12                                                             
         MVI   ERROR,INVALID       IF NO, ERROR                                 
         B     TRAPERR                                                          
         ZIC   R1,5(R2)            CHK THE LENGTH LESS THAN 32                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,32                                                            
         BNH   *+12                                                             
         MVI   ERROR,INVALID       IF NO, ERROR                                 
         B     TRAPERR                                                          
         CHI   R1,1                                                             
         BNL   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         STC   R1,SUCMLEN                                                       
*                                                                               
         ZIC   R0,0(R2)            VALIDATE MBI                                 
         AR    R2,R0                                                            
         OI    SUCMUSE1,X'70'      DEFAULT IS X'70' ON                          
         CLI   5(R2),0             ANYTHING IN MBI ?                            
         BE    VR50                IT'S OK                                      
         CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         MVI   ERROR,INVALID       IF NO, ERROR                                 
         B     TRAPERR                                                          
         OI    SUCMUSE1,X'08'                                                   
*                                                                               
VR50     ZIC   R0,0(R2)            VALIDATE TITLE/NAME                          
         AR    R2,R0                                                            
*                                                                               
         ST    R2,ACURFLD                                                       
         BAS   RE,VREFF                                                         
         TM    FLAG2,EFFDINP       EFFECTIVE DATE?                              
         BZ    *+8                                                              
         LA    R2,JUSTDATH                                                      
*                                                                               
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BNZ   VR55                                                             
*                                                                               
         TM    FLAG2,EFFDINP                                                    
         BO    VR60                                                             
         MVI   ERROR,MISSING       IF NO, ERROR                                 
         B     TRAPERR                                                          
*                                                                               
VR55     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SUCMDT(0),8(R2)     PUT FIELD NAME IN ELEM                       
VR60     AHI   R1,7                6-OVERHEAD, 1 FOR EXECUTE                    
         STC   R1,SUCMELEN         ELEM LENGTH (VARIABLE 6+DATA)                
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         L     R2,ACURFLD                                                       
*                                                                               
VRNEXT   ZIC   R0,0(R2)            GOTO NXT ROW                                 
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R4,VR5                                                           
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VRX                                                              
         BAS   RE,COMPELEM         COMPARE NEW ELEMS WITH # OF OLD              
*                                  IF LESS IN REBUILT REC -- DIE!               
         BE    VRX                                                              
         OI    COMMEDH+6,X'81'     SET MODIFIED (FORCE REVALIDATION)            
         LA    R2,CONACTH                                                       
         MVC   ERRNUM,=AL2(NOCHGTYP)  CANNOT CHANGE OR REMOVE TYP               
         B     SPERREX                                                          
*                                                                               
*******                                                                         
*  VALIDATE PRD OR ESTIMATE LEVEL DATA                                          
*******                                                                         
VROTHER  DS    0H                  VALIDATA FIELD DATA                          
*                                                                               
         BAS   RE,GETLVLC          GET CLT LEVEL REC INTO AIO2                  
*                                                                               
         LA    R2,COMLN1TH                                                      
VRO10    CLI   8(R2),C' '          ANYTHING IN TYPE ?                           
         BNH   VRX                 NO, DONE                                     
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VRO12                                                            
         BAS   RE,CKINPUT          CHECK FOR ANY DATA ON ACTION ADD             
*                                                                               
VRO12    BAS   RE,GETELCOD         GET ELEM CODE                                
         LA    RE,5                GET TO DATA FIELD                            
VRO15    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,VRO15                                                         
         CLI   8(R2),C' '          ANYTHING IN DATA FIELD ?                     
         BNH   VRONEXT             NO, NEXT ROW                                 
         BAS   RE,VROADDEL         VALIDATE DATA FILD & ADD ELEM                
*                                                                               
VRONEXT  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R4,VRO10                                                         
*                                                                               
VRX      B     DR                  GOTO DISP REC LOGIC                          
         EJECT                                                                  
******************************************************************              
* VALIDATES FIELD DATA IF EFFECTIVE DATE IS INPUTTED                            
******************************************************************              
VREFF    NTR1                                                                   
         MVC   ORIGDATA,8(R2)                                                   
         MVC   ORIGDLEN,5(R2)                                                   
*                                                                               
         CLI   COMESTH+5,0         ONLY USE EFFECTIVE DATE IF ESTIMATE          
         BE    VREFFX              IS INPUTTED                                  
*                                                                               
         NI    FLAG2,X'FF'-EFFDINP                                              
         CLI   8(R2),C'#'          DENOTES EFFECTIVE DATE                       
         BNE   VREFFX                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   9(R2),C'T'          TIME?                                        
         BE    VRE03                                                            
         CLI   9(R2),C'I'          INTEGRATION?                                 
         BE    VRE03                                                            
         CLI   9(R2),C'R'          REGULAR BILL?                                
         BE    VRE03                                                            
         CLI   9(R2),C'F'          COMMISSION BILL?                             
         BNE   VRE05                                                            
         CLC   =C'FEB',9(R2)       IS IT FEB MOS?                               
         BE    VRE05                                                            
VRE03    CLI   10(R2),C'#'         MUST END WITH A #                            
         BNE   TRAPERR                                                          
*                                                                               
*        MVI   JUSTDATH+5,X'03'                                                 
*        MVC   JUSTDATA(3),8(R2)                                                
*                                                                               
         CLI   12(R2),C'#'         COST TYPE CAN BE FOLLOWED                    
         BNE   VREFFX              BY LINE ITEM                                 
         CLI   11(R2),C'R'                                                      
         BE    *+12                                                             
         CLI   11(R2),C'F'                                                      
         BNE   TRAPERR                                                          
*                                                                               
*        MVI   JUSTDATH+5,X'06'    #T##R# (EXAMPLE)                             
*        MVI   JUSTDATA+3,C'#'                                                  
*        MVC   JUSTDATA+4(2),11(R2)                                             
         XC    NEWFDATA,NEWFDATA   ADD #, SHIFT BY 1 BYTE                       
         MVC   NEWFDATA(3),8(R2)   MOVE IN #T#                                  
         LA    R1,NEWFDATA+3                                                    
         ZIC   RF,5(R2)            LENGTH OF FIELD                              
         SHI   RF,3                MINUS 2 FOR #T, MINUS 1 FOR EX               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),10(R2)      MOVE REST OF FLD STARTING AT #R              
*                                                                               
         ZIC   RF,5(R2)            ADD 1 FOR EXTRA #                            
         AHI   RF,1                                                             
         STC   RF,5(R2)            UPDATED LENGTH OF FIELD                      
         MVC   8(32,R2),NEWFDATA   MOVE BACK WHOLE FLD W/ XTRA #                
         B     VREFFX                                                           
*                                                                               
VRE05    OI    FLAG2,EFFDINP                                                    
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
*                                                                               
         LA    R1,TEMPFLD+8        BUILD FLD HDR W/ JUST DATE                   
         LA    R4,9(R2)            A(TITLE/NAME) FIELD                          
         SR    R5,R5                                                            
         MVI   ERROR,INVALID                                                    
*                                                                               
VRE10    DS    0H                                                               
         CLI   0(R4),C'#'          GET TO END OF DATE LINE?                     
         BE    VRE20                                                            
         CH    R5,=H'32'           DID WE REACH END OF FIELD?                   
         BH    TRAPERR                                                          
*                                                                               
         MVC   0(1,R1),0(R4)       MOVE DATE INTO TEMP FLD                      
         AHI   R1,1                                                             
         AHI   R4,1                                                             
         AHI   R5,1                                                             
         B     VRE10                                                            
*                                                                               
VRE20    DS    0H                                                               
         STC   R5,TEMPFLD+5        PUT IN LENGTH OF DATE INPUT                  
         AHI   R5,8                TAKE HDR FLD TO ACCOUNT                      
         STC   R5,TEMPFLD                                                       
*                                                                               
         LA    RF,TEMPFLD+8        SETUP PERVAL CALL                            
         ST    RF,DMCB             A(TEMP DATE FIELD)                           
         MVC   DMCB(1),TEMPFLD+5   INPUT LENGTH                                 
*                                                                               
         XC    MYBLOCK,MYBLOCK                                                  
         LA    RF,MYBLOCK                                                       
         ST    RF,DMCB+4                                                        
*                                                                               
         CLI   TEMPFLD+5,X'08'     SINGLE DATE INPUT?                           
         BH    *+8                                                              
         OI    DMCB+4,X'20'                                                     
*                                                                               
         GOTO1 PERVAL,DMCB                                                      
         CLI   DMCB+4,X'04'        ONLY ONE DATE INPUT?                         
         BE    *+12                                                             
         CLI   DMCB+4,0                                                         
         BNE   TRAPERR                                                          
*                                                                               
         LA    RF,MYBLOCK                                                       
         USING PERVALD,RF                                                       
*                                                                               
         MVC   PERIOD,PVALCPER     DISPLAYABLE PERIOD                           
         MVC   STARTC,PVALCSTA     START OF PERIOD                              
         MVC   ENDC,PVALCEND       END OF PERIOD                                
         MVC   STARTB,PVALBSTA     START OF PERIOD                              
         MVC   ENDB,PVALBEND       END OF PERIOD                                
*                                                                               
         MVC   STARTMY(2),PVALBSTA      YYMM                                    
         MVC   ENDMY(2),PVALBEND        YYMM                                    
         DROP  RF                                                               
*                                                                               
         ZIC   RF,5(R2)            SUBTRACT LENGTH OF DATE                      
         ZIC   R1,TEMPFLD+5        FROM LENGTH OF FIELD                         
         AHI   R1,2                ADD 2BYTES FOR #                             
         SR    RF,R1                                                            
         STC   RF,NEWFDATH+5       NEW FIELD LENGTH                             
*                                                                               
         XC    NEWFDATA,NEWFDATA                                                
         LA    R1,NEWFDATA                                                      
         LA    R4,8(R2)                                                         
         SR    R5,R5                                                            
*                                                                               
         MVI   0(R1),C'#'          INITIALIZE FIELD WITH #                      
         AHI   R1,1                                                             
*                                                                               
         MVC   0(2,R1),STARTMY     AND PERIOD                                   
         AHI   R1,2                                                             
*                                                                               
         OC    ENDMY,ENDMY                                                      
         BZ    *+10                                                             
         MVC   0(2,R1),ENDMY                                                    
*                                                                               
         AHI   R1,2                                                             
         LA    R4,9(R2)                                                         
         LA    R5,31                                                            
*                                                                               
VRE50    DS    0H                                                               
         CLI   0(R4),C'#'          END OF DATE?                                 
         BE    VRE55                                                            
*                                                                               
         AHI   R4,1                                                             
         BCT   R5,VRE50                                                         
*                                                                               
VRE55    DS    0H                                                               
         CLI   2(R4),C'#'          FOLLOWED BY A #T# OR #I#?                    
         BNE   VRE65                                                            
*                                                                               
         CLI   1(R4),C'T'                                                       
         BE    VRE57                                                            
         CLI   1(R4),C'I'                                                       
         BE    VRE57                                                            
         CLI   1(R4),C'R'                                                       
         BE    VRE57                                                            
         CLI   1(R4),C'F'                                                       
         BNE   VRE65                                                            
*                                                                               
VRE57    MVC   0(2,R1),=C'##'                                                   
         AHI   R1,2                                                             
         MVC   0(1,R1),1(R4)                                                    
         AHI   R1,1                                                             
         AHI   R4,2                                                             
         AHI   R5,-2                                                            
*                                                                               
         ZIC   RF,NEWFDATH+5                                                    
         AHI   RF,1                                                             
         STC   RF,NEWFDATH+5                                                    
*                                                                               
         CLC   =C'#R#',0(R4)          FOLLOWED BY A #R# OR #F#?                 
         BE    *+14                                                             
         CLC   =C'#F#',0(R4)                                                    
         BNE   VRE65                                                            
*                                                                               
         MVC   0(2,R1),=C'##'                                                   
         AHI   R1,2                                                             
         MVC   0(1,R1),1(R4)                                                    
         AHI   R1,1                                                             
         AHI   R4,2                                                             
         AHI   R5,-2                                                            
*                                                                               
         ZIC   RF,NEWFDATH+5                                                    
         AHI   RF,1                                                             
         STC   RF,NEWFDATH+5                                                    
*                                                                               
*RE60    MVC   0(1,R1),0(R4)                                                    
*        AHI   R4,1                                                             
*        AHI   R1,1                                                             
*                                                                               
VRE65    SHI   R5,1                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R4)                                                    
*                                                                               
         XC    JUSTDATH,JUSTDATH                                                
         XC    JUSTDATA,JUSTDATA                                                
*                                                                               
         ZIC   RF,NEWFDATH+5       GET NEW LENGTH OF FIELD                      
*                                                                               
         STC   RF,JUSTDATH+5       LENGTH OF JUST DATA FIELD                    
         MVC   JUSTDATA(26),0(R1)  JUST THE DATA FIELD                          
*                                                                               
         AHI   RF,6                WITH #(M/Y)(M/Y)#                            
*                                                                               
         STC   RF,5(R2)                                                         
         MVC   8(32,R2),NEWFDATA                                                
         OI    6(R2),X'01'                                                      
*                                                                               
VREFFX   DS    0H                                                               
         B     EXIT                                                             
******************************************************************              
* VALIDATES PRD AND EST DATA USING RULE FROM CLT RECORD                         
******************************************************************              
VROADDEL NTR1                                                                   
         XC    ELEM,ELEM                                                        
         MVC   ELEM(1),ELCODE                                                   
*                                                                               
         ST    R2,ACURFLD                                                       
*                                                                               
         BAS   RE,VREFF                                                         
         TM    FLAG2,EFFDINP       EFFECTIVE DATE?                              
         BZ    *+8                                                              
         LA    R2,JUSTDATH                                                      
*                                                                               
         L     R6,AIO2             POINT TO CLT LEVEL REC                       
         BAS   RE,GETEL            GET THE NEEDED "EDIT" ELEMENT                
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING SUCMELEM,R6                                                      
*                                                                               
         CLC   5(1,R2),SUCMLEN     CHECK IF L'INPUT IS VALID                    
         BNH   *+18                INPUT TOO LONG ERROR                         
         L     R2,ACURFLD                                                       
         MVC   ERRNUM,=AL2(BADLNTH)                                             
         B     SPERREX                                                          
*                                                                               
VRUSR5   CLI   SUCMEDIT,C' '       ACCEPT ANY INPUT                             
         BNH   VRUSR30                                                          
         CLI   SUCMEDIT,C'N'       IF TYPE IS NUMERIC                           
         BNE   VRUSR10                                                          
         TM    4(R2),X'08'         INPUT MUST BE NUMERIC                        
         BO    VRUSR30                                                          
         ZIC   R1,5(R2)            BUT ALLOW '- /'                              
         LA    R5,8(R2)                                                         
         CLI   5(R2),0                                                          
         BE    VRUSR30                                                          
*                                                                               
VRUSR7   CLI   0(R5),C'0'                                                       
         BL    VRUSR8                                                           
         CLI   0(R5),C'9'                                                       
         BNH   VRUSR9                                                           
*                                                                               
VRUSR8   CLI   0(R5),C' '                                                       
         BE    VRUSR9                                                           
         CLI   0(R5),C'/'                                                       
         BE    VRUSR9                                                           
         CLI   0(R5),C'-'                                                       
         BE    VRUSR9              NOT NUMERIC ERROR                            
         CLI   0(R5),C'#'                                                       
         BE    VRUSR9                                                           
*                                                                               
         TM    FLAG2,EFFDINP                                                    
         BZ    *+16                                                             
         MVC   8(L'ORIGDATA,R2),ORIGDATA                                        
         MVC   5(1,R2),ORIGDLEN                                                 
*                                                                               
         MVI   ERROR,NOTNUM                                                     
         L     R2,ACURFLD                                                       
         B     TRAPERR                                                          
*                                                                               
VRUSR9   LA    R5,1(R5)                                                         
         BCT   R1,VRUSR7                                                        
         B     VRUSR30                                                          
*                                                                               
VRUSR10  CLI   SUCMEDIT,C'C'       IF TYPE IS ALPHABETIC                        
         BNE   VRUSR20                                                          
         ZIC   R1,5(R2)            ALLOW ALL INPUT EXCEPT NUMBERS               
         LA    R5,8(R2)                                                         
         CLI   5(R2),0                                                          
         BE    VRUSR30                                                          
*                                                                               
VRUSR15  CLI   0(R5),C'0'                                                       
         BL    VRUSR17                                                          
         CLI   0(R5),C'9'                                                       
         BH    VRUSR17             NOT ALL ALPHABETIC                           
*                                                                               
         TM    FLAG2,EFFDINP                                                    
         BZ    *+16                                                             
         MVC   8(L'ORIGDATA,R2),ORIGDATA                                        
         MVC   5(1,R2),ORIGDLEN                                                 
*                                                                               
         MVI   ERROR,NOTALPHA                                                   
         L     R2,ACURFLD                                                       
         B     TRAPERR                                                          
*                                                                               
VRUSR17  LA    R5,1(R5)                                                         
         BCT   R1,VRUSR15                                                       
         B     VRUSR30                                                          
*                                                                               
VRUSR20  CLI   SUCMEDIT,C'D'       IF TYPE DATE                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   VRUSR25                                                          
*                                                                               
         TM    FLAG2,EFFDINP                                                    
         BZ    *+16                                                             
         MVC   8(L'ORIGDATA,R2),ORIGDATA                                        
         MVC   5(1,R2),ORIGDLEN                                                 
*                                                                               
         MVI   ERROR,INVDATE                                                    
         L     R2,ACURFLD                                                       
         B     TRAPERR                                                          
*                                                                               
VRUSR25  L     R5,0(R1)            L'DATE                                       
         ZIC   R1,5(R2)            L'INPUT                                      
         SR    R1,R5                                                            
         BZ    VRUSR30                                                          
*                                                                               
         TM    FLAG2,EFFDINP                                                    
         BZ    *+16                                                             
         MVC   8(L'ORIGDATA,R2),ORIGDATA                                        
         MVC   5(1,R2),ORIGDLEN                                                 
*                                                                               
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VRUSR30  DS    0H                                                               
         L     R2,ACURFLD                                                       
         ZIC   R1,5(R2)            R1=L(INPUT)                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+6(0),8(R2)     PUT FIELD DATA IN ELEM                       
         AHI   R1,7                6-OVERHEAD, 1 FOR EXECUTE                    
         STC   R1,ELEM+1           ELEM LENGTH (VARIABLE 6+DATA)                
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VRUSRX   B     EXIT                DONE WITH THIS ELEM                          
********                                                                        
*  GET NEXT AVAILABLE ELEM CODE                                                 
********                                                                        
GETELCOD NTR1                                                                   
         CLI   8(R2),C'P'                                                       
         BNE   GETLC20                                                          
         MVC   ELCODE,PRDSW        PUT ELCODE FOR PRODUCT                       
         ZIC   R0,PRDSW                                                         
         AHI   R0,1                GET NXT AVAILABLE ELCODE FOR PRD             
         STC   R0,PRDSW                                                         
         B     EXIT                                                             
*                                                                               
GETLC20  CLI   8(R2),C'E'          TYPE E ?                                     
         BE    GETLC40                                                          
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
GETLC40  DS    0H                                                               
         MVC   ELCODE,ESTSW        PUT ELCODE FOR ESTIMATE                      
         ZIC   R0,ESTSW                                                         
         AHI   R0,1                                                             
         STC   R0,ESTSW                                                         
         B     EXIT                                                             
*                                                                               
********                                                                        
*  ROUTINE TO CHK IF WHOLE ROW IS EMPTY                                         
*  IF PARTIALLY EMPTY EXIT WITH ERROR                                           
********                                                                        
CHKROW   DS    0H                                                               
         ST    R2,SAVER2           POINTING TO TYP                              
         LA    R1,4                TEST EDT,LEN,MBI,NAME FIELDS                 
CHKTOP   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             MISSING ?                                    
         BE    CHKBACK             YES - GOOD                                   
         L     R2,SAVER2           POINT R2 BACK TO TYP                         
         MVI   ERROR,MISSING       MISSING ERROR                                
         B     TRAPERR                                                          
CHKBACK  BCT   R1,CHKTOP                                                        
         BR    RE                  WITH R2 POINTING TO FIELD NAME               
         EJECT                                                                  
******************************************************************              
* CHECK FOR ANY DATA INPUT ON ACTION ADD                                        
******************************************************************              
CKINPUT  NTR1                                                                   
*                                                                               
         LA    R2,COMLN1TH         POINT TO 1ST DATA FIELD                      
CKI10    CLI   0(R2),0             END OF SCREEN ?                              
         BE    CKIX                                                             
         TM    1(R2),X'02'         EXTENSION ?                                  
         BZ    CKI20                                                            
         ZIC   RE,0(R2)            GET LEN                                      
         SHI   RE,8                                                             
         AR    RE,R2               RE POINTS TO XTENSION                        
         CLI   0(RE),30            DATA FIELD                                   
         BNE   CKI20               NO THEN SKIP                                 
         OC    8(10,R2),8(R2)      ANY INPUT                                    
         BNZ   EXIT                FOUND INPUT THEN RETURN                      
CKI20    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     CKI10                                                            
*                                                                               
CKIX     LA    R2,COMLN1DH         POINT TO 1ST DATA FIELD                      
         MVI   ERROR,MISSING       AND ASK FOR INPUT                            
         B     TRAPERR                                                          
*                                                                               
********                                                                        
*  CANNOT DELETE CLT LEVEL ELEM IF PRD OR EST EXIST                             
********                                                                        
*                                                                               
COMPELEM NTR1                                                                   
         NI    PRDSW,X'0F'         TURN OFF '4' FOR PRD ELEMS                   
         NI    ESTSW,X'0F'         TURN OFF '5' FOR EST ELEMS                   
         ZIC   R0,PRDSW            DECREMENT, FOR PRDSW AND ESTSW               
         AHI   R0,-1               ARE SET TO NEXT AVAIL ELEM SPACE             
         STC   R0,PRDSW                                                         
         ZIC   R0,ESTSW            DECREMENT, FOR PRDSW AND ESTSW               
         AHI   R0,-1               ARE SET TO NEXT AVAIL ELEM SPACE             
         STC   R0,ESTSW                                                         
         CLC   RPRDCNT,PRDSW                                                    
         BH    CREAD               FEWER PRDS ON SCREEN                         
         CLC   RESTCNT,ESTSW                                                    
         BNH   YES                                                              
CREAD    MVC   SAVEKEY,KEY                                                      
         XC    KEY+6(L'KEY-6),KEY+6                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'UCOMKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SEQ                                                              
         CLC   SAVEKEY(6),KEY      COMPARE NXT REC THRU CLT CODE                
         BNE   CYES                                                             
         B     NO                  THERE IS MORE THAN CLT LEV, BAD              
*                                                                               
CYES     MVC   KEY,SAVEKEY         REREAD REC                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'UCOMKEY),KEYSAVE   SAME REC ?                              
         BE    YES                                                              
         DC    H'0'                                                             
*                                                                               
********                                                                        
*  COUNT PRD & EST ELEMS IN UCOM CLT                                            
********                                                                        
*                                                                               
COUNTPE  NTR1                                                                   
         SR    R3,R3               FOR PRD COUNTS                               
         SR    R4,R4               FOR EST COUNTS                               
         L     R5,AIO              POINT TO CLIENT LEVEL RECORD                 
         LA    R5,24(R5)           FIRST ELEMENT                                
CPE10    CLI   0(R5),0             END OF RECORD ?                              
         BE    CPEEND              YES - STORE COUNTS                           
         CLI   0(R5),X'4A'         PRODUCT ELEM ?                               
         BH    CPE20               NO - MUST BE ESTIMATE                        
         LA    R3,1(R3)            ADD TO PRD COUNT                             
         B     *+8                                                              
CPE20    LA    R4,1(R4)            ADD TO EST COUNT                             
         ZIC   R0,1(R5)                                                         
         AR    R5,R0               POINT TO NEXT ELEM                           
         B     CPE10               LOOK FOR MORE ELEMENTS                       
CPEEND   DS    0H                                                               
         STC   R3,RPRDCNT          STORE PRD COUNT FOR "LATER"                  
         STC   R4,RESTCNT          STORE EST COUNT FOR "LATER"                  
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*                  DISPLAY RECORD                                *              
******************************************************************              
DR       DS    0H                                                               
         BAS   RE,CLRLUP           CLEAR NON KEY FIELDS                         
*                                                                               
         CLI   ACTNUM,ACTSEL       ACTION SELECT?                               
         BNE   DR50                                                             
         CLI   MODE,DISPREC                                                     
         BNE   DR50                                                             
         CLC   =XL2'0D0C',KEY                                                   
         BNE   DR50                                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
DR50     L     R5,AIO                                                           
         TM    FLAG,CLTLEV         CLIENT LEV RECORD ?                          
         BO    DREC                YES,REC IN AIO, NO GETREC IN AIO2            
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(6),SAVEKEY                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R5,AIO2                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         CLI   ACTNUM,ACTADD       IF ADDING NO SEQ RESTORE                     
         BE    DREC                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     REC FOUND ?                                  
         BE    *+6                                                              
         DC    H'0'                NO, DIE                                      
*                                                                               
         MVC   AIO,AIO3            RESET GETREC FOR PUTREC                      
         GOTO1 GETREC              RESTORE REC                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
******************************************************************              
*  DISPLAY CLIENT LEVEL REC                                                     
******************************************************************              
DREC     DS    0H                  R5 -> TO UCOM CLT REC                        
*                                  R6 -> TO 'OTHER' UCOM REC (IF NEED)          
*                                                                               
         LA    R2,COMLN1TH         1ST SCREEN DATA FIELD                        
         LA    R5,24(R5)           1ST UCOM CLT RECORD ELEM                     
         USING SUCMELEM,R5         R5 --> TO UCOM CLT REC ELEM                  
*                                                                               
DRSEL    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    DREXIT              DONE                                         
         TM    FLAG,CLTLEV                                                      
         BO    DRKEEP                                                           
         CLI   0(R5),X'4E'         PRODUCT ELEM ?                               
         BH    DRSEL10             NO, MUST BE EST                              
         TM    FLAG,PRDLEV         DO WE WANT PRODUCT                           
         BO    DRKEEP                                                           
         B     DRNEXT                                                           
*                                                                               
DRSEL10  CLI   0(R5),X'5E'         ESTIMATE ELEM ?                              
         BH    DRNEXT                                                           
         TM    FLAG,ESTLEV         DO WE WANT EST?                              
         BO    DRKEEP                                                           
DRNEXT   ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DRSEL                                                            
*                                                                               
DRKEEP   DS    0H                                                               
         MVI   8(R2),C'E'          "TYP" FIELD                                  
         CLI   SUCMELEM,X'4E'      ESTIMATE ELEM ?                              
         BH    *+8                 YES                                          
         MVI   8(R2),C'P'          NO, PRODUCT ELEM                             
         BAS   RE,DRKXMIT          SEND 'TYP' & -> TO 'EDT'                     
*                                                                               
         MVC   8(1,R2),SUCMEDIT                                                 
         BAS   RE,DRKXMIT          SEND 'EDT' GET 'LEN'                         
*                                                                               
         EDIT  (1,SUCMLEN),(2,8(R2)),ALIGN=LEFT                                 
         BAS   RE,DRKXMIT          SEND 'LEN' GET 'MBI'                         
*                                                                               
         MVI   8(R2),C'Y'                                                       
         TM    SUCMUSE1,X'08'      MBI = Y ?                                    
         BO    *+8                 YES                                          
         MVI   8(R2),C' '          NO,CLEAR                                     
         BAS   RE,DRKXMIT          SEND 'MBI', POINT TO 'FIELD NAME'            
*                                                                               
         ZIC   RE,SUCMELEN         ELEMENT LENGTH                               
         AHI   RE,-7               ELEM OVERHEAD + 1 FOR EX MOVE                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SUCMDT      "FIELD NAME"                                 
         BAS   RE,DRKXMIT          SEND "NAME" & POINT TO "FIELD DATA"          
         MVC   8(L'COMLN1D,R2),SPACES        CLEAR "DATA"                       
*                                                                               
         TM    FLAG,CLTLEV                                                      
         BZ    DRKMORE                                                          
DRKNXT   BAS   RE,DRKXMIT                                                       
         B     DRNEXT                                                           
*                                                                               
DRKMORE  DS    0H                  ON PRD OR EST LEV                            
         L     R6,AIO              R6 POINTS TO PRD OR EST UCOM REC             
         MVC   ELCODE,SUCMELEM     GET ELCODE FROM CLT                          
         BAS   RE,GETEL                                                         
         BNE   DRKNXT                                                           
*                                                                               
         CLI   6(R6),C'#'          EFFECTIVE DATE?                              
         BNE   DRKMOR50                                                         
         LA    R4,8(R2)                                                         
         MVI   0(R4),C'#'                                                       
         AHI   R4,1                                                             
*                                                                               
         CLI   7(R6),C'T'                                                       
         BE    DRKMOR03                                                         
         CLI   7(R6),C'I'                                                       
         BE    DRKMOR03                                                         
         CLI   7(R6),C'R'                                                       
         BE    DRKMOR03                                                         
         CLI   7(R6),C'F'                                                       
         BE    DRKMOR03                                                         
*                                                                               
         CLI   8(R6),C'T'                                                       
         BE    *+12                                                             
         CLI   8(R6),C'I'                                                       
         BNE   DRKMOR05                                                         
*                                                                               
         MVC   0(1,R4),8(R6)                                                    
         MVI   1(R4),C'#'                                                       
         AHI   R4,2                                                             
         B     DRKNXT                                                           
*                                                                               
DRKMOR03 CLC   =C'##',8(R6)        LINE ITEM FOLLOWING CTYPE?                   
         BNE   DRKMOR50                                                         
         CLI   10(R6),C'R'                                                      
         BE    *+12                                                             
         CLI   10(R6),C'F'                                                      
         BNE   DRKMOR50                                                         
*                                                                               
         MVC   0(2,R4),7(R6)       MOVE IN COST TYPE AND #                      
         MVC   2(2,R4),10(R6)      MOVE IN LINE ITEM AND #                      
         AHI   R4,4                                                             
         ZIC   RE,1(R6)            ELEMENT LENGTH                               
         AHI   RE,-7               ELEM OVERHEAD + 1 FOR EX MOVE                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),12(R6)      MOVE TO "FIELD DATA"                         
         B     DRKNXT              FINISH ROW AND ELEM                          
*                                                                               
DRKMOR05 XC    STARTB,STARTB                                                    
         MVC   STARTB(2),7(R6)        YYMM OF START                             
         MVI   STARTB+2,X'0F'         DEFAULT TO 15TH                           
*                                                                               
         XC    ENDB,ENDB                                                        
         MVC   ENDB(2),9(R6)          YYMM OF END                               
         MVI   ENDB+2,X'0F'           DEFAULT TO 15TH                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,STARTB),(6,PERIOD)                                
         MVC   0(6,R4),PERIOD                                                   
         AHI   R4,6                                                             
*                                                                               
         OC    ENDB(2),ENDB                                                     
         BZ    DRKMOR10                                                         
         CLC   STARTB,ENDB            JUST ONE MOS?                             
         BE    DRKMOR10                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,ENDB),(6,PERIOD)                                  
         MVI   0(R4),C'-'                                                       
         MVC   1(6,R4),PERIOD                                                   
         AHI   R4,7                                                             
*                                                                               
DRKMOR10 DS    0H                                                               
         MVI   0(R4),C'#'                                                       
         AHI   R4,1                                                             
         ZIC   RE,1(R6)                                                         
         SHI   RE,6                ELEM OVERHEAD                                
         SHI   RE,6                #MYMY#                                       
         SHI   RE,1                FOR EXECUTED MOVE                            
*                                                                               
         LTR   RE,RE                                                            
         BNP   DRKNXT                                                           
         BZ    DRKNXT                                                           
*                                                                               
         CLI   12(R6),C'#'                                                      
         BNE   DRKMOR20                                                         
         CLC   =C'##',14(R6)       IS THERE LINE ITEM TOO?                      
         BE    DRKMOR15                                                         
         SHI   RE,1                NO, ONLY COST TYPE                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),13(R6)      MOVE TO "FIELD DATA"                         
         B     DRKNXT                                                           
*                                                                               
DRKMOR15 MVC   0(2,R4),13(R6)      MOVE IN COST TYPE AND #                      
         AHI   R4,2                                                             
         SHI   RE,4                #T##                                         
         LTR   RE,RE                                                            
         BNP   DRKNXT                                                           
         BZ    DRKNXT                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),16(R6)      MOVE TO "FIELD DATA"                         
         B     DRKNXT                                                           
*                                                                               
DRKMOR20 EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),12(R6)      MOVE TO "FIELD DATA"                         
         B     DRKNXT                                                           
*                                                                               
DRKMOR50 DS    0H                                                               
         ZIC   RE,1(R6)            ELEMENT LENGTH                               
         AHI   RE,-7               ELEM OVERHEAD + 1 FOR EX MOVE                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),6(R6)       MOVE TO "FIELD DATA"                         
         B     DRKNXT              FINISH ROW AND ELEM                          
         DROP  R5                                                               
*                                                                               
DREXIT   DS    0H                                                               
         TM    FLAG,CLTLEV         IF CLIENT, EXIT                              
         BO    *+8                                                              
         BAS   RE,SETPROS          PROTECT 'OTHER' LEVEL ENTRY FIELDS           
         B     EXIT                                                             
*                                                                               
**************************************************************                  
*  TRANSMIT AND BUMP TO NEXT FIELD                                              
**************************************************************                  
DRKXMIT  DS    0H                                                               
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R0,0(R2)            GET NEXT FIELD                               
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
******************************************************************              
*  DISPLAY CLIENT LEVEL REC - R5 POINT TO CLT REC                               
******************************************************************              
DCLT     NTR1                      R5 -> TO UCOM CLT REC                        
*                                                                               
         BAS   RE,CLRLUP           CLEAR NON KEY FIELDS                         
*                                                                               
         LA    R2,COMLN1TH         1ST SCREEN DATA FIELD                        
         LA    R5,24(R5)           1ST UCOM CLT RECORD ELEM                     
         USING SUCMELEM,R5         R5 --> TO UCOM CLT REC ELEM                  
*                                                                               
DCLT10   DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    DCLTX               DONE                                         
         CLI   0(R5),X'4E'         PRODUCT ELEM ?                               
         BH    DCLT20              NO, MUST BE EST                              
         TM    FLAG,PRDLEV         DO WE WANT PRODUCT                           
         BO    DCLT50                                                           
         B     DCLT30                                                           
*                                                                               
DCLT20   CLI   0(R5),X'5E'         ESTIMATE ELEM ?                              
         BH    DCLT30                                                           
         TM    FLAG,ESTLEV         DO WE WANT EST?                              
         BO    DCLT50                                                           
DCLT30   ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DCLT10                                                           
*                                                                               
DCLT50   DS    0H                                                               
         MVI   8(R2),C'E'          "TYP" FIELD                                  
         CLI   SUCMELEM,X'4E'      ESTIMATE ELEM ?                              
         BH    *+8                 YES                                          
         MVI   8(R2),C'P'          NO, PRODUCT ELEM                             
         BAS   RE,DRKXMIT          SEND 'TYP' & -> TO 'EDT'                     
*                                                                               
         MVC   8(1,R2),SUCMEDIT                                                 
         BAS   RE,DRKXMIT          SEND 'EDT' GET 'LEN'                         
*                                                                               
         EDIT  (1,SUCMLEN),(2,8(R2)),ALIGN=LEFT                                 
         BAS   RE,DRKXMIT          SEND 'LEN' GET 'MBI'                         
*                                                                               
         MVI   8(R2),C'Y'                                                       
         TM    SUCMUSE1,X'08'      MBI = Y ?                                    
         BO    *+8                 YES                                          
         MVI   8(R2),C' '          NO,CLEAR                                     
         BAS   RE,DRKXMIT          SEND 'MBI', POINT TO 'FIELD NAME'            
*                                                                               
         ZIC   RE,SUCMELEN         ELEMENT LENGTH                               
         AHI   RE,-7               ELEM OVERHEAD + 1 FOR EX MOVE                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SUCMDT      "FIELD NAME"                                 
         BAS   RE,DRKXMIT          SEND "NAME" & POINT TO "FIELD DATA"          
         MVC   8(L'COMLN1D,R2),SPACES        CLEAR "DATA"                       
*                                                                               
         BAS   RE,DRKXMIT                                                       
         B     DCLT30                                                           
*                                                                               
DCLTX    B     EXIT                                                             
**************************************************************                  
*  CLEAR SCREEN                                                                 
**************************************************************                  
CLRLUP   NTR1                                                                   
         LA    R6,COMLN1TH         1ST DATA FIELD ON SCREEN                     
CLUP     CLI   0(R6),0             END OF SCREEN ?                              
         BE    CLUPXIT             DONE                                         
         TM    1(R6),X'02'         EXTENDED HEADER ?                            
         BZ    CLUPSKP             NO - NEXT FIELD                              
         ZIC   RE,0(R6)                                                         
         AHI   RE,-17              FOR EXECUTED CLEAR                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R6),8(R6)       CLEAR THIS AREA                              
         OI    6(R6),X'80'         TRANSMIT FIELD                               
CLUPSKP  DS    0H                                                               
         ZIC   RE,0(R6)                                                         
         AR    R6,RE                                                            
         B     CLUP                GO TEST NEXT FIELD                           
*                                                                               
CLUPXIT  DS    0H                                                               
         XIT1                                                                   
**************************************************************                  
*  PROTECT EMPTY SCREEN LINES                                                   
**************************************************************                  
SETPROS  NTR1                                                                   
         LA    R2,COMLN1TH                                                      
         MVI   PROSW,C'P'                                                       
         TM    FLAG,PRDLEV         PRODUCT LEVEL ?                              
         BO    *+8                                                              
         MVI   PROSW,C'E'                                                       
         LA    R4,10               SET BCT COUNT                                
*                                                                               
SETP10   CLC   PROSW,8(R2)                                                      
         BNE   *+8                                                              
         OI    FLAG,X'01'          SET RIGHT LEVEL INDICATION FOR LATER         
         LA    R1,5                                                             
SETP20   ZIC   R0,0(R2)            GET TO DATA FIELD                            
         AR    R2,R0                                                            
         BCT   R1,SETP20                                                        
         TM    FLAG,X'01'          IF SET ON, SKIP PROTECTION                   
         BO    SETP30                                                           
         OI    1(R2),X'20'         PROTECT 'OTHER' LEVEL ENTRY                  
         OI    6(R2),X'80'                                                      
SETP30   ZIC   R0,0(R2)                                                         
         AR    R2,R0               GOTO NXT TYP FIELD                           
         NI    FLAG,X'FF'-X'01'                                                 
         BCT   R4,SETP10                                                        
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*                   DISPLAY KEY                                      *          
**********************************************************************          
DK       DS    0H                                                               
         NI    FLAG,X'FF'-CLTLEV                                                
         L     R3,AIO                                                           
         USING UCOMHDR,R3                                                       
*                                                                               
         MVC   BYTE,UCOMKAGY       ISOLATE MEDIA CODE                           
         NI    BYTE,X'0F'                                                       
         LA    R5,MEDTAB             FIND MEDIA CODE USING MEDIA TABLE          
DK10     CLC   BYTE,1(R5)                                                       
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DK10                                                             
DK20     MVC   COMMED,0(R5)                                                     
         OI    COMMEDH+6,X'80'                                                  
         MVI   COMMEDH+5,1          TRANSMIT MEDIA CODE TO SCREEN               
*                                                                               
         GOTO1 CLUNPK,DMCB,UCOMKCLT,COMCLI                                      
         OI    COMCLIH+6,X'80'                                                  
         MVI   COMCLIH+5,3          TRANSMIT CLIENT CODE TO SCREEN              
         CLI   COMCLI+2,C' '                                                    
         BH    *+8                                                              
         MVI   COMCLIH+5,2                                                      
*                                                                               
         OC    UCOMKPRD,UCOMKPRD                                                
         BZ    DKX                                                              
         MVC   COMPRO,UCOMKPRD                                                  
         MVI   COMPROH+5,3                                                      
         OI    COMPROH+6,X'80'      TRANSMIT PRODUCT CODE TO SCREEN             
         CLI   COMPRO+2,C' '                                                    
         BH    *+8                                                              
         MVI   COMPROH+5,2                                                      
*                                                                               
         OC    UCOMKEST,UCOMKEST                                                
         BZ    DKX                                                              
         EDIT  UCOMKEST,COMEST,FILL=0                                           
         OI    COMESTH+6,X'80'      TRANSMIT ESTIMATE CODE TO SCREEN            
         OI    COMESTH+4,X'08'      NUMERIC CODE                                
         MVI   COMESTH+5,3                                                      
*                                                                               
DKX      B     VK                                                               
         DROP  R3                                                               
         EJECT                                                                  
******************************************************************              
*               LIST RECORDS                                     *              
******************************************************************              
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING UCOMHDRD,R6                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    KEY,KEY              TEST FIRST TIME                             
         BNZ   LR15                 KEY IS LAST RECORD READ                     
         MVC   KEY,UCKEY            UCKEY IS VALIDATED KEY                      
         B     *+10                                                             
LR10     MVC   KEY,SVLSTKEY                                                     
*                                                                               
LR15     GOTO1 HIGH                                                             
         B     LR30                                                             
LR20     GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(4),KEYSAVE     TEST FOR ALL DONE                             
         BNE   LRX                                                              
         CLI   KEY+3,C'U'          SEE IF UCOM REC                              
         BNE   LRX                                                              
         OC    COMCLI,COMCLI       ANY CLIENT                                   
         BZ    LR32                                                             
         CLC   KEY+4(2),BCLT                                                    
         BNE   LRX                                                              
*                                                                               
LR32     OC    COMPRO,COMPRO     SEE IF PRODUCT GIVEN                           
         BZ    LR34                                                             
         CLC   KEY+6(3),QPRD                                                    
         BNE   LRX                                                              
*                                                                               
LR34     OC    COMEST,COMEST     SEE IF ESTIMATE GIVEN                          
         BZ    LR50                                                             
         CLC   KEY+9(1),BEST                                                    
         BNE   LRX                                                              
*                                                                               
LR50     DS    0H                                                               
         GOTO1 GETREC              GET THE UCOM RECORD                          
         L     R6,AIO                                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         GOTO1 CLUNPK,DMCB,UCOMKCLT,LSTCLT                                      
*                                                                               
         MVC   LSTPRD,UCOMKPRD                                                  
*                                                                               
         OC    UCOMKEST,UCOMKEST   ESTIMATE ?                                   
         BZ    LR60                NO                                           
         ZIC   R0,UCOMKEST                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LSTEST,DUB                                                       
*                                                                               
         MVC   SVLSTKEY,KEY                                                     
*                                                                               
LR60     GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
* DELETE REC - MAKE SURE NO PRD OR EST LEVEL RECORDS             *              
******************************************************************              
RDEL     DS    0H                                                               
         MVI   ACTELOPT,C'N'       SET FOR NO ACTIVITY ELEMENT                  
*                                                                               
         TM    FLAG,CLTLEV         UCOM CLT LEVEL REC ?                         
         BZ    EXIT                NO - OK TO DELETE                            
*                                  CANNOT DELETE CLT LEVEL REC IF ANY           
*                                  OTHER LEVEL RECS EXIST FOR CLIENT            
         MVC   SAVEKEY,KEY                                                      
         XC    KEY+6(L'KEY-6),KEY+6  CLEAR AFTER CLIENT                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'UCOMKEY),KEYSAVE                                           
         BE    *+6                 CLIENT UCOM REC FOUND ?                      
         DC    H'0'                NO - DIE                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 NEXT RECORD                                  
         CLC   SAVEKEY(6),KEY      COMPARE THRU CLIENT CODE                     
         BNE   RDEOK               OK - NO OTHER REC'S FOR CLIENT               
*                                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT        CANNOT DELETE - INVALID ACTION               
         B     TRAPERR                                                          
*                                                                               
RDEOK    DS    0H                  RESTORE SEQUENCE                             
         MVC   KEY(L'UCOMKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'UCOMKEY),SAVEKEY       SAME RECORD ?                       
         BE    EXIT                YES - OK                                     
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
******************************************************************              
* READ CLIENT LEVEL RECORD INTO AIO2                                            
******************************************************************              
GETLVLC  DS    0H              MUST GET UCOM CLIENT INTO AIO2                   
         ST    RE,SAVERE                                                        
         MVC   SAVEKEY,KEY                                                      
         XC    KEY+6(L'KEY-6),KEY+6   CLEAR ALL AFTER CLT CODE                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'UCOMKEY),KEYSAVE                                           
         BE    *+6                 CLIENT UCOM REC FOUND ?                      
         DC    H'0'                NO - DIE                                     
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'UCOMKEY),SAVEKEY     RESTORE KEY                           
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING, NO SEQ RESTORE                    
         BE    GETLVLCX                                                         
         GOTO1 HIGH                RESTORE KEY SEQ                              
         CLC   KEY(L'UCOMKEY),KEYSAVE                                           
         BE    *+6                 REC FOUND ?                                  
         DC    H'0'                NO - DIE                                     
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC              REREAD RECORD                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
GETLVLCX DS    0H                                                               
         MVC   AIO,AIO1            RESTORE PRD/EST UCOM REC ADD                 
         L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
*                                                                               
******************************************************************              
* ERROR EXITS AND STUFF                                                         
******************************************************************              
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,23                                                        
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
*                                                                               
*                                                                               
*                                                                               
BADLNTH  EQU   303                                                              
NOCHGTYP EQU   304                                                              
NOCLTLEV EQU   305                                                              
NOCLTPRD EQU   306                                                              
NOCLTEST EQU   307                                                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
ZEROES   DC    20C'0'                                                           
RELO     DS    F                                                                
*                                                                               
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM68D         MAINT SCREEN                                  
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM69D         LIST SCREEN                                   
         EJECT                                                                  
*                                                                               
UCOMHDRD DSECT                                                                  
       ++INCLUDE SPGENUCOM                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE+220                                                     
*                                                                               
UCKEY    DS    CL32                                                             
SAVEKEY  DS    CL32                                                             
SVCLTKEY DS    CL32                                                             
SVLSTKEY DS    CL32                                                             
SVFLAG   DS    X                                                                
PROSW    DS    X                   USED IN PROTECT/UNPOTECT SUB                 
PRDSW    DS    X                   USED IN VAL REC, SET TO X'41'                
ESTSW    DS    X                   USED IN VAL REC, SET TO X'51'                
RPRDCNT  DS    X                   HOLDS # OF PRD ELEMS IN CLT REC              
RESTCNT  DS    X                   HOLDS # OF EST ELEMS IN CLT REC              
ERRNUM   DS    H                                                                
SAVER2   DS    F                                                                
SAVERE   DS    F                                                                
FLAG     DS    X                   MULTIPLE FLAG                                
PRDLEV   EQU   X'80'                                                            
ESTLEV   EQU   X'40'                                                            
CLTLEV   EQU   X'20'                                                            
*                                                                               
FLAG2    DS    X                   MULTIPLE FLAG                                
KEYCHG   EQU   X'80'                                                            
EFFDINP  EQU   X'40'               USER ENTERED EFFECTIVE DATE                  
*                                                                               
NEWFDATH DS    XL8                 LENGTH OF NEW FIELD                          
NEWFDATA DS    XL32                NEW DATA FIELD                               
*                                                                               
JUSTDATH DS    XL8                 LENGTH OF JUST DATA FIELD                    
JUSTDATA DS    XL32                JUST DATA FIELD                              
*                                                                               
ORIGDLEN DS    XL1                                                              
ORIGDATA DS    XL32                                                             
*                                                                               
ACURFLD  DS    F                                                                
*                                                                               
MYBLOCK  DS    XL100                                                            
TEMPFLD  DS    XL40                                                             
DATELEN  DS    XL1                                                              
PERIOD   DS    CL17                                                             
STARTC   DS    XL2                                                              
ENDC     DS    XL2                                                              
STARTB   DS    XL3                                                              
ENDB     DS    XL3                                                              
STARTMY  DS    XL2                                                              
ENDMY    DS    XL2                                                              
*                                                                               
         PRINT OFF                                                              
         SPACE 1                                                                
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCLT   DS    CL3                                                              
         DS    CL2                                                              
LSTPRD   DS    CL3                                                              
         DS    CL2                                                              
LSTEST   DS    CL3                                                              
       ++INCLUDE DDPERVALD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'111NESFM5B   10/28/10'                                      
         END                                                                    
