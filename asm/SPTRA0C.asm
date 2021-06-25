*          DATA SET SPTRA0C    AT LEVEL 045 AS OF 05/26/17                      
*PHASE T2160CA                                                                  
***********************************************************************         
*                                                                     *         
*        REGISTER USAGE                                               *         
*                                                                     *         
*         R0 - WORK REG                                               *         
*         R1 - WORK REG                                               *         
*         R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR      *         
*         R3 - WORK REG                                               *         
*         R4 - KEY POINTER REG                                        *         
*         R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE      *         
*         R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM  *         
*              FOR DSECT IN VALREC                                    *         
*         R7 -                                                        *         
*         R8 - POINTER TO SPOOLD                                      *         
*         R9 - POINTER TO SYSD                                        *         
*         RA - POINTER TO ATWA                                        *         
*         RB - FIRST BASE                                             *         
*         RC - POINTER TO GEND                                        *         
*         RD - SAVE AREA POINTER                                      *         
*         RE - GOTO1 REG                                              *         
*         RF - GOTO1 REG                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 3                                                                
***********************************************************************         
*                                                                     *         
* LEV 30    MAR29/90 ADD FAX AND COMMENTS LINES                       *         
* LEV 31    MAY23/91 FIX DISPLAY COMMENTS LINES BUG IF FAX PRESENT    *         
* LEV 32    JUN17/92 ADD AGENCY TO RECORD                             *         
* LEV 33    SEP12/94 ADD OPTIONAL 1 TO FAX NUMBER                     *         
* LEV 34    OCT07/94 DON'T DROP NUMBER IF NO AREA CODE                *         
* LEV 35 SMUR MAR18/97 ADD MB= AND ETRANS COMPANY CODE                *         
* LEV 37 SMUR JUL29/04 SOX                                            *         
* LEV 38 SMUR JUN17/13 ADD OPTICA & DELAY, REMOVE MB= & ETRANS FIELDS *         
* LEV 39 SMUR OCT15/13 OPEN UP HOUSE REC FOR NET TRAFFIC FOR OPTICA   *         
* LEV 40 SMUR NOV21/14 ADD YANGAROO (YNG) OPTICA VENDOR               *         
* LEV 41 SMUR JUL24/15 ADD SPOT GENIE (SPG) OPTICA VENDOR             *         
* LEV 42 SMUR SEP09/16 DISABLE OPTICA/ORDER DELAY FIELD               *         
* LEV 43 SMUR MAY25/17 ENABLE OPTICA FIELD FOR EXTREME REACH (ER)     *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2160C PRODUCTION HOUSE ADDRESS DISPLAY, CHANGE, ADD, DC        
               ELETE, LIST'                                                     
T2160C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2160C**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFF LINE REPORT                              
         BE    LR                                                               
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
*                                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
*     VALIDATE KEY ROUTINE                                                      
         SPACE 3                                                                
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
*                                                                               
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VK02     DS    0H                                                               
         LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         XC    PHSE,PHSE                                                        
         LA    R2,TRAPRHH          PRODUCTION HOUSE                             
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      IF LIST, NOT NEEDED                          
         BE    EXIT                                                             
VK10     GOTO1 ANY                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRHKEY,R4                                                        
         MVC   PRHKID,=XL2'0A29'                                                
         MVC   PRHKAM,BAGYMD                                                    
         MVC   PRHKPRH,WORK                                                     
         MVC   PHSE,WORK                                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE 3                                                                
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VRS01                                                            
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VRS01    DS    0H                                                               
         L     R4,AIO                                                           
         MVC   20(2,R4),AGENCY                                                  
         USING PRHKEY,R4                                                        
         MVC   BAGYMD,PRHKAM                                                    
         MVC   PHSE,PRHKPRH                                                     
         DROP  R4                                                               
         MVI   ELCODE,X'10'        ADDRESS PART OF ELEMENT                      
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING PRHDTAEL,R6                                                      
*                                                                               
* FOR ADD, FORMAT DUMMY ELEMENT HEADER                                          
*                                                                               
         MVI   PRHDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   PRHDTALN,PRHDTAX-PRHDTAEL ELEMENT LENGTH                         
*                                                                               
         LA    R2,TRAAL1H          ADDRESS LINE 1                               
         GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   PRHLINE1,WORK                                                    
*                                                                               
         LA    R2,TRAAL2H          ADDRESS LINE 2                               
         GOTO1 ANY                                                              
         MVC   PRHLINE2,WORK                                                    
*                                                                               
         LA    R2,TRAAL3H          ADDRESS LINE 3                               
         GOTO1 ANY                                                              
         MVC   PRHLINE3,WORK                                                    
*                                                                               
         LA    R2,TRAAL4H          ADDRESS LINE 4                               
         XC    PRHLINE4,PRHLINE4                                                
         CLI   5(R2),0             IF INPUT LENGTH ZERO                         
         BE    VR10                NOTHING ENTERED                              
         GOTO1 ANY                                                              
         MVC   PRHLINE4,WORK                                                    
*                                                                               
VR10     GOTO1 ADDELEM                                                          
         EJECT                                                                  
*                                                                               
VR20     DS    0H                  EDIT FAX NUMBER                              
*                                  FAX ELEMENT                                  
VRFAX    MVI   ELCODE,X'20'                                                     
         GOTO1 REMELEM             WILL REMOVE ALL X'20' ELEMENTS               
         LA    R6,ELEM                                                          
         XC    ELEM(30),ELEM                                                    
         USING PRHFAXEL,R6                                                      
         MVI   PRHFAXEL,X'20'                                                   
         MVI   PRHFAXLN,PRHFAXX-PRHFAXEL GET ELEMENT LENGTH                     
*                                                                               
         LA    RF,PRHFTEL          POINT TO TEL NMBR IN RECORD                  
         XC    WORK,WORK           CLEAR TEMP WORK AREA                         
         LA    R2,TRAFAXAH         POINT TO TEL AREA CODE HDR                   
         USING FLDHDRDS,R2         ESTABLISH ADDR'BLTY FOR DSECT                
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BNE   VRFAXA               YES, CHECK IT OUT                           
*                                                                               
         CLI   TRAFAXEH+5,0        MUST ENTER ALL OR NONE                       
         BNE   FAXPARER                                                         
         CLI   TRAFAXNH+5,0        MUST ENTER ALL OR NONE                       
         BNE   FAXPARER                                                         
         CLI   TRAFAXXH+5,0        MUST ENTER ALL OR NONE                       
         BNE   FAXPARER                                                         
         CLI   TRAFAX1H+5,0        MUST ENTER ALL OR NONE                       
         BNE   FAXPARER                                                         
         B     VRFAXXX             NO  OK FAX IS OPTIONAL                       
VRFAXA   CLI   FLDILEN,3           LENGTH EQ 3 ?                                
         BNE   TELNFG              NO-ERROR                                     
         SLR   RE,RE               INIT R14                                     
         IC    RE,FLDILEN          RETRV TEL EXT FIELD LENGTH                   
         BCTR  RE,R0               LESS 1 FOR MACHINE LENGTH                    
         EX    RE,VRFAXMVZ         MOVE CHAR ZERO ZONES TO WORK                 
         EX    RE,VRFAXCLC         DETERMINE IF FIELD IS NUMERIC                
         BNE   TELNTNUM             NO-ERROR                                    
         EX    RE,VRFAXMVC         MOVE TO FIELD TO RECORD                      
         LA    RF,1(RE,RF)         BUMP TEL NMBR RECORD                         
         MVI   0(RF),C'-'          MOVE DASH TO RECORD                          
         LA    RF,1(RF)            BYPASS DASH                                  
VRFAXE   EQU   *                                                                
         LA    R2,TRAFAXEH         POINT TO TEL EXCHANGE                        
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BE    TELMISER            NO-ERROR                                     
         CLI   FLDILEN,3           LENGTH EQ 3 ?                                
         BNE   TELNFG              NO-ERROR                                     
         SLR   RE,RE               INIT R14                                     
         IC    RE,FLDILEN          RETRV TEL EXT FIELD LENGTH                   
         BCTR  RE,R0               LESS 1 FOR MACHINE LENGTH                    
         EX    RE,VRFAXMVZ         MOVE CHAR ZERO ZONES TO WORK                 
         EX    RE,VRFAXCLC         DETERMINE IF FIELD IS NUMERIC                
         BNE   TELNTNUM             NO-ERROR                                    
         EX    RE,VRFAXMVC         MOVE FIELD TO RECORD                         
         LA    RF,1(RE,RF)         BUMP TEL NMBR RECORD                         
         MVI   0(RF),C'-'          MOVE DASH TO RECORD                          
         LA    RF,1(RF)            BYPASS DASH                                  
VRFAXN   EQU   *                                                                
         LA    R2,TRAFAXNH         POINT TO TEL NMBR                            
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BE    TELMISER            NO-ERROR                                     
         CLI   FLDILEN,4           LENGTH EQ 4 ?                                
         BNE   TELNFG              NO-ERROR                                     
         SLR   RE,RE               INIT R14                                     
         IC    RE,FLDILEN          RETRV TEL EXT FIELD LENGTH                   
         BCTR  RE,R0               LESS 1 FOR MACHINE LENGTH                    
         EX    RE,VRFAXMVZ         MOVE CHAR ZERO ZONES TO WORK                 
         EX    RE,VRFAXCLC         DETERMINE IF FIELD IS NUMERIC                
         BNE   TELNTNUM             NO-ERROR                                    
         EX    RE,VRFAXMVC         MOVE FIELD TO RECORD                         
         LA    RF,1(RE,RF)         BUMP TEL NMBR RECORD                         
         MVI   0(RF),C' '          MOVE ' ' TO RECORD                           
         LA    RF,1(RF)            BYPASS ' '                                   
VRFAXEXT EQU   *                                                                
         LA    R2,TRAFAXXH         POINT TO EXTENSION                           
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BE    VRFAX1               NO-TEST FOR 1 PREFIX                        
         CLI   FLDILEN,3           EXTENSION LT 3 ?                             
         BL    TELNFG              YES-ERROR                                    
         SLR   RE,RE               INIT R14                                     
         IC    RE,FLDILEN          RETRV TEL EXT FIELD LENGTH                   
         BCTR  RE,R0               LESS 1 FOR MACHINE LENGTH                    
         EX    RE,VRFAXMVZ         MOVE CHAR ZERO ZONES TO WORK                 
         EX    RE,VRFAXCLC         DETERMINE IF EXTENSION IS NUMERIC            
         BNE   TELNFG              YES-ERROR                                    
         EX    RE,VRFAXMVC         MOVE TO TRATELAC IN RECORD                   
VRFAX1   EQU   *                                                                
         LA    R2,TRAFAX1H         POINT TO PREFIX                              
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BE    VRFAXX               NO-TEST FOR 1 PREFIX                        
         CLI   FLDDATA,C'0'        CHECK TO BE NUMERIC                          
         BL    TELNFG               NO                                          
         CLI   FLDDATA,C'9'        CHECK TO BE NUMERIC                          
         BH    TELNFG               NO                                          
         MVC   PRHFTEL1,FLDDATA                                                 
         B     VRFAXX              EXIT TEL NMBR RTN                            
VRFAXMVZ MVZ   WORK(0),FLDDATA     EXECUTE FOR MOVING ZONES TO WORK             
VRFAXCLC CLC   WORK(0),ZEROC       EXECUTE FOR NUMERIC DATA VALIDATION          
VRFAXMVC MVC   0(0,RF),FLDDATA     EXECUTE FOR STORING TEL NMBR IN REC          
VRFAXX   EQU   *                                                                
         GOTO1 ADDELEM                                                          
*                                                                               
VRFAXXX  DS    0H                                                               
         DROP  R2,R6                                                            
*                                                                               
* X'30' ELEM IS NO LONGER USED                                                  
VR25     MVI   ELCODE,X'30'        MAILBOX ELEMENT - NO LONGER IN USE           
         GOTO1 REMELEM             WILL REMOVE ALL X'30' ELEMENTS               
*                                                                               
         MVI   ELCODE,X'25'                                                     
         GOTO1 REMELEM             AND X'25' ELEMS                              
*                                                                               
         CLI   TRADLYH+5,0         ANY DELAY                                    
         LA    R2,TRAOPCH                                                       
         BE    *+12                 NO                                          
         CLI   5(R2),0             ANY OPTICA                                   
         BE    MISERR              DELAY FOR OPTICA ONLY                        
*                                                                               
         CLI   5(R2),0             ANY OPTICA CODE?                             
         BE    VR35X                NO                                          
*                                                                               
*NOP     CLI   5(R2),3             MUST BE 3 CHARS                              
*****    BNE   OPCERR              ERROR                                        
                                                                                
         GOTO1 ANY                                                              
         CLC   =C'ER ',WORK        EXTREME REACH                                
         BNE   COMERR              INVALID CODE ENTERED                         
*&&DO                                                                           
VK25C    CLI   SPOTNETF,C'N'       SYSTEM NET TRAFFIC                           
         BNE   VK25D                NO, MUST BE SPOT                            
         CLI   TRAMED,C'N'         VALID ONLY FOR MEDIA N                       
         BNE   COMERR                                                           
         B     VK25H                                                            
*                                                                               
VK25D    CLI   TRAMED,C'X'         MEDIA X                                      
         BE    *+12                                                             
         CLI   TRAMED,C'R'         MEDIA R                                      
         BNE   VK25F                                                            
         CLC   =C'YNG',WORK        FOR YANGAROO                                 
         BE    VK25H                                                            
         CLC   =C'SPG',WORK        AND SPOT GENIE ONLY                          
         BE    VK25H                                                            
         BNE   COMERR              INVALID CODE ENTERED                         
*                                                                               
VK25F    CLI   TRAMED,C'T'         CAD FOR MEDIA T ONLY                         
         BNE   COMERR              INVALID CODE ENTERED                         
*&&                                                                             
*                                                                               
VK25H    LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
*                                                                               
         USING PRHOPCEL,R6                                                      
         MVI   PRHOPCEL,X'25'                                                   
         MVI   PRHOPCLN,PRHOPCX-PRHOPCEL GET ELEMENT LENGTH                     
         MVC   PRHOPC,WORK         OPTICA COMPANY CODE                          
*                                                                               
         MVI   PRHDELAY+1,X'3C'    PRESET ORDER DELAY TO 60 MIN                 
         LA    R2,TRADLYH                                                       
         CLI   5(R2),0                                                          
         BE    VR28                                                             
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC                                
         BZ    INVERR                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,PRHDELAY                                                    
*                                                                               
VR28     DS    0H                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
* GET ANY COMMENTS                                                              
*                                                                               
VR35X    LA    R3,1                COMMENT NUMBER                               
         MVI   ELCODE,X'40'                                                     
         GOTO1 REMELEM             WILL REMOVE ALL X'40' ELEMENTS               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PRHCMTEL,R6                                                      
         MVI   PRHCMTEL,X'40'                                                   
         STC   R3,PRHCMTNO         COMMENT NUMBER                               
         LA    R2,TRACMT1H                                                      
         CLI   5(R2),0                                                          
         BE    VR40                                                             
         GOTO1 ANY                                                              
         ZIC   R1,5(R2)            LENGTH                                       
         LA    R0,3(,R1)                                                        
         STC   R0,PRHCMTLN                                                      
         BCTR  R1,0                                                             
         EX    R1,VRMVC                                                         
         GOTO1 ADDELEM                                                          
         LA    R3,1(,R3)           INCREMENT COMMENT NUMBER                     
VR40     LA    R2,TRACMT2H                                                      
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         GOTO1 ANY                                                              
         MVI   PRHCMTEL,X'40'                                                   
         STC   R3,PRHCMTNO         COMMENT NUMBER                               
         ZIC   R1,5(R2)            LENGTH                                       
         LA    R0,3(,R1)                                                        
         STC   R0,PRHCMTLN                                                      
         BCTR  R1,0                                                             
         EX    R1,VRMVC                                                         
         GOTO1 ADDELEM                                                          
VR50     B     DR                                                               
VRMVC    MVC   PRHCMT,WORK                                                      
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
         USING PRHDTAEL,R6                                                      
         MVC   WORK(L'TRAAL1),SPACES                                            
         MVC   WORK(L'PRHLINE1),PRHLINE1                                        
         CLC   TRAAL1,WORK         ADDRESS LINE1                                
         BE    *+14                                                             
         MVC   TRAAL1,WORK                                                      
         OI    TRAAL1H+6,X'80'                                                  
         MVC   WORK(L'TRAAL2),SPACES                                            
         MVC   WORK(L'PRHLINE2),PRHLINE2                                        
         CLC   TRAAL2,WORK         ADDRESS LINE 2                               
         BE    *+14                                                             
         MVC   TRAAL2,WORK                                                      
         OI    TRAAL2H+6,X'80'                                                  
         MVC   WORK(L'TRAAL3),SPACES                                            
         MVC   WORK(L'PRHLINE3),PRHLINE3                                        
         CLC   TRAAL3,WORK         ADDRESS LINE 3                               
         BE    *+14                                                             
         MVC   TRAAL3,WORK                                                      
         OI    TRAAL3H+6,X'80'                                                  
         MVC   WORK(L'TRAAL4),SPACES                                            
         MVC   WORK(L'PRHLINE4),PRHLINE4                                        
         CLC   TRAAL4,WORK         ADDRESS LINE 4                               
         BE    *+14                                                             
         MVC   TRAAL4,WORK                                                      
         OI    TRAAL4H+6,X'80'                                                  
*                                                                               
DRFAX    DS    0H                  DISPLAY FAX                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRFCLR                                                           
         USING PRHFAXEL,R6                                                      
         MVI   BYTE,0                                                           
         CLI   PRHFAXLN,21                                                      
         BL    *+10                                                             
         MVC   BYTE,PRHFTEL1                                                    
*                                                                               
         CLC   TRAFAX1,BYTE        SCRN TEL AREA CODE EQ RECORD ?               
         BE    DRFAXA              YES-TRY AREA CODE                            
         MVC   TRAFAX1,BYTE        PUT FIELD INTO SCREEN                        
         OI    TRAFAX1H+6,X'80'    REWRITE THIS FIELD                           
DRFAXA   EQU   *                                                                
         CLC   TRAFAXA,PRHFTELA    SCRN TEL AREA CODE EQ RECORD ?               
         BE    DRFAXE              YES-TRY EXCHANGE                             
         MVC   TRAFAXA,PRHFTELA    PUT FIELD INTO SCREEN                        
         OI    TRAFAXAH+6,X'80'    REWRITE THIS FIELD                           
DRFAXE   EQU   *                                                                
         CLC   TRAFAXE,PRHFTELE    SCRN TEL EXCHANGE EQ RECORD ?                
         BE    DRFAXN              YES-TRY NUMBER                               
         MVC   TRAFAXE,PRHFTELE    PUT FIELD INTO SCREEN                        
         OI    TRAFAXEH+6,X'80'    REWRITE THIS FIELD                           
DRFAXN   EQU   *                                                                
         CLC   TRAFAXN,PRHFTELN    SCRN TEL NMBR EQ RECORD ?                    
         BE    DRFAXEXT            YES-TRY EXTENSION                            
         MVC   TRAFAXN,PRHFTELN    PUT FIELD INTO SCREEN                        
         OI    TRAFAXNH+6,X'80'    REWRITE THIS FIELD                           
DRFAXEXT EQU   *                                                                
         CLC   TRAFAXX,PRHFTELX    SCRN TEL EXT EQ RECORD ?                     
         BE    DR05                RETURN TO GENCON                             
         MVC   TRAFAXX,PRHFTELX    PUT FIELD INTO SCREEN                        
         OI    TRAFAXXH+6,X'80'    REWRITE THIS FIELD                           
         B     DR05                RETURN TO GENCON                             
*                                                                               
DRFCLR   XC    TRAFAXA,TRAFAXA     CLEAR FAX AREAS                              
         XC    TRAFAXE,TRAFAXE                                                  
         XC    TRAFAXN,TRAFAXN                                                  
         XC    TRAFAXX,TRAFAXX                                                  
         XC    TRAFAX1,TRAFAX1                                                  
         OI    TRAFAXAH+6,X'80'                                                 
         OI    TRAFAXEH+6,X'80'                                                 
         OI    TRAFAXNH+6,X'80'                                                 
         OI    TRAFAXXH+6,X'80'                                                 
         OI    TRAFAX1H+6,X'80'                                                 
*                                                                               
DR05     DS    0H                                                               
         MVC   TRAOPC,SPACES       CLEAR OPTICA FIELD                           
         OI    TRAOPCH+6,X'80'                                                  
         MVC   TRADLY,SPACES       AND DELAY FIELD                              
         OI    TRADLYH+6,X'80'                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'        OPTICA ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   DR08                                                             
         USING PRHOPCEL,R6                                                      
*                                                                               
         MVC   TRAOPC,PRHOPC       DISPLAY OPTICA COMPANY CODE                  
         OI    TRAOPCH+6,X'80'     TRANSMIT                                     
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,PRHDELAY       DELAY TIME                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
                                                                                
         LA    RE,4                                                             
         LA    RF,WORK                                                          
DRDLY    CLI   0(RF),C'0'          GET RID OF LEADING ZEROS                     
         BNE   DRDLY02                                                          
         LA    RF,1(RF)                                                         
         BCT   RE,DRDLY                                                         
         MVI   TRADLY,C'0'                                                      
         B     DRDLYX                                                           
                                                                                
DRDLY02  BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TRADLY(0),0(RF)     MOVE IN DELAY MINUTES                        
                                                                                
DRDLYX   OI    TRADLYH+6,X'80'     TRANSMIT                                     
         DROP  R6                                                               
*                                                                               
DR08     DS    0H                  COMMENTS                                     
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         LA    R5,2                LIST UP TO 2 COMMENTS                        
         LA    R2,TRACMT1H                                                      
         BAS   RE,GETEL                                                         
         BNE   DR40                                                             
         USING PRHCMTEL,R6                                                      
DR10     ZIC   R1,PRHCMTLN                                                      
         SH    R1,=H'4'                                                         
         ZIC   R3,0(R2)                                                         
         SH    R3,=H'9'                                                         
         CR    R3,R1                                                            
         BNL   DR12                                                             
         LR    R1,R3                                                            
DR12     EX    R3,DRMVCA                                                        
         EX    R1,DRMVCB                                                        
         EX    R1,DRCLC                                                         
         BE    DR24                                                             
DR20     EX    R1,DRMVCC                                                        
         OI    6(R2),X'80'                                                      
DR24     BCT   R5,DR30                                                          
         B     EXIT                                                             
DR30     LA    R2,TRACMT2H                                                      
         BAS   RE,NEXTEL                                                        
         BNE   DR50                                                             
         B     DR10                                                             
DR40     CLC   TRACMT1,SPACES                                                   
         BE    DR50                                                             
         MVC   TRACMT1,SPACES                                                   
         OI    TRACMT1H+6,X'80'                                                 
DR50     CLC   TRACMT2,SPACES                                                   
         BE    EXIT                                                             
         MVC   TRACMT2,SPACES                                                   
         OI    TRACMT2H+6,X'80'                                                 
         B     EXIT                                                             
DRCLC    CLC   8(0,R2),WORK                                                     
DRMVCA   MVC   WORK(0),SPACES                                                   
DRMVCB   MVC   WORK(0),PRHCMT                                                   
DRMVCC   MVC   8(0,R2),PRHCMT                                                   
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE 3                                                                
DK       L     R4,AIO                                                           
         USING PRHKEY,R4                                                        
         MVC   BAGYMD,PRHKAM                                                    
         MVC   PHSE,PRHKPRH                                                     
         MVC   WORK(L'TRAPRH),SPACES                                            
         MVC   WORK(L'PRHKPRH),PRHKPRH      MOVE IN PROD HOUSE                  
         CLC   TRAPRH,WORK                                                      
         BE    *+14                                                             
         MVC   TRAPRH,WORK                                                      
         OI    TRAPRHH+6,X'80'     SET ON TRANSMIT BIT                          
         DROP  R4                                                               
         B     EXIT                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     TRAPERR                                                          
*                                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*                                                                               
LR       LA    R4,KEY                                                           
         USING PRHKEY,R4                                                        
         OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR10                NO, GOTO HIGH                                
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDGRTN           HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
* BUILD KEY, AND DO READHI                                                      
         MVC   PRHKID(2),=XL2'0A29'                                             
         MVC   PRHKAM,BAGYMD                                                    
         MVC   PRHKPRH,TRAPRH      NOTE, IS THIS RIGHT                          
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR24                                                             
         CLI   MODE,PRINTREP       OFF LINE REPORT                              
         BNE   EXIT                                                             
         MVC   P(38),=C'NO PRODUCTION HOUSE ADDRESS RECS FOUND'                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
LR22     DS    0H                                                               
* CHECK FILTERS HERE - HOW                                                      
         CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   EXIT                YES                                          
LR24     MVC   SVKEY(2),=XL2'0A29'                                              
         MVC   SVKEY+2(1),BAGYMD                                                
         CLC   SVKEY(3),KEY                                                     
         BL    EXIT                                                             
         BH    LR20                                                             
LR30     GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PRHDTAEL,R6                                                      
         CLI   MODE,PRINTREP       OFF LINE REPORT                              
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         DC    H'0'                MUST BE ON/OFFLINE                           
*                                                                               
* FORMAT OFFLINE REPORT HERE                                                    
*                                                                               
LRR      LA    R5,P                PRINT LINE ADDRESS                           
         MVC   P,SPACES                                                         
         USING PRTLINE,R5                                                       
         MVC   PPRH,PRHKPRH                                                     
         MVC   PALINES,PRHLINE1                                                 
         LA    R5,132(,R5)                                                      
         MVC   PALINES,PRHLINE2                                                 
         LA    R5,132(,R5)                                                      
         MVC   PALINES,PRHLINE3                                                 
         LA    R5,132(,R5)                                                      
         MVC   PALINES,PRHLINE4                                                 
         LA    R5,P                PRINT LINE ADDRESS                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL            GET COMMENTS ELEMENT                         
         BNE   LRR20                                                            
         USING PRHCMTEL,R6                                                      
LRR10    ZIC   R1,PRHCMTLN                                                      
         SH    R1,=H'4'            COMMENT LEN -1                               
         EX    R1,LRRMVC                                                        
         LA    R5,132(,R5)                                                      
         BAS   RE,NEXTEL           MAY BE 2                                     
         BE    LRR10                                                            
         B     LRR20                                                            
*                                                                               
LRRMVC   MVC   PCMTS(0),PRHCMT       EXECUTED                                   
         DROP  R6                                                               
*                                                                               
LRR20    LA    R5,P                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        GET FAX NUMBER                               
         USING PRHFAXEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   LRR30                                                            
         MVC   PFAX,PRHFTEL                                                     
         OC    PRHFTELX,PRHFTELX   CHK FOR EXTENSION                            
         BZ    LRR30                                                            
         MVC   PEXT(3),=C'EXT'                                                  
         MVC   PEXT+4(5),PRHFTELX                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
LRR30    DS    0H                                                               
*                                                                               
         LA    R5,P                                                             
         LA    R5,132(R5)                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'        OPTICA COMPANY CODE                          
         USING PRHOPCEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   LRR40                                                            
         MVC   PFAX(7),=C'OPTICA='                                              
         MVC   PFAX+8(3),PRHOPC                                                 
*                                                                               
LRR40    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
*                                                                               
         USING PRHDTAEL,R6                                                      
LRL      LA    R5,LISTAR           ADDRESS OF WORK AREA                         
         MVC   LISTAR,SPACES                                                    
         USING LSTLINE,R5                                                       
         MVC   LPRH,PRHKPRH                                                     
         MVC   LALINE1,PRHLINE1    ADDRESS LINE 1                               
         MVC   LALINE2,PRHLINE2    ADDRESS LINE 2                               
         MVC   LALINE3,PRHLINE3    ADDRESS LINE 3 (15 CHAR ONLY)                
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
         DROP  R4,R5,R6                                                         
*                                                                               
HDGRTN   NTR1                                                                   
         MVC   H2+9(L'QMED),QMED                                                
         MVC   H2+11(L'MEDNM),MEDNM                                             
         B     EXIT                                                             
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
ZIPERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
TELMISER EQU   *                                                                
MISERR   MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
FAXPARER EQU   *                                                                
         XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'FAXPARMS),FAXPARMS TENTERED PART OF NUMBER             
         B     USRERR              PROCESS USER ERROR                           
TELNFG   EQU   *                                                                
         XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'TELINVAL),TELINVAL TEL NMBR INVALID                    
         B     USRERR              PROCESS USER ERROR                           
OPCERR   EQU   *                                                                
         XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'OPCLENM),OPCLENM  CODE MUST BE 3 CHARS                 
         B     USRERR              PROCESS USER ERROR                           
COMERR   EQU   *                                                                
         XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'COMCODE),COMCODE  COMPANY CODE ERR                     
         B     USRERR              PROCESS USER ERROR                           
TELNTNUM EQU   *                                                                
         XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'TELNNUM),TELNNUM TEL NMBR MUST BE NUMERIC              
         B     USRERR              PROCESS USER ERROR                           
USRERR   GOTO1 ERREX2              USER ERROR MESSAGE ROUTINE                   
         B     EXIT                                                             
*                                                                               
FAXPARMS DC    C'* ERROR * MUST ENTER ALL OF TEL, NOT PARTIAL *'                
TELINVAL DC    C'* ERROR * TELEPHONE NUMBER INVALID. PLEASE RE-ENTER *'         
TELNNUM  DC    C'* ERROR * TELEPHONE NUMBER MUST BE NUMERIC *'                  
OPCLENM  DC    C'* ERROR * OPTICA CODE MUST BE 3 CHARS *'                       
COMCODE  DC    C'* ERROR * INVALID COMPANY CODE'                                
ZEROC    DC    10C'0'              CHARACTER ZEROS                              
         PRINT NOGEN                                                            
         EJECT                                                                  
         LTORG                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,27,C'PRODUCTION HOUSE LIST'                                   
         SSPEC H1,58,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,27,C'---------------------'                                   
         SSPEC H2,58,AGYADD                                                     
         SSPEC H4,58,REPORT                                                     
         SSPEC H4,70,RUN                                                        
         SSPEC H5,58,REQUESTOR                                                  
         SSPEC H5,88,PAGE                                                       
         SSPEC H8,3,C'HOUSE'                                                    
         SSPEC H8,11,C'ADDRESS LINES'                                           
         SSPEC H8,37,C'FAX NUMBER'                                              
         SSPEC H8,61,C'COMMENTS'                                                
         SSPEC H9,3,C'-----'                                                    
         SSPEC H9,11,C'-------------'                                           
         SSPEC H9,37,C'----------'                                              
         SSPEC H9,61,C'--------'                                                
         DC    X'00'               END MARKER FOR SSPEC                         
PRTLINE  DSECT                     OFFLINE REPORT                               
         DS    CL2                                                              
PPRH     DS    CL6                                                              
         DS    CL2                                                              
PALINES  DS    CL24                                                             
         DS    CL2                                                              
PFAX     DS    CL12                                                             
         DS    CL1                                                              
PEXT     DS    CL9                                                              
         DS    CL2                                                              
PCMTS    DS    CL24                                                             
LSTLINE  DSECT                     ONLINE LIST                                  
LPRH     DS    CL6                                                              
         DS    CL2                                                              
LALINE1  DS    CL24                                                             
         DS    C                                                                
LALINE2  DS    CL24                                                             
         DS    C                                                                
LALINE3  DS    CL15                                                             
         EJECT                                                                  
       ++INCLUDE SPTRPRH                                                        
         EJECT                                                                  
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAFCD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
PHSE     DS    CL6                                                              
MBOXEX   EQU   62                  MAILBOX EXCHANGE                             
       ++INCLUDE FLDHDRDS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPTRA0C   05/26/17'                                      
         END                                                                    
