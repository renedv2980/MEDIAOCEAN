*          DATA SET SPTRA08    AT LEVEL 057 AS OF 12/19/08                      
*PHASE T21608C                                                                  
         TITLE 'T21608 - AGENCY CONTACT REC. DISPLAY CHANGE, ADD, DELETC        
               E, LIST'                                                         
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 -                                                            
*             AIO3 -                                                            
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - WORK REG                                                          
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
*                                                                               
*  LEV 41    NOV16/87 ONLINE LIST STOPS AT 2ND SCREEN                           
*  LEV 50    AUG11/01 ADD FAX                                                   
*  LEV 51 BGRI OCT10/01 ADD CLIENT SECURITY FOR LISTS                           
*  LEV 52 SMUR OCT02/02 ADD E-MAIL ELEMENT                                      
*  LEV 53 SMUR OCT25/02 FIX CLIENT SECURITY IN FIND CLIENT ROUTINE              
*  LEV 54 SMUR JUL28/04 SOX                                                     
*                                                                               
***********************************************************************         
         EJECT                                                                  
         PRINT NOGEN                                                            
T21608   CSECT                                                                  
         NMOD1 0,T21608**                                                       
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
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,RECDEL         BEFORE DELETE RECORD                         
         BNE   EXIT                                                             
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
EXIT     XIT1                                                                   
         SPACE 3                                                                
*     VALIDATE KEY ROUTINE                                                      
         SPACE                                                                  
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
         SPACE                                                                  
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     DS    0H                                                               
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
         LA    R2,TRACLTH                                                       
         XC    BCLT,BCLT                                                        
         XC    QCLT,QCLT                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK20                YES, NOT NEEDED                              
         SPACE                                                                  
VK10     GOTO1 VALICLT                                                          
         SPACE                                                                  
VK20     LA    R2,TRACONH                                                       
         CLI   5(R2),0             ENTERED ?                                    
         BNE   VK30                                                             
         CLI   ACTNUM,ACTLIST      LIST                                         
         BNE   CONMISER                                                         
         B     VK40                                                             
VK30     GOTO1 ANY                                                              
         MVC   SVCNT,WORK                                                       
         SPACE                                                                  
* SET UP KEY FOR GENCON *                                                       
         SPACE                                                                  
VK40     LA    R4,KEY                                                           
         XC    KEY(13),KEY                                                      
         USING CNTKEY,R4                                                        
         MVC   CNTKID,=XL2'0A36'                                                
         MVC   CNTKAM,BAGYMD                                                    
         MVC   CNTKCLT,BCLT        MOVE IN CLIENT                               
         MVC   CNTKNAME,TRACON     SAVE CONTACT NAME                            
         B     EXIT                RETURN TO GENCON                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE (IT IS ASSUMED THAT GENCON HAS FOUND RECORD           
*                          OF CNTKEY IN VK ROUTINE)                             
         SPACE                                                                  
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VRS01    DS    0H                                                               
         L     R4,AIO              RETRV RECORD POINTER                         
         USING CNTKEY,R4                                                        
         CLC   BAGYMD,CNTKAM       AGENCY/MEDIAS SAME ?                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         CLC   BCLT,CNTKCLT        CLIENTS SAME                                 
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVC   CNTAGYA,AGENCY                                                   
         DROP  R4                                                               
         SPACE                                                                  
         MVI   ELCODE,X'10'        CONTACT NAME DATA ELEMENT                    
         GOTO1 REMELEM             WILL REMOVE ALL X'40' ELEMENTS               
         LA    R6,ELEM                                                          
         USING CNTDTAEL,R6                                                      
         MVI   CNTDTAEL,X'10'                                                   
         MVI   CNTDTALN,CNTDTAX-CNTDTAEL GET ELEMENT LENGTH                     
         LA    R2,TRACONMH         CONTACT NAME                                 
         CLI   5(R2),0                                                          
         BE    CNMMISER                                                         
         MVC   CNTNAME,TRACONM                                                  
         SPACE                                                                  
         XC    CNTTEL,CNTTEL       CLEAR TEL NMBR IN RECORD                     
         LA    RF,CNTTEL           POINT TO TEL NMBR IN RECORD                  
         XC    WORK,WORK           CLEAR TEMP WORK AREA                         
         LA    R2,TRATELAH         POINT TO TEL AREA CODE HDR                   
         USING FLDHDRDS,R2         ESTABLISH ADDR'BLTY FOR DSECT                
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BE    TELMISER            NO-ERROR                                     
         CLI   FLDILEN,3           LENGTH EQ 3 ?                                
         BNE   TELNFG              NO-ERROR                                     
         SLR   RE,RE               INIT R14                                     
         IC    RE,FLDILEN          RETRV TEL EXT FIELD LENGTH                   
         BCTR  RE,R0               LESS 1 FOR MACHINE LENGTH                    
         EX    RE,VRTELMVZ         MOVE CHAR ZERO ZONES TO WORK                 
         EX    RE,VRTELCLC         DETERMINE IF FIELD IS NUMERIC                
         BNE   TELNTNUM            YES-ERROR                                    
         EX    RE,VRTELMVC         MOVE TO FIELD TO RECORD                      
         LA    RF,1(RE,RF)         BUMP TEL NMBR RECORD                         
         MVI   0(RF),C'-'          MOVE DASH TO RECORD                          
         LA    RF,1(RF)            BYPASS DASH                                  
VRTELE   EQU   *                                                                
         LA    R2,TRATELEH         POINT TO TEL EXCHANGE                        
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BE    TELMISER            NO-ERROR                                     
         CLI   FLDILEN,3           LENGTH EQ 3 ?                                
         BNE   TELNFG              NO-ERROR                                     
         SLR   RE,RE               INIT R14                                     
         IC    RE,FLDILEN          RETRV TEL EXT FIELD LENGTH                   
         BCTR  RE,R0               LESS 1 FOR MACHINE LENGTH                    
         EX    RE,VRTELMVZ         MOVE CHAR ZERO ZONES TO WORK                 
         EX    RE,VRTELCLC         DETERMINE IF FIELD IS NUMERIC                
         BNE   TELNTNUM            YES-ERROR                                    
         EX    RE,VRTELMVC         MOVE FIELD TO RECORD                         
         LA    RF,1(RE,RF)         BUMP TEL NMBR RECORD                         
         MVI   0(RF),C'-'          MOVE DASH TO RECORD                          
         LA    RF,1(RF)            BYPASS DASH                                  
VRTELN   EQU   *                                                                
         LA    R2,TRATELNH         POINT TO TEL NMBR                            
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BE    TELMISER            NO-ERROR                                     
         CLI   FLDILEN,4           LENGTH EQ 4 ?                                
         BNE   TELNFG              NO-ERROR                                     
         SLR   RE,RE               INIT R14                                     
         IC    RE,FLDILEN          RETRV TEL EXT FIELD LENGTH                   
         BCTR  RE,R0               LESS 1 FOR MACHINE LENGTH                    
         EX    RE,VRTELMVZ         MOVE CHAR ZERO ZONES TO WORK                 
         EX    RE,VRTELCLC         DETERMINE IF FIELD IS NUMERIC                
         BNE   TELNTNUM            YES-ERROR                                    
         EX    RE,VRTELMVC         MOVE FIELD TO RECORD                         
         LA    RF,1(RE,RF)         BUMP TEL NMBR RECORD                         
         MVI   0(RF),C' '          MOVE ' ' TO RECORD                           
         LA    RF,1(RF)            BYPASS ' '                                   
VRTELEXT EQU   *                                                                
         LA    R2,TRATELXH         POINT TO TEL NMBR                            
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BE    VRTELX              NO-EXIT TEL NUM RTN                          
         CLI   FLDILEN,3           EXTENTION LT 3 ?                             
         BL    TELNFG              YES-ERROR                                    
         SLR   RE,RE               INIT R14                                     
         IC    RE,FLDILEN          RETRV TEL EXT FIELD LENGTH                   
         BCTR  RE,R0               LESS 1 FOR MACHINE LENGTH                    
         EX    RE,VRTELMVZ         MOVE CHAR ZERO ZONES TO WORK                 
         EX    RE,VRTELCLC         DETERMINE IF EXTENTION IS NUMERIC            
         BNE   TELNFG              YES-ERROR                                    
         EX    RE,VRTELMVC         MOVE TO TRATELAC IN RECORD                   
         B     VRTELX              EXIT TEL NMBR RTN                            
VRTELMVZ MVZ   WORK(0),FLDDATA     EXECUTE FOR MOVING ZONES TO WORK             
VRTELCLC CLC   WORK(0),ZEROC       EXECUTE FOR NUMERIC DATA VALIDATION          
VRTELMVC MVC   0(0,RF),FLDDATA     EXECUTE FOR STORING TEL NMBR IN REC          
VRTELX   EQU   *                                                                
         GOTO1 ADDELEM                                                          
*                                  FAX ELEMENT                                  
VRFAX    MVI   ELCODE,X'20'                                                     
         GOTO1 REMELEM             WILL REMOVE ALL X'20' ELEMENTS               
         LA    R6,ELEM                                                          
         XC    ELEM(30),ELEM                                                    
         USING CNTFAXEL,R6                                                      
         MVI   CNTFAXEL,X'20'                                                   
         MVI   CNTFAXLN,CNTFAXX-CNTFAXEL GET ELEMENT LENGTH                     
         XC    CNTFTEL,CNTFTEL     CLEAR FTEL NMBR IN RECORD                    
         LA    RF,CNTFTEL          POINT TO TEL NMBR IN RECORD                  
         XC    WORK,WORK           CLEAR TEMP WORK AREA                         
         LA    R2,TRAFAXAH         POINT TO TEL AREA CODE HDR                   
         USING FLDHDRDS,R2         ESTABLISH ADDR'BLTY FOR DSECT                
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BE    VRFAXXX             NO  OK FAX IS OPTIONAL                       
         CLI   FLDILEN,3           LENGTH EQ 3 ?                                
         BNE   TELNFG              NO-ERROR                                     
         SLR   RE,RE               INIT R14                                     
         IC    RE,FLDILEN          RETRV TEL EXT FIELD LENGTH                   
         BCTR  RE,R0               LESS 1 FOR MACHINE LENGTH                    
         EX    RE,VRFAXMVZ         MOVE CHAR ZERO ZONES TO WORK                 
         EX    RE,VRFAXCLC         DETERMINE IF FIELD IS NUMERIC                
         BNE   TELNTNUM            YES-ERROR                                    
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
         BNE   TELNTNUM            YES-ERROR                                    
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
         BNE   TELNTNUM            YES-ERROR                                    
         EX    RE,VRFAXMVC         MOVE FIELD TO RECORD                         
         LA    RF,1(RE,RF)         BUMP TEL NMBR RECORD                         
         MVI   0(RF),C' '          MOVE ' ' TO RECORD                           
         LA    RF,1(RF)            BYPASS ' '                                   
VRFAXEXT EQU   *                                                                
         LA    R2,TRAFAXXH         POINT TO TEL NMBR                            
         CLI   FLDILEN,0           WAS IT ENTERED ?                             
         BE    VRFAXX              NO-EXIT TEL NUM RTN                          
         CLI   FLDILEN,3           EXTENTION LT 3 ?                             
         BL    TELNFG              YES-ERROR                                    
         SLR   RE,RE               INIT R14                                     
         IC    RE,FLDILEN          RETRV TEL EXT FIELD LENGTH                   
         BCTR  RE,R0               LESS 1 FOR MACHINE LENGTH                    
         EX    RE,VRFAXMVZ         MOVE CHAR ZERO ZONES TO WORK                 
         EX    RE,VRFAXCLC         DETERMINE IF EXTENTION IS NUMERIC            
         BNE   TELNFG              YES-ERROR                                    
         EX    RE,VRFAXMVC         MOVE TO TRATELAC IN RECORD                   
         B     VRFAXX              EXIT TEL NMBR RTN                            
VRFAXMVZ MVZ   WORK(0),FLDDATA     EXECUTE FOR MOVING ZONES TO WORK             
VRFAXCLC CLC   WORK(0),ZEROC       EXECUTE FOR NUMERIC DATA VALIDATION          
VRFAXMVC MVC   0(0,RF),FLDDATA     EXECUTE FOR STORING TEL NMBR IN REC          
VRFAXX   EQU   *                                                                
         GOTO1 ADDELEM                                                          
*                                                                               
VRFAXXX  DS    0H                                                               
*                                  E-MAIL ELEMENT                               
         MVI   ELCODE,X'30'                                                     
         GOTO1 REMELEM             WILL REMOVE ALL X'30' ELEMENTS               
*                                                                               
         CLI   TRAMAILH+5,0        WAS E-MAIL ENTERED                           
         BE    DR                   NO,EXIT                                     
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING CNTEMLEL,R6                                                      
         MVI   CNTEMLEL,X'30'                                                   
         MVI   CNTEMLLN,62         ELEMENT LENGTH                               
*                                                                               
         MVC   CNTEMLAD,TRAMAIL                                                 
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE                                                                  
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NAME ELEMENT MUST EXIST                      
         USING CNTDTAEL,R6                                                      
DRCONAM  EQU   *                                                                
         CLC   TRACONM,CNTNAME                                                  
         BE    DRTELA                                                           
         MVC   TRACONM,CNTNAME     PUT CONTACT NAME INTO SCREEN                 
         OI    TRACONMH+6,X'80'    REWRITE THIS FIELD                           
DRTELA   EQU   *                                                                
         CLC   TRATELA,CNTTELA     SCRN TEL AREA CODE EQ RECORD ?               
         BE    DRTELE              YES-TRY EXCHANGE                             
         MVC   TRATELA,CNTTELA     PUT FIELD INTO SCREEN                        
         OI    TRATELAH+6,X'80'    REWRITE THIS FIELD                           
DRTELE   EQU   *                                                                
         CLC   TRATELE,CNTTELE     SCRN TEL EXCHANGE EQ RECORD ?                
         BE    DRTELN              YES-TRY NUMBER                               
         MVC   TRATELE,CNTTELE     PUT FIELD INTO SCREEN                        
         OI    TRATELEH+6,X'80'    REWRITE THIS FIELD                           
DRTELN   EQU   *                                                                
         CLC   TRATELN,CNTTELN     SCRN TEL NMBR EQ RECORD ?                    
         BE    DRTELEXT            YES-TRY EXTENTION                            
         MVC   TRATELN,CNTTELN     PUT FIELD INTO SCREEN                        
         OI    TRATELNH+6,X'80'    REWRITE THIS FIELD                           
DRTELEXT EQU   *                                                                
         CLC   TRATELX,CNTTELEX    SCRN TEL EXT EQ RECORD ?                     
         BE    DRFAX               YES-CHECK FOR FAX ELEM                       
         MVC   TRATELX,CNTTELEX    PUT FIELD INTO SCREEN                        
         OI    TRATELXH+6,X'80'    REWRITE THIS FIELD                           
         B     DRFAX               CHECK FOR FAX ELEM                           
*                                                                               
DRFAX    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRFCLR                                                           
         USING CNTFAXEL,R6                                                      
DRFAXA   EQU   *                                                                
         CLC   TRAFAXA,CNTFTELA    SCRN TEL AREA CODE EQ RECORD ?               
         BE    DRFAXE              YES-TRY EXCHANGE                             
         MVC   TRAFAXA,CNTFTELA    PUT FIELD INTO SCREEN                        
         OI    TRAFAXAH+6,X'80'    REWRITE THIS FIELD                           
DRFAXE   EQU   *                                                                
         CLC   TRAFAXE,CNTFTELE    SCRN TEL EXCHANGE EQ RECORD ?                
         BE    DRFAXN              YES-TRY NUMBER                               
         MVC   TRAFAXE,CNTFTELE    PUT FIELD INTO SCREEN                        
         OI    TRAFAXEH+6,X'80'    REWRITE THIS FIELD                           
DRFAXN   EQU   *                                                                
         CLC   TRAFAXN,CNTFTELN    SCRN TEL NMBR EQ RECORD ?                    
         BE    DRFAXEXT            YES-TRY EXTENTION                            
         MVC   TRAFAXN,CNTFTELN    PUT FIELD INTO SCREEN                        
         OI    TRAFAXNH+6,X'80'    REWRITE THIS FIELD                           
DRFAXEXT EQU   *                                                                
         CLC   TRAFAXX,CNTFTELX    SCRN TEL EXT EQ RECORD ?                     
         BE    DREMAIL             RETURN TO GENCON                             
         MVC   TRAFAXX,CNTFTELX    PUT FIELD INTO SCREEN                        
         OI    TRAFAXXH+6,X'80'    REWRITE THIS FIELD                           
         B     DREMAIL             RETURN TO GENCON                             
*                                                                               
DRFCLR   XC    TRAFAXA,TRAFAXA     CLEAR FAX AREAS                              
         XC    TRAFAXE,TRAFAXE                                                  
         XC    TRAFAXN,TRAFAXN                                                  
         XC    TRAFAXX,TRAFAXX                                                  
         OI    TRAFAXAH+6,X'80'                                                 
         OI    TRAFAXEH+6,X'80'                                                 
         OI    TRAFAXNH+6,X'80'                                                 
         OI    TRAFAXXH+6,X'80'                                                 
*                                                                               
DREMAIL  OI    TRAMAILH+6,X'80'    TRANSMIT EMAIL FIELD                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         XC    TRAMAIL,TRAMAIL     CLEAR E-MAIL FIELD                           
         B     DRX                                                              
*                                                                               
         USING CNTEMLEL,R6                                                      
*                                                                               
         MVC   TRAMAIL,CNTEMLAD    E-MAIL ADDRESS                               
*                                                                               
DRX      B     EXIT                                                             
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE                                                                  
DK       EQU   *                                                                
         SPACE                                                                  
DKMED    EQU   *                                                                
         LA    R2,TRAMEDH                                                       
         L     R4,AIO                                                           
         USING CNTKEY,R4                                                        
         MVC   WORK(L'TRAMED),SPACES                                            
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    DKMEDX              YES-EXIT MEDIA ROUTINE                       
         MVC   TRAMED,WORK         MOVE IN MEDIA                                
         OI    TRAMEDH+6,X'80'     SET ON TRANSMIT BIT                          
         MVC   BAGYMD,CNTKAM       MOVE IN MEDIA                                
DKMEDX   EQU   *                                                                
         SPACE 2                                                                
DKCLT    EQU   *                                                                
         LA    R2,TRACLTH                                                       
         MVC   WORK(L'TRACLT),SPACES                                            
         CLI   CNTKCLT,0                                                        
         BE    DKCLTCLC                                                         
         GOTO1 CLUNPK,DMCB,CNTKCLT,QCLT                                         
         MVC   BCLT,CNTKCLT                                                     
         MVC   WORK(L'QCLT),QCLT                                                
DKCLTCLC EQU   *                                                                
         CLC   TRACLT,WORK                                                      
         BE    DKCLTX                                                           
         MVC   TRACLT,WORK          MOVE IN CLIENT                              
         OI    TRACLTH+6,X'80'      SET ON TRANSMIT BIT                         
DKCLTX   EQU   *                                                                
         SPACE 2                                                                
DKCON    EQU   *                                                                
         CLC   TRACON,CNTKNAME                                                  
         BE    DKCONX                                                           
         MVC   TRACON,CNTKNAME                                                  
         OI    TRACONH+6,X'80'                                                  
DKCONX   EQU   *                                                                
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE 3                                                                
LR       LA    R4,KEY                                                           
         USING CNTKEY,R4                                                        
         SPACE                                                                  
         MVC   SVBCLT,BCLT         SAVE CLIENT CODE                             
         SPACE                                                                  
         OC    KEY,KEY             FIRST TIME IN ?                              
         BZ    *+12                                                             
         MVI   REVLDCLT,C'Y'       FORCE REVALIDATE CLIENT                      
         B     LR10                RE-POINT TO LAST RECORD READ                 
         SPACE                                                                  
         MVI   REVLDCLT,C'N'       INIT REVALIDATE CLT                          
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDGRTN           HEADING ROUTINE FOR REPORT$$$$$$$$           
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
* BUILD KEY, AND DO READHI                                                      
         MVC   CNTKID(2),=XL2'0A36'                                             
         MVC   CNTKAM(3),BAGYMD & BCLT                                          
         MVC   CNTKNAME,TRACON                                                  
         SPACE                                                                  
* SET UP COMPARE KEY TO CONTROL END OF LIST                                     
         SPACE                                                                  
         GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEY(3),KEYSAVE      ANY RECS FOR THIS AG/MED?                    
         BE    LR30                                                             
         CLI   MODE,PRINTREP                                                    
         BNE   EXIT                                                             
         MVC   P(31),=C'NO AGENCY CONTACT RECORDS FOUND'                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEY(3),KEYSAVE      ANY RECS FOR THIS AG/MED?                    
         BE    LR30                SAVE KEY FOR DEGUGGING PURPOSES              
         B     EXIT                                                             
LR20     DS   0H                                                                
         LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
         CLC   KEY(3),KEYSAVE      RECS FOR THIS AGY/MED ?                      
         BNE   EXIT                YES                                          
         SPACE                                                                  
LR30     DS   0H                                                                
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=XL2'0A36' TO INSURE                                    
         MVC   SVKEY+2(1),BAGYMD   BUILD KEY                                    
         CLC   SVKEY(3),KEY        ONLY WANTED KEYS ARE PASSED                  
         BL    EXIT                                                             
         OC    SVBCLT,SVBCLT       CLIENT ENTERED ?                             
         BZ    LR40                                                             
         CLC   CNTKCLT,SVBCLT      CLIENT'S EQ ?                                
         BNE   EXIT                 NO, DONE                                    
         B     LR44                                                             
         SPACE                                                                  
LR40     DS   0H                                                                
         CLC   KEYSAVE+3(2),KEY+3  SAME CLIENT                                  
         BNE   LR42                                                             
         CLI   REVLDCLT,C'Y'       NEED TO REVALIDATE CLT                       
         BNE   LR44                 NO                                          
         MVI   REVLDCLT,C'N'                                                    
         SPACE                                                                  
LR42     BAS   RE,FCLT             GO SEE IF CLIENT OKAY (SECURITY              
         BNE   LR10                                                             
         SPACE                                                                  
LR44     GOTO1 GETREC                                                           
         L     R4,AIO              RETRV I/O ADDR. OF REC                       
         LR    R6,R4               INIT GETEL ELEMENT POINTER                   
         USING CNTDTAEL,R6         ESTAB.ADDR'BLTY TO DATA ELEM                 
         MVI   ELCODE,X'10'        PUT ELEMENT CODE IN CODE WORK                
         BAS   RE,GETEL            GET DESIRED ELEMENT                          
         BE    *+6                 FOUND-BYPASS CRASH                           
         DC    H'0'                STOP-THIS SHOULDN'T HAPPEN                   
         SPACE                                                                  
         CLI   MODE,LISTRECS       LIST RECORDS ?                               
         BE    LRL                 YES-GO TO IT                                 
         CLI   MODE,PRINTREP       PRINT REPORT ?                               
         BE    LRR                 YES-GO TO IT                                 
         DC    H'0'                STOP-THIS SHOULDN'T HAPPEN                   
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE                                                    
         SPACE                                                                  
LRR      EQU   *                                                                
         MVC   P1,SPACES           INIT PRINT LINE                              
LRR12    L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         USING CNTDTAEL,R6                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
         SPACE                                                                  
LRL      EQU   *                   R6 IS POINTING TO DESIRED ELEMENT            
         MVC   LISTAR,SPACES                                                    
         OC    CNTKCLT,CNTKCLT                                                  
         BZ    LRL10                                                            
         GOTO1 CLUNPK,DMCB,CNTKCLT,LCLT GET CLIENT                              
         SPACE                                                                  
LRL10    MVC   LCON,CNTKNAME       GET CONTACT                                  
         MVC   LCONNAME,CNTNAME    GET CONTACT NAME                             
         MVC   LTEL,CNTTEL         GET TELEPHONE NMBR                           
         OC    CNTTELEX,CNTTELEX   IS THERE A TELEPHONE NUM EXTENSION ?         
         BZ    LRL20               NO-THEN GO LIST WHAT YOU HAVE                
         MVI   LTELX,C'X'          MOVE AN 'X' TO SCREEN                        
         MVC   LTELEX,CNTTELEX     MOVE EXTENTION TO SCREEN                     
LRL20    EQU   *                                                                
         L     R6,AIO              CHECK FOR FAX ELEM                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRL30                                                            
         USING CNTFAXEL,R6                                                      
         MVC   LFAX,CNTFTEL        FAX NUMBER                                   
         OC    CNTFTELX,CNTFTELX   IS THERE A TELEPHONE NUM EXTENSION ?         
         BZ    LRL30               NO-THEN GO LIST WHAT YOU HAVE                
         MVI   LFAXX,C'X'          MOVE AN 'X' TO SCREEN                        
         MVC   LFAXEX,CNTFTELX     MOVE EXTENTION TO SCREEN                     
LRL30    GOTO1 LISTMON             PUT LINE IN SCREEN                           
         B     LR20                GO READ NEXT RECORD                          
         EJECT                                                                  
* FIND CLIENT HEADER AND CHECK SECURITY                                         
         SPACE                                                                  
FCLT     NTR1                                                                   
         SPACE                                                                  
         OC    CNTKCLT,CNTKCLT                                                  
         BZ    FCLT50                                                           
         SPACE                                                                  
* SAVE CURRENT RECORD & KEY                                                     
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         GOTO1 CLUNPK,DMCB,CNTKCLT,FLD                                          
         DROP  R4                                                               
         SPACE                                                                  
         LA    R2,FLDH                                                          
         SPACE                                                                  
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         MVI   ERROR,0                                                          
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FCLT10              IF NOT FOUND, JUST GO ON                     
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    EXIT                                                             
         SPACE                                                                  
FCLT10   MVI   KEY+5,X'FF'         NEXT CLIENT                                  
         SPACE                                                                  
         LTR   RB,RB               SET CC NE                                    
         B     EXIT                                                             
         SPACE                                                                  
FCLT50   SR    R2,R2                                                            
         SR    R3,R3                                                            
         L     RE,ASVCLIST                                                      
         LA    RF,880                                                           
         MVCL  RE,R2                                                            
         MVC   CLTNM,=CL20'NON-CLIENT SPECIFIC'                                 
         MVC   QCLT,SPACES                                                      
         SPACE                                                                  
         CR    RB,RB               SET EQ TO ACCEPT RECS                        
         B     EXIT                                                             
         EJECT                                                                  
* PRINT REPORT HEADINGS *                                                       
         SPACE 3                                                                
HDGRTN   NTR1                                                                   
         MVC   H2+8(L'QMED),QMED                                                
         MVC   H2+10(L'MEDNM),MEDNM                                             
         B     EXIT                                                             
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
CONMISER EQU   *                                                                
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
CNMMISER EQU   *                                                                
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
TELMISER EQU   *                                                                
         MVI   ERROR,MISSING                                                    
         SPACE                                                                  
TRAPERR  GOTO1 ERREX               STANDARD ERROR MESSAGE ROUTINE               
         B     EXIT                                                             
         SPACE                                                                  
TELNFG   EQU   *                                                                
         XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'TELINVAL),TELINVAL TEL NMBR INVALID                    
         B     USRERR              PROCESS USER ERROR                           
TELNTNUM EQU   *                                                                
         XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'TELNNUM),TELNNUM TEL NMBR MUST BE NUMERIC              
         B     USRERR              PROCESS USER ERROR                           
USRERR   GOTO1 ERREX2              USER ERROR MESSAGE ROUTINE                   
         B     EXIT                                                             
         SPACE 3                                                                
TELINVAL DC    C'* ERROR * TELEPHONE NUMBER INVALID. PLEASE RE-ENTER *'         
TELNNUM  DC    C'* ERROR * TELEPHONE NUMBER MUST BE NUMERIC *'                  
ZEROC    DC    10C'0'              CHARACTER ZEROS                              
         LTORG                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,32,C'AGENCY CONTACT LIST'                                     
         SSPEC H1,60,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,32,C'--------------'                                          
         SSPEC H2,60,AGYADD                                                     
         SSPEC H4,60,REPORT                                                     
         SSPEC H4,72,RUN                                                        
         SSPEC H5,60,REQUESTOR                                                  
         SSPEC H5,90,PAGE                                                       
         SSPEC H8,3,C'CLIENT'                                                   
         SSPEC H8,15,C'COPY'                                                    
         SSPEC H8,30,C'RRRRRRRRRRR'                                             
         SSPEC H8,55,C'SSSSSSSSSSS'                                             
         SSPEC H9,5,C'---'                                                      
         SSPEC H9,15,C'JJJJ'                                                    
         SSPEC H9,30,C'---------------'                                         
         SSPEC H9,55,C'---------------'                                         
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
       ++INCLUDE SPTRAGYCON                                                     
       ++INCLUDE DDSPOOLD                                                       
SPOOLD   DSECT                                                                  
         ORG   P1                  REDEFINE PRINT LINE                          
         DS    CL4                                                              
PCLT     DS    CL3                                                              
         DS    CL9                                                              
PRCDE    DS    CL1                                                              
         DS    CL12                                                             
PRLMTS   DS    CL75                                                             
         ORG                                                                    
       ++INCLUDE DDSPLWORKD                                                     
GEND     DSECT                                                                  
         ORG   LISTAR              REDEFINE SCREEN LIST LINE                    
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LCON     DS    CL8                                                              
         DS    CL1                                                              
LCONNAME DS    CL24                                                             
         DS    CL1                                                              
LTEL     DS    CL12                                                             
LTELX    DS    C'X'                                                             
LTELEX   DS    CL5                                                              
         DS    CL1                                                              
LFAX     DS    CL12                                                             
LFAXX    DS    C'X'                                                             
LFAXEX   DS    CL5                                                              
         ORG                                                                    
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF8D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
SVCNT    DS    CL8             CONTACT SAVE                                     
SVBCLT   DS    XL2                                                              
REVLDCLT DS    XL1                 REVALIDATE CLIENT                            
FLDH     DS    XL8                                                              
FLD      DS    CL40                                                             
       ++INCLUDE FLDHDRDS                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057SPTRA08   12/19/08'                                      
         END                                                                    
