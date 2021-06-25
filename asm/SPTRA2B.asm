*          DATA SET SPTRA2B    AT LEVEL 019 AS OF 12/19/11                      
*PHASE T2162BC                                                                  
*INCLUDE VEMAIL                                                                 
         TITLE 'T2162B NETWORK FAX RECORD DISPLAY, CHANGE, ADD, DELETE,X        
                LIST'                                                           
***********************************************************************         
*             PROGRAM - SPTRA2B /MAINT - SPTRA8B /LIST - SPTRA7B                
*                                                                               
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - KEEP DAYPART RECORD WHEN VALIDATING CLT SECURITY           
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
*        R7 - SECOND BASE                                                       
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
*                         CHANGE LOG                                  *         
*                                                                     *         
* LEV 3   SMUR  JUN12/96 ALLOW DAYPRT RECORDS BY CLIENT OR OFFICE     *         
* LEV 5   SMUR  SEP03/99 ADD NEW ELEMENT OV FAX (NEW SCREEN)          *         
* LEV 6   SMUR  SEP23/99 ADD NEW ELEMENT INTERNATIONAL FAX (ON HOLD)  *         
* LEV 7   SMUR  MAR13/00 MAKE FAX A REQUIRED FIELD                    *         
* LEV 8   SMUR  APR10/01 USE TRAFFIC  OFFICE                          *         
* LEV 9   SMUR  OCT10/01 OFFICE AND OFFICE LIST SECURITY              *         
* LEV 10  SMUR  JUN28/02 CLIENT STRING SECURITY                       *         
* LEV 11  SMUR  MAY29/03 2 CHARACTER DAYPART CODE                     *         
* LEV 12  SMUR  MAR17/03 BRAND LEVEL SECURITY                         *         
* LEV 13  SMUR  JUL26/04 SOX                                          *         
* LEV 14  SMUR  SEP12/05 2 CHAR OFFICE CODE                           *         
* LEV 15  SMUR  FEB16/06 MODIFY FOR MORE BRANDS PROJECT               *         
* LEV 16  SMUR  APR24/06 FIX FOR LIMITED ACCESS                       *         
* LEV 17  SMUR  OCT06/10 FIX ALPHA NUMERIC CLIENT DISPLAY             *         
* LEV 18  MNAS NOV15/11 CHANGES TO ACCOMODATE NEW VENDOR RECORD       *         
*         (REPLACING DAYPART) - BUG FIX FOR FEEDS ALSO INCLUDED       *         
* LEV 19  MNAS DEC15/11 REMOVE EMAIL NOTIFICATIONS                    *         
*         FORCE RECORD TO VENDOR OF AGENCY ENTERS DAYPART             *         
*                                                                     *         
***********************************************************************         
*                                                                               
T2162B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2162B*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR2BRR                                                      
*                                                                               
         CLI   TRLSTPGM,X'2B'                                                   
         BE    *+12                                                             
         MVI   TRLSTPGM,X'2B'                                                   
         NI    TRANETH+4,X'FF'-X'20'    SET UNVALIDATED                         
*                                                                               
         CLC   CONREC(3),=C'DAY'                                                
         BNE   CKMODES                                                          
         MVC   CONREC,SPACES                                                    
         MVC   CONREC(6),=C'VENDOR'                                             
         OI    CONRECH+6,X'80'     TRANSMIT                                     
*                                                                               
CKMODES  DS    0H                                                               
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
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
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
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
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
*                                                                               
*        TM    TRANETH+4,X'20'                                                  
*        BZ    VK04                                                             
*        TM    TRADPCH+4,X'20'                                                  
*        BZ    VK04                                                             
*        TM    TRAPRGH+4,X'20'                                                  
*        BZ    VK04                                                             
*        TM    TRACLTH+4,X'20'                                                  
*        BZ    VK04                                                             
*        TM    TRAOFCH+4,X'20'                                                  
*        BO    EXIT                                                             
*                                                                               
VK04     LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
*                                                                               
         XC    NETWORK,NETWORK     NETWORK                                      
         LA    R2,TRANETH                                                       
         CLI   5(R2),0             TEST NETWORK ENTERED                         
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK10                                                             
         B     MISSERR                                                          
         BAS   RE,VNET                                                          
*                                                                               
VK10     DS    0H                                                               
         LA    R2,TRAPRGH          PROGRAM                                      
         XC    PROGRAM,PROGRAM                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    *+8                 OPTIONAL                                     
         BAS   RE,VPROG                                                         
*                                                                               
         XC    BCLT,BCLT                                                        
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         XC    CLIENT,CLIENT                                                    
         CLI   5(R2),0             ANY CLIENT                                   
         BNE   VK12                                                             
*                                                                               
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK17                 NO                                          
*                                                                               
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
*                                                                               
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
*                                                                               
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
*                                                                               
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
*                                                                               
         MVI   ERROR,0                                                          
         B     VK17                                                             
*                                                                               
VK12     DS    0H                                                               
         GOTO1 VALICLT                                                          
         MVC   CLIENT,BCLT                                                      
*                                                                               
VK17     MVI   QDPT,0                                                           
         XC    QDPT2,QDPT2                                                      
*                                                                               
         LA    R2,TRADPCH          DAYPART CODE                                 
         CLI   5(R2),0             WAS DAYPART ENTERED                          
         BE    VK18                 NO, OPTIONAL                                
*                                                                               
         MVC   SVKEY(20),KEY       SAVE KEY                                     
         GOTO1 VALIDPT             VALIDATE DAYPART CODE                        
         MVC   KEY(20),SVKEY       RESTORE KEY                                  
*                                                                               
* GET OFFICE FOR LIMITED ACCESS CLT IF NEEDED                                   
*                                                                               
VK18     DS    0H                                                               
         MVI   LAOFFICE,0          INIT OFFICE FOR THIS LIMETED ACCESS          
*                                                                               
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BNZ   VK19                    YES                                      
*                                                                               
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VK19                                                             
*                                                                               
         CLI   T216FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    VK19                                                             
*                                                                               
         CLI   T216FFD+6,C'$'      TEST OFFICE LIST                             
         BE    VK19                                                             
*                                                                               
         BAS   RE,GOFF             GET OFFICE FOR THIS CLIENT                   
*                                                                               
VK19     DS    0H                                                               
         LA    R2,TRAOFCH          OFFICE                                       
         MVI   OFFICE,0                                                         
         CLI   5(R2),0             ANY OFFICE                                   
         BE    VK30                OPTIONAL                                     
         CLI   5(R2),2                                                          
         BH    INVOFERR            INVALID OFFICE                               
                                                                                
         BAS   RE,COFF             CONVERT TO 1 BYTE OFFICE CODE                
                                                                                
         BAS   RE,VOFFICE                                                       
         BE    VK20                                                             
         MVC   GERROR,=Y(NOCLTS)                                                
         GOTO1 VTRAERR                                                          
                                                                                
VK20     CLI   OFFICE,0            CLT OR OFFICE BUT NOT BOTH                   
         BE    VK30                                                             
         OC    CLIENT,CLIENT                                                    
         BZ    VK30                                                             
         B     CLTOFCER          ERROR BOTH CLT AND OFFICE WERE ENTERED         
*                                                                               
* BUILD THE KEY                                                                 
*                                                                               
VK30     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING FAXNRECD,R4         NETWORK FAX RECORD                           
         MVC   FAXKID,=X'26'       RECORD ID                                    
         MVC   FAXKAM,BAGYMD       AGY/MED                                      
         MVC   FAXKNET,NETWORK                                                  
         MVC   FAXKDPC,QDPT        1 BYTE DAYPART                               
         OC    PROGRAM,PROGRAM                                                  
         BZ    *+10                                                             
         MVC   FAXKPRG,PROGRAM                                                  
*                                                                               
         MVC   FAXKCLT,CLIENT                                                   
         MVC   FAXKOFC,OFFICE                                                   
*                                                                               
         MVC   MYSVKEY,KEY         SAVE THE KEY                                 
*                                                                               
         BRAS  RE,INITNET                                                       
*                                                                               
         OI    TRANETH+4,X'20'                                                  
         OI    TRADPCH+4,X'20'                                                  
         OI    TRAPRGH+4,X'20'                                                  
         OI    TRACLTH+4,X'20'                                                  
         OI    TRAOFCH+4,X'20'                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
*                                                                               
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VR01                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VR01     DS    0H                                                               
         L     R4,AIO                                                           
         USING FAXKEY,R4                                                        
         MVC   BAGYMD,FAXKAM                                                    
         MVC   NETWORK,FAXKNET                                                  
         MVC   QDPT,FAXKDPC        1 BYTE DAYPART CODE                          
         MVC   PROGRAM,FAXKPRG                                                  
         MVC   CLIENT,FAXKCLT                                                   
         MVC   OFFICE,FAXKOFC                                                   
         DROP  R4                                                               
*                                                                               
         OC    CLIENT,CLIENT       ANY CLIENT                                   
         BZ    VR05                                                             
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VR05                                                             
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    VR05                                                             
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     TRAPERR             CAN'T CHANGE THIS RECORD                     
*                                                                               
VR05     LR    R6,R4                                                            
         ZAP   FAXCTR,=P'0'        FAX COUNTER                                  
         MVI   DONEFLAG,0          INIT                                         
*                                                                               
*        MAKE SURE THERE ARE NO DUPLICATE FAX NUMBERS                           
*                                                                               
         CLI   TRAFAX1H+5,0         IF NO INPUT SKIP IT                         
         BE    VR05C                                                            
         LA    R2,TRAFAX1H                                                      
         CLI   TRAFAX2H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAFAX1,TRAFAX2                                                  
         BE    FAXERR                                                           
         CLI   TRAFAX3H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAFAX1,TRAFAX3                                                  
         BE    FAXERR                                                           
         CLI   TRAFAX4H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAFAX1,TRAFAX4                                                  
         BE    FAXERR                                                           
                                                                                
VR05C    CLI   TRAFAX2H+5,0         IF NO INPUT SKIP IT                         
         BE    VR05E                                                            
         LA    R2,TRAFAX2H                                                      
         CLI   TRAFAX3H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAFAX2,TRAFAX3                                                  
         BE    FAXERR                                                           
         CLI   TRAFAX4H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAFAX2,TRAFAX4                                                  
         BE    FAXERR                                                           
                                                                                
VR05E    CLI   TRAFAX3H+5,0         IF NO INPUT SKIP IT                         
         BE    VR05G                                                            
         LA    R2,TRAFAX3H                                                      
         CLI   TRAFAX4H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAFAX3,TRAFAX4                                                  
         BE    FAXERR                                                           
*                                                                               
*        MAKE SURE THERE ARE NO DUPLICATE EMAIL ADDRESSES                       
*                                                                               
VR05G    CLI   TRAEML1H+5,0         IF NO INPUT SKIP IT                         
         BE    VR05H                                                            
         LA    R2,TRAEML1H                                                      
         CLI   TRAEML2H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAEML1,TRAEML2                                                  
         BE    EMLER1                                                           
         CLI   TRAEML3H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAEML1,TRAEML3                                                  
         BE    EMLER1                                                           
         CLI   TRAEML4H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAEML1,TRAEML4                                                  
         BE    EMLER1                                                           
                                                                                
VR05H    CLI   TRAEML2H+5,0         IF NO INPUT SKIP IT                         
         BE    VR05J                                                            
         LA    R2,TRAEML2H                                                      
         CLI   TRAEML3H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAEML2,TRAEML3                                                  
         BE    EMLER1                                                           
         CLI   TRAEML4H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAEML2,TRAEML4                                                  
         BE    EMLER1                                                           
                                                                                
VR05J    CLI   TRAEML2H+5,0         IF NO INPUT SKIP IT                         
         BE    VR05K                                                            
         LA    R2,TRAEML3H                                                      
         CLI   TRAEML4H+5,0         IF NO INPUT SKIP IT                         
         BE    *+14                                                             
         CLC   TRAEML3,TRAEML4                                                  
         BE    EMLER1                                                           
*                                                                               
VR05K    DS    0H                                                               
*MNBETA  MVI   ELCODE,X'50'        REMOVE DEFUNCT INTERNATIONAL FAX             
*MNBETA  GOTO1 REMELEM                                                          
*MNBETA  MVI   ELCODE,X'40'        REMOVE DEFUNCT OVERNIGHT FAX ELEM            
*MNBETA  GOTO1 REMELEM                                                          
*MN                                                                             
         CLI   TRAFAX1H+5,0         IF NO INPUT SKIP IT                         
         BH    VR08                                                             
         CLI   TRAEML1H+5,0         IF NO INPUT SKIP IT                         
         BH    VR08                                                             
         LA    R2,TRAFAX1H                                                      
         B     MISSERR                                                          
*                                                                               
VR08     MVI   ELCODE,X'30'        REMOVE OLD FAX ELEMENTS                      
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'80'        REMOVE ANY NEW FAX ELEMENTS TO               
         GOTO1 REMELEM             PREPARE FOR DATA REFRESH                     
*                                                                               
         LA    R2,TRAFAX1H         POINT TO FIRST FAX/EMAIL LINE                
         USING DFAXD,R2                                                         
         LA    R5,1                KEEP COUNTER FOR ELEM SEQ NUMBER             
         LA    R3,4                COUNT DOWN LINES PROCESSED                   
*                                                                               
VR10     DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING FAXVEMEL,R6                                                      
         MVI   FAXVEMEL,X'80'       FAX/EMAIL ELEM X'80'                        
         MVI   FAXVEMLQ,FAXVEMLX-FAXVEMEL ELEMENT LENGTH                        
*                                                                               
         CLI   DFAXH+5,0           IF NO INPUT SKIP IT                          
         BE    VR15                                                             
         CLI   DFAXH+5,10          10 DIGIT INPUT                               
         BL    FAXNERR                                                          
         TM    DFAXH+4,X'08'       IS IT NUMERIC                                
         BZ    FAXNTNUM                                                         
         MVC   FAXVFAX,DFAX                                                     
*                                                                               
VR15     DS    0H                                                               
         CLI   DEMLH+5,0           IF NO EMAIL INPUT SKIP IT                    
         BE    VR16                                                             
         GOTO1 =V(VEMAIL),DMCB,DEMLH,0,RR=SPTR2BRR                              
         CLI   0(R1),0                                                          
         BE    VR15C                                                            
         LA    R2,DEMLH                                                         
         B     EMLERR                                                           
VR15C    MVC   FAXVEMAD,DEML                                                    
*                                                                               
VR16     CLI   DEMLH+5,0           IF AN EMAIL ADDRESS IS PRESENT               
         BE    *+8                 DEFAULT TYPE TO P(PDF)                       
         MVI   FAXVTYP,C'P'                                                     
         CLI   DTYPH+5,0           IF NO TYPE INPUT SKIP IT                     
         BE    VR20                                                             
         CLI   DEMLH+5,0           IF NO EMAIL INPUT SKIP IT                    
         BH    VR16C                                                            
         LA    R2,DTYPH                                                         
         B     TYPERR                                                           
                                                                                
VR16C    CLI   DTYP,C'B'                                                        
         BE    VR17                                                             
         CLI   DTYP,C'X'                                                        
         BE    VR17                                                             
         CLI   DTYP,C'P'                                                        
         BE    VR17                                                             
         LA    R2,DTYPH                                                         
         MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     TRAPERR             CAN'T CHANGE THIS RECORD                     
VR17     MVC   FAXVTYP,DTYP                                                     
*                                                                               
VR20     CLI   DRSPH+5,0           IF NO RESPONSE INPUT SKIP IT                 
         BE    VR25                                                             
         CLI   DEMLH+5,0           IF NO EMAIL INPUT SKIP IT                    
         BH    VR20C                                                            
         LA    R2,DRSPH                                                         
         B     RSPERR                                                           
                                                                                
VR20C    CLI   DRSP,C'N'                                                        
         BE    VR23                                                             
         LA    R2,DRSPH                                                         
         MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     TRAPERR             CAN'T CHANGE THIS RECORD                     
VR23     MVC   FAXVRSP,DRSP                                                     
*                                                                               
VR25     DS    0H                                                               
         CLC   FAXVFAX,SPACES                                                   
         BH    VR40                IF LOW NO DATA ENTERED ON THIS LINE          
         CLC   FAXVEMAD,SPACES                                                  
         BH    VR40                IF LOW NO DATA ENTERED ON THIS LINE          
         CLI   FAXVTYP,C' '                                                     
         BH    VR40                IF LOW NO DATA ENTERED ON THIS LINE          
         CLI   FAXVTYP,C' '                                                     
         BNH   VR45                                                             
*                                                                               
VR40     STCM  R5,1,FAXVSEQ                                                     
         GOTO1 ADDELEM                                                          
         LA    R5,1(R5)            BUMP ELEMENT SEQUENCE NUMBER                 
*                                                                               
VR45     DS    0H                                                               
         LA    R2,DFAXLEN(R2)      POINT TO NEXT FAX/EMAIL LINE                 
         BCT   R3,VR10             PROCESS ALL LINES                            
         DROP  R6                                                               
*                                                                               
*        MVC   MSGTEXT+33(2),AGENCY                                             
*        MVC   MSGTEXT+36(4),TRANET                                             
*        MVC   MSGTEXT+41(2),TRADPC                                             
*        MVC   MSGTEXT+44(6),TRAPRG                                             
*        MVC   MSGTEXT+51(3),TRACLT                                             
*        GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'MSGTEXT,MSGTEXT)                       
*                                                                               
*        CODE TO MAINTAIN OLD X'30' ELEMENTS                                    
VR50     LA    R2,TRAFAX1H         POINT TO FIRST FAX/EMAIL LINE                
         USING DFAXD,R2                                                         
         LA    R3,4                COUNT DOWN LINES PROCESSED                   
*                                                                               
VR52     DS    0H                                                               
         CLI   DFAXH+5,0           IF NO INPUT SKIP IT                          
         BE    VR55                                                             
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING FAXNUMEL,R6                                                      
         MVI   FAXNUMEL,X'30'                                                   
         MVI   FAXNUMLN,FAXNUMEQ-FAXNUMEL                                       
         MVC   FAXNUMA,DFAX                                                     
         MVC   FAXNUME,DFAX+3                                                   
         MVI   FAXNUME+3,C'-'                                                   
         MVC   FAXNUMN,DFAX+6                                                   
         GOTO1 ADDELEM                                                          
VR55     DS    0H                                                               
         LA    R2,DFAXLEN(R2)      POINT TO NEXT FAX/EMAIL LINE                 
         BCT   R3,VR52             PROCESS ALL LINES                            
*                                                                               
         LA    R2,TRANADRH                                                      
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING FAXADREL,R6                                                      
         MVI   FAXADREL,X'10'      ELEMENT IDENTIFIER                           
         MVI   FAXADRLN,32         ELEMENT LENGTH                               
         GOTO1 ANY                 MOVES DATA LEFT JUST                         
         MVC   FAXADRAD,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,TRANAMEH         NAME FIELD                                   
         MVI   ELCODE,X'20'        NAME ELEMENT CODE                            
         GOTO1 REMELEM                                                          
*                                                                               
VR80     LA    R5,1                SET NAME COUNT                               
         LA    R6,ELEM                                                          
         USING FAXLSTEL,R6                                                      
         MVI   SEQNUM,0                                                         
*                                                                               
VR85     CLI   5(R2),0             TEST FOR A NAME IN THIS FIELD                
         BE    VR100                                                            
*                                                                               
VR90     XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT ELEMENT SEQUENCE NUMBER            
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   FAXLSTEL,X'20'      ELEMENT IDENTIFIER                           
         MVI   FAXLSTLN,33         ELEMENT LENGTH                               
         MVC   FAXLSTSQ,SEQNUM     ELEMENT SEQUENCE NUMBER                      
*                                                                               
         CLM   R5,1,SEQNUM         NEED ANY BLANK PADDED ELEMS                  
         BE    VR95                                                             
         MVI   FAXLSTLN,4          ELEMENT LENGTH                               
         MVC   FAXLSTSQ,SEQNUM                                                  
         GOTO1 ADDELEM                                                          
         B     VR90                                                             
*                                                                               
VR95     GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         MVC   FAXLSTNM,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
VR100    ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    R5,1(,R5)           ADD TO NAME COUNT                            
         LA    RF,TRATAGH                                                       
         CR    R2,RF               TEST END OF SCREEN                           
         BL    VR85                                                             
*                                                                               
         LA    R2,TRADPCH          DAYPART CODE                                 
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'70'      DATA ELEMENT                                   
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING FAXDTEL,R6                                                       
         MVI   FAXDTEL,X'70'                                                    
         MVI   FAXDTLN,FAXDTX-FAXDTEL  ELEMENT LENGTH                           
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VRX                                                              
*                                                                               
         MVC   SVKEY(20),KEY       SAVE KEY                                     
         GOTO1 VALIDPT             GET DAYPART CODE EQUIVALENT                  
         MVC   KEY(20),SVKEY       RESTORE KEY                                  
*                                                                               
         MVC   FAXDTDPT,QDPT2      MOVE IN 2 CHAR DAYPART CODE                  
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VRX      B     DR                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         LA    R6,4                                                             
         USING DFAXD,R2                                                         
         LA    R2,TRAFAX1H                                                      
DR10     XC    DFAX,DFAX           CLEAR                                        
         OI    DFAXH+6,X'80'       TRANSMIT                                     
         XC    DEML,DEML                                                        
         OI    DEMLH+6,X'80'       TRANSMIT                                     
         MVI   DTYP,C' '                                                        
         OI    DTYPH+6,X'80'       TRANSMIT                                     
         MVI   DRSP,C' '                                                        
         OI    DRSPH+6,X'80'       TRANSMIT                                     
         LA    R2,DFAXLEN(R2)                                                   
         BCT   R6,DR10                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'80'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
*                                                                               
         USING FAXVEMEL,R6                                                      
         USING DFAXD,R2                                                         
         LA    R2,TRAFAX1H                                                      
*                                                                               
DR15     DS    0H                                                               
         MVC   DFAX,FAXVFAX                                                     
         OI    DFAXH+6,X'80'                                                    
         MVC   DEML,FAXVEMAD                                                    
         OI    DEMLH+6,X'80'                                                    
         MVC   DTYP,FAXVTYP                                                     
         OI    DTYPH+6,X'80'                                                    
         MVC   DRSP,FAXVRSP                                                     
         OI    DRSPH+6,X'80'                                                    
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DR70                                                             
         LA    R2,DFAXLEN(R2)                                                   
         B     DR15                                                             
*                                                                               
DR20     DS    0H                                                               
         L     R6,AIO                                                           
         LA    R2,TRAFAX1H         PREFIX                                       
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR70                                                             
*                                                                               
         USING FAXNUMEL,R6                                                      
         MVC   TRAFAX1(L'FAXNUMA),FAXNUMA                                       
         MVC   TRAFAX1+L'FAXNUMA(L'FAXNUME),FAXNUME                             
         MVC   TRAFAX1+L'FAXNUMA+L'FAXNUME(L'FAXNUMN),FAXNUMN                   
         OI    TRAFAX1H+6,X'80'       TRANSMIT                                  
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DR70                                                             
         LA    R2,TRAFAX2H         PREFIX                                       
         MVC   TRAFAX2(L'FAXNUMA),FAXNUMA                                       
         MVC   TRAFAX2+L'FAXNUMA(L'FAXNUME),FAXNUME                             
         MVC   TRAFAX2+L'FAXNUMA+L'FAXNUME(L'FAXNUMN),FAXNUMN                   
         OI    TRAFAX2H+6,X'80'       TRANSMIT                                  
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DR70                                                             
         LA    R2,TRAFAX3H         PREFIX                                       
         MVC   TRAFAX3(L'FAXNUMA),FAXNUMA                                       
         MVC   TRAFAX3+L'FAXNUMA(L'FAXNUME),FAXNUME                             
         MVC   TRAFAX3+L'FAXNUMA+L'FAXNUME(L'FAXNUMN),FAXNUMN                   
         OI    TRAFAX3H+6,X'80'       TRANSMIT                                  
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DR70                                                             
         LA    R2,TRAFAX4H         PREFIX                                       
         MVC   TRAFAX4(L'FAXNUMA),FAXNUMA                                       
         MVC   TRAFAX4+L'FAXNUMA(L'FAXNUME),FAXNUME                             
         MVC   TRAFAX4+L'FAXNUMA+L'FAXNUME(L'FAXNUMN),FAXNUMN                   
         OI    TRAFAX4H+6,X'80'       TRANSMIT                                  
*                                                                               
* CLEAR THE REST OF THE FIELDS                                                  
*                                                                               
DR70     L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            GET NETWORK ADDRESS                          
         BE    *+6                                                              
         DC    H'0'                ADDRESS MUST BE THERE                        
*                                                                               
         USING FAXADREL,R6                                                      
*                                                                               
         MVC   TRANADR,SPACES      NETWORK ADDRESS                              
         MVC   TRANADR(L'FAXADRAD),FAXADRAD                                     
         OI    TRANADRH+6,X'80'                                                 
*                                                                               
DR80     LA    R2,TRANAMEH         FIRST NAME FIELD                             
         LA    R3,8                MAX OF 8 FIELDS                              
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            TEST ANY NAMES                               
         BNE   DR100                                                            
*                                                                               
         USING FAXLSTEL,R6                                                      
*                                                                               
DR90     MVC   WORK(L'TRANAME),SPACES                                           
         CLI   FAXLSTLN,4          IF NAME FIELD IS BLANK SKIP                  
         BE    *+10                                                             
         MVC   WORK(L'FAXLSTNM),FAXLSTNM                                        
         MVC   8(L'TRANAME,R2),WORK                                             
         OI    6(R2),X'80'         TRANSMIT NEW FIELD                           
*                                                                               
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BZ    DRX                                                              
         BAS   RE,NEXTEL           NEXT NAME                                    
         BE    DR90                                                             
*                                                                               
DR100    MVC   8(L'TRANAME,R2),SPACES    BLANK OUT REMAINING FIELDS             
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BNZ   DR100                                                            
*                                                                               
DRX      B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO                                                           
         USING FAXNRECD,R4                                                      
*                                                                               
         XC    TRANET,TRANET                                                    
         XC    TRADPC,TRADPC                                                    
         XC    TRAPRG,TRAPRG                                                    
         XC    TRACLT,TRACLT                                                    
         XC    TRAOFC,TRAOFC                                                    
*                                                                               
         MVC   TRANET,FAXKNET      DISPLAY NETWORK                              
         MVC   TRAPRG,FAXKPRG              PROGRAM                              
*                                                                               
         CLI   FAXKOFC,0           ANY OFFICE                                   
         BE    DK10                                                             
*                                                                               
         MVC   SVOFFICE,FAXKOFC                                                 
         BAS   RE,POFF             CONVERT TO PRINTABLE 2 CHAR OFFICE           
         BNE   *+14                                                             
         MVC   TRAOFC(2),WORK                                                   
         B     *+10                                                             
         MVC   TRAOFC,FAXKOFC      OR 1 CHAR OFFICE NUMBER                      
*                                                                               
DK10     L     R0,AIO3             TO                                           
         L     RE,AIO1             FROM                                         
         L     RF,SIZEIO                                                        
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,FAXKCLT,QCLT  VALICLT WILL FIX AAN                   
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT             GET CORRECT QCLT (AAN)                       
*                                                                               
         MVC   TRACLT,QCLT         CLIENT                                       
*                                                                               
         L     R0,AIO1             TO                                           
         L     RE,AIO3             FROM                                         
         L     RF,SIZEIO                                                        
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         BRAS  RE,INITNET                                                       
*                                                                               
         CLI   FAXKDPC,0           ANY DAYPART ?                                
         BE    DK30                 NO                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'70'        DATA ELEMENT                                 
         BAS   RE,GETEL                                                         
         BNE   DK20                                                             
*                                                                               
         USING FAXDTEL,R6                                                       
*                                                                               
         MVC   TRADPC,FAXDTDPT     DAYPART CODE                                 
         B     *+10                                                             
DK20     MVC   TRADPC(1),FAXKDPC   DAYPART CODE                                 
*                                                                               
DK30     OI    TRANETH+6,X'80'     SET ON TRANSMIT BIT                          
         OI    TRADPCH+6,X'80'                                                  
         OI    TRAPRGH+6,X'80'                                                  
         OI    TRACLTH+6,X'80'                                                  
         OI    TRAOFCH+6,X'80'                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*                                                                               
LR       LA    R4,KEY                                                           
         USING FAXKEY,R4                                                        
         OC    KEY(20),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                NO, GOTO HIGH                                
*                                                                               
         MVC   KEY,MYSVKEY         MOVE IN SAVED KEY                            
*                                                                               
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         ZAP   FAXCTR,=P'0'        FAX COUNTER                                  
*                                                                               
* DO READHI                                                                     
*                                                                               
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         MVC   NET,KEY+2                                                        
         CLC   MYSVKEY(2),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR22                                                             
*                                                                               
LR16     CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   EXIT                                                             
         MVC   P(28),=C'NO NETWORK FAX RECORDS FOUND'                           
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
*                                                                               
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
*                                                                               
LR22     CLC   KEY(2),MYSVKEY      AT END OF THIS AGENCY/MEDIA                  
         BNE   LREND               YES                                          
*                                                                               
         OC    KEY+13(2),KEY+13    ANY CLIENT                                   
         BZ    LR25                 NO                                          
*                                                                               
         MVC   BCLT,KEY+13                                                      
         BAS   RE,FCLT                                                          
         BNE   LREND                                                            
*                                                                               
LR25     CLI   KEY+15,0            ANY OFFICE                                   
         BE    LR30                 NO                                          
*                                                                               
         CLI   LAOFFICE,0          CLT LIMITED ACCESS                           
         BE    LR26                NO                                           
*                                                                               
         CLC   LAOFFICE,KEY+15     SAME OFFICE?                                 
         BNE   LR27                                                             
*                                                                               
LR26     MVC   SVKEY(20),KEY       SAVE KEY                                     
         MVC   SVOFFICE,OFFICE     SAVE OFFICE                                  
         MVC   OFFICE,KEY+15                                                    
*                                                                               
         BAS   RE,VOFFICE                                                       
*                                                                               
         MVC   OFFICE,SVOFFICE     RESTORE OFFICE                               
         MVC   KEY(20),SVKEY       RESTORE KEY                                  
         BE    LR28                                                             
*                                                                               
LR27     GOTO1 HIGH                DUMMY READ FOR SEQ                           
         B     LR20                                                             
*                                                                               
LR28     GOTO1 HIGH                DUMMY READ FOR SEQ                           
*                                                                               
LR30     OC    NETWORK,NETWORK     SEE IF NETWORK WAS ENTERED                   
         BZ    *+14                                                             
         CLC   NETWORK,KEY+2       IF SO TEST KEY MATCH                         
         BNE   LR20                                                             
*                                                                               
         OC    QDPT,QDPT           SEE IF DAYPART WAS ENTERED                   
         BZ    *+14                                                             
         CLC   QDPT,KEY+6          IF SO TEST KEY MATCH                         
         BNE   LR20                                                             
*                                                                               
         OC    PROGRAM,PROGRAM     SEE IF PROGRAM WAS ENTERED                   
         BZ    *+14                                                             
         CLC   PROGRAM,KEY+7       IF SO TEST KEY MATCH                         
         BNE   LR20                                                             
*                                                                               
         OC    CLIENT,CLIENT       SEE IF CLIENT WAS ENTERED                    
         BZ    *+14                                                             
         CLC   CLIENT,KEY+13       IF SO TEST KEY MATCH                         
         BNE   LR20                                                             
*                                                                               
         CLI   OFFICE,0            SEE IF OFFICE WAS ENTERED                    
         BE    *+14                                                             
         CLC   OFFICE,KEY+15       IF SO TEST KEY MATCH                         
         BNE   LR20                                                             
*                                                                               
         CLC   NET,KEY+2           NETWORK CHANGE ?                             
         BE    LR50                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   NET,KEY+2           NETWORK CHANGE                               
*                                                                               
LR50     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
LREND    CLI   MODE,PRINTREP       IF OFFLINE                                   
         BNE   EXIT                                                             
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         MVC   P+9(27),=C'NETWORK FAX NUMBERS PRINTED'                          
         EDIT (P3,FAXCTR),(5,P+3),ZERO=NOBLANK                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
*                                                                               
LRL      LA    R5,LISTAR           ADDRESS OF WORK AREA                         
         MVC   LISTAR,SPACES                                                    
         USING LSTLINE,R5                                                       
         MVC   LNET,FAXKNET        NETWORK                                      
         MVC   LPRG,FAXKPRG        PROGRAM                                      
*                                                                               
         CLI   FAXKOFC,0           ANY OFFICE                                   
         BE    LRL02                                                            
*                                                                               
         MVC   SVOFFICE,FAXKOFC                                                 
         BAS   RE,POFF             CONVERT TO PRINTABLE 2 CHAR OFFICE           
         BNE   *+14                                                             
         MVC   LOFC(2),WORK                                                     
         B     *+10                                                             
         MVC   LOFC(1),FAXKOFC     OR 1 CHAR OFFICE                             
*                                                                               
LRL02    GOTO1 CLUNPK,DMCB,(SVCPROF6,FAXKCLT),QCLT                              
         MVC   LCLT,QCLT           CLIENT                                       
*                                                                               
         CLI   FAXKDPC,0           ANY DAYPART ?                                
         BE    LRL05                NO                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'70'        DATA ELEMENT                                 
         BAS   RE,GETEL                                                         
         BNE   LRL05                                                            
*                                                                               
         USING FAXDTEL,R6                                                       
*                                                                               
         MVC   LDPC,FAXDTDPT       DAYPART CODE                                 
         B     *+10                                                             
LRL05    MVC   LDPC(1),FAXKDPC     DAYPART CODE                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        NETWORK ADDRESS                              
         BAS   RE,GETEL                                                         
         BNE   LRL10                                                            
*                                                                               
         USING FAXADREL,R6                                                      
*                                                                               
         MVC   LNETADDR,FAXADRAD                                                
*                                                                               
LRL10    DS    0H                                                               
         L     R6,AIO              AIO                                          
         MVI   ELCODE,X'80'        FAX ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   LRL15                                                            
*                                                                               
         USING FAXVEMEL,R6                                                      
         OC    FAXVFAX,FAXVFAX                                                  
         BNZ   LRL13                                                            
         MVC   LFAX(L'LFAX),FAXVEMAD                                            
         B     LRL20                                                            
                                                                                
LRL13    MVI   LFAX,C'('                                                        
         MVC   LFAX+1(3),FAXVFAX   AREA CODE                                    
         MVI   LFAX+4,C')'                                                      
         MVC   LFAX+6(7),FAXVFAX+3 EXTENTION AND NUMBER                         
         B     LRL20                                                            
*                                                                               
LRL15    DS    0H                                                               
         L     R6,AIO              AIO                                          
         MVI   ELCODE,X'30'        FAX ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   LRL20                                                            
*                                                                               
         USING FAXNUMEL,R6                                                      
*        MVC   LFAX(1),FAXNUM1     PREFIX IF ANY                                
         MVI   LFAX,C'('                                                        
         MVC   LFAX+1(3),FAXNUMA   AREA CODE                                    
         MVI   LFAX+4,C')'                                                      
         MVC   LFAX+6(8),FAXNUME   EXTENTION AND NUMBER                         
         B     LRL20                                                            
*                                                                               
LRL20    GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
*                                                                               
* FORMAT OFFLINE REPORT HERE                                                    
*                                                                               
LRR      MVI   DONEFLAG,0          INIT                                         
*                                                                               
         LA    R5,P                PRINT LINE ADDRESS                           
         MVC   P,SPACES                                                         
         USING PRTLINE,R5                                                       
*                                                                               
         MVC   PPRG,FAXKPRG        PROGRAM                                      
         LA    R3,PPRG                                                          
         CLC   PPRG,SPACES                                                      
         BNH   *+8                                                              
         LA    R3,132(,R3)                                                      
*                                                                               
         OC    FAXKCLT,FAXKCLT                                                  
         BZ    LRR03                                                            
*                                                                               
         MVC   0(4,R3),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,(SVCPROF6,FAXKCLT),QCLT                              
         MVC   4(3,R3),QCLT                                                     
         LA    R3,132(R3)                                                       
*                                                                               
LRR03    CLI   FAXKOFC,0                                                        
         BE    *+16                                                             
         MVC   0(4,R3),=C'OFC='                                                 
         MVC   4(1,R3),FAXKOFC                                                  
*                                                                               
         XC    FAXTBL(FAXTBLEN),FAXTBL  INIT FAX TABLE                          
         XC    EMLTBL(EMLTBLEN),EMLTBL  EMAIL TABLE                             
*                                                                               
         CLI   FAXKDPC,0           ANY DAYPART ?                                
         BE    LRR04C               NO                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'70'        DATA ELEMENT                                 
         BAS   RE,GETEL                                                         
         BNE   LRR04                                                            
*                                                                               
         USING FAXDTEL,R6                                                       
*                                                                               
         MVC   PDPC,FAXDTDPT       DAYPART CODE                                 
         B     *+10                                                             
LRR04    MVC   PDPC(1),FAXKDPC     DAYPART CODE                                 
*                                                                               
LRR04C   DS    0H                                                               
         OI    DONEFLAG,FAXSW1   PRE-SET DONE WITH FAX                          
*                                                                               
LRR05    L     R6,AIO                                                           
         MVI   ELCODE,X'80'        FAX ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   LRR06                                                            
*                                                                               
         NI    DONEFLAG,X'FF'-FAXSW1 DONE WITH FAX                              
*                                                                               
         USING FAXVEMEL,R6                                                      
         LA    R1,FAXTBL                SAVE FAX # (UPTO 4)                     
         LA    R2,EMLTBL                                                        
         LA    R3,1                                                             
LRR05C   AP    FAXCTR,=P'1'                                                     
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(3),=C'FAX'                                                
         EDIT  (R3),(1,MYWORK+3)                                                
         MVI   MYWORK+4,C':'                                                    
         MVI   MYWORK+6,C'('                                                    
         MVC   MYWORK+7(3),FAXVFAX      AREA CODE                               
         MVI   MYWORK+10,C')'                                                   
         MVC   MYWORK+12(3),FAXVFAX+3   EXTENTION AND NUMBER                    
         MVI   MYWORK+15,C'-'                                                   
         MVC   MYWORK+16(4),FAXVFAX+6                                           
*                                                                               
         MVC   0(1,R2),MYWORK+3                                                 
         MVC   1(57,R2),FAXVEMAD                                                
         MVC   58(1,R2),FAXVTYP                                                 
         MVC   59(1,R2),FAXVRSP                                                 
         LA    R2,L'EMLTBL(R2)                                                  
*                                                                               
         MVC   0(L'FAXTBL,R1),MYWORK    SAVE FAX NUMBER                         
         LA    R1,L'FAXTBL(R1)          BUMP IN FAX TABLE                       
         LA    R3,1(R3)                                                         
         BAS   RE,NEXTEL                                                        
         BE    LRR05C                                                           
         B     LRR09                                                            
*                                                                               
LRR06    L     R6,AIO                                                           
         MVI   ELCODE,X'30'        FAX ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   LRR09                                                            
*                                                                               
         NI    DONEFLAG,X'FF'-FAXSW1 DONE WITH FAX                              
*                                                                               
         USING FAXNUMEL,R6                                                      
*                                                                               
         LA    R1,FAXTBL           SAVE FAX # (UPTO 4)                          
         LA    R3,1                                                             
LRR06C   AP    FAXCTR,=P'1'                                                     
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(3),=C'FAX'                                                
         EDIT  (R3),(1,MYWORK+3)                                                
         MVI   MYWORK+4,C':'                                                    
         MVC   MYWORK+5(1),FAXNUM1   PREFIX IF ANY                              
         MVI   MYWORK+6,C'('                                                    
         MVC   MYWORK+7(3),FAXNUMA AREA CODE                                    
         MVI   MYWORK+10,C')'                                                   
         MVC   MYWORK+12(8),FAXNUME EXTENTION AND NUMBER                        
*                                                                               
         MVC   0(L'FAXTBL,R1),MYWORK SAVE FAX NUMBER                            
         LA    R1,L'FAXTBL(R1)     BUMP IN FAX TABLE                            
         LA    R3,1(R3)                                                         
         BAS   RE,NEXTEL                                                        
         BE    LRR06C                                                           
*                                                                               
LRR09    LR    R6,R4               AIO                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         USING FAXADREL,R6                                                      
*                                                                               
         MVC   PNETADDR,FAXADRAD   PRINT NETWORK ADDRESS                        
*                                                                               
         LA    R3,FAXTBL                                                        
         LA    R2,EMLTBL                                                        
         LA    R1,4                                                             
         CLC   PNETADDR,SPACES     ANY ADDRESS                                  
         BNH   LRR09C               YES                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LRR09C   DS    0H                                                               
         MVC   PFAX,0(R3)                                                       
         OC    1(57,R2),1(R2)                                                   
         BZ    LRR09D                                                           
         MVC   PEMLTAG,=C'EML :'                                                
         MVC   PEMLTAG+3(1),0(R2)                                               
         MVC   PEMAIL,1(R2)                                                     
LRR09D   DS    0H                                                               
         CLI   58(R2),0                                                         
         BE    *+16                                                             
         MVC   PTYP(4),=C'TYP='                                                 
         MVC   PTYP+4(1),58(R2)                                                 
*                                                                               
         CLI   59(R2),0                                                         
         BE    *+16                                                             
         MVC   PRSP(4),=C'RSP='                                                 
         MVC   PRSP+4(1),59(R2)                                                 
*                                                                               
         LA    R5,132(R5)                                                       
         LA    R3,L'FAXTBL(R3)                                                  
         LA    R2,L'EMLTBL(R2)                                                  
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    LRR09F                                                           
         CLC   0(L'FAXTBL,R3),SPACES                                            
         BH    LRR09C                                                           
         CLC   0(L'EMLTBL,R2),SPACES                                            
         BH    LRR09C                                                           
*                                                                               
LRR09F   DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P                                                             
         LA    R3,4                                                             
         LR    R6,R4                                                            
         MVI   ELCODE,X'20'        NAME LIST ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   LRR16                                                            
         USING FAXLSTEL,R6                                                      
*                                                                               
         MVC   PNAME1-12(10),=C'COPIES TO:'                                     
*                                                                               
LRR10    DS    0H                                                               
         LA    R2,PNAME1                                                        
         CLI   FAXLSTLN,4                                                       
         BE    *+10                                                             
         MVC   0(30,R2),FAXLSTNM                                                
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   LRR16                                                            
*                                                                               
         LA    R2,PNAME2                                                        
         CLI   FAXLSTLN,4                                                       
         BE    *+10                                                             
         MVC   0(30,R2),FAXLSTNM                                                
*                                                                               
         LA    R5,132(R5)                                                       
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BZ    LRR16                                                            
         BAS   RE,NEXTEL                                                        
         BE    LRR10                                                            
*                                                                               
LRR16    DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT                                        
         B     LR20                                                             
         EJECT                                                                  
* CONVERT 2 CHAR OFFICE CODE TO 1 BYTE                                          
*                                                                               
COFF     NTR1                                                                   
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,8(R2)       2 CHAR OFFICE CODE                           
         CLI   5(R2),2                                                          
         BE    *+10                                                             
         OC    OFCOFC2,SPACES                                                   
*                                                                               
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCSECD,ASECBLK                                                  
*                                                                               
         CLI   LAOFFICE,0                                                       
         BNE   COFF20                                                           
*                                                                               
COFF10   MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
*                                                                               
COFF20   XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   INVOFERR            INVALID OFFICE                               
*                                                                               
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    COFF40              2 CHAR OFFICE IS NOT ON                      
*                                                                               
         MVC   OFFICE,OFCOFC       SAVE 1 BYTE OFFICE CODE                      
         B     COFFX                                                            
*                                                                               
COFF40   DS    0H                                                               
         MVC   OFFICE,8(R2)                                                     
*                                                                               
COFFX    XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
* GET OFFICE FOR SINGLE CLIENT LIMITED ACCESS                                   
*                                                                               
GOFF     NTR1                                                                   
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),T216FFD+6  LIMITED ACCESS CLT                           
*                                                                               
         BRAS  RE,INITSPT                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BE    *+6                                                              
         DC    H'0'                WHAT'S WRONG                                 
*                                                                               
         CLC   KEY+1(1),BAGYMD                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
*                                                                               
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   LAOFFICE,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   LAOFFICE,C'A'       IF THERE IS ONE                              
         BNL   *+10                                                             
         MVC   LAOFFICE,COFFICE    USE MEDIA OFFICE                             
*                                                                               
         BRAS  RE,INITNET                                                       
*                                                                               
         L     R6,AIO1             RESET TO AIO1                                
         ST    R6,AIO                                                           
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
* GET PRINTABLE OFFICE (CONVERT FROM 1 BYTE TO 2 CHAR                           
*                                                                               
POFF     NTR1                                                                   
*                                                                               
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVOFFICE     1 CHAR OFFICE CODE                           
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCSECD,ASECBLK                                                  
*                                                                               
         CLI   LAOFFICE,0          ANY LIMITED ACCESS                           
         BNE   POFF20              YES                                          
*                                                                               
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
*                                                                               
POFF20   L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BETTER BE A VALID OFFICE                     
*                                                                               
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    POFFNE              2 CHAR OFFICE IS NOT ON                      
*                                                                               
         MVC   WORK(2),OFCOFC2                                                  
         CR    RB,RB               SET CC EQ                                    
         B     POFFX                                                            
*                                                                               
         DROP  R3                                                               
*                                                                               
POFFNE   DS    0H                                                               
         CR    RB,RC               SET CC NE                                    
*                                                                               
POFFX    XIT1                                                                   
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
*                                                                               
FCLT     NTR1                                                                   
*                                                                               
FCLT10   MVC   MYSVKEY,KEY                                                      
*                                                                               
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
*                                                                               
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
*                                                                               
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FCLT13              NO, GET NEXT CLIENT                          
*                                                                               
         TM    SECFLAG,NOCLOFF     NO MATCH ON CLIENT OFFICE                    
         BZ    FCLT20              OFFICES MATCH, OK TO LIST                    
*                                                                               
FCLT13   MVC   KEY,MYSVKEY                                                      
         MVI   KEY+15,X'FF'        GET NEXT CLIENT                              
*                                                                               
         BRAS  RE,INITNET                                                       
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(2),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   FCLTNE                                                           
*                                                                               
         OC    KEY+13(2),KEY+13    ANY CLIENT                                   
         BNZ   FCLT15                                                           
         XC    BCLT,BCLT                                                        
         MVC   MYSVKEY,KEY                                                      
         B     FCLT20                                                           
*                                                                               
FCLT15   MVC   BCLT,KEY+13         SAVE CLIENT                                  
         B     FCLT10                                                           
*                                                                               
FCLT20   DS    0H                                                               
*                                                                               
         MVC   KEY,MYSVKEY         RESTORE KEY AND DISK ADDR                    
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         BRAS  RE,INITNET                                                       
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CR    RB,RB                                                            
         B     FCLTX                                                            
*                                                                               
FCLTNE   LTR   RB,RB                                                            
*                                                                               
FCLTX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE NETWORK                                                              
*                                                                               
VNET     NTR1                                                                   
         USING STARECD,R4          LOOK UP NETWORK IN STATION RECORD            
*                                                                               
         GOTO1 ANY                 PUTS INPUT INTO WORK LEFT-JUSTIFIE           
         XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         MVI   STAKTYPE,C'S'       STATION RECORD TYPE                          
         MVI   STAKMED,C'N'        MEDIA NETWORK                                
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         L     R4,AIO                                                           
         CLC   0(9,R4),KEY         TEST NETWORK IS ON FILE                      
         BNE   NETERR                                                           
         MVC   NETWORK,WORK                                                     
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,BNET             SAVE NETWORK MARKET NUMBER                   
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE OFFICE CODE                                                          
*                                                                               
VOFFICE  NTR1                                                                   
*                                                                               
         BRAS  RE,INITSPT                                                       
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BZ    *+16                                                             
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BE    VOFFX               BYPASS, NOT WORKING YET                      
         B     VOFF50                                                           
*                                                                               
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VOFF05                                                           
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BNE   VOFF02                                                           
*                                                                               
         CLC   T216FFD+7(1),OFFICE    MATCH OFFICE CODE                         
         B     VOFFX                                                            
*                                                                               
VOFF02   CLI   T216FFD+6,C'$'          TEST OFFICE LIST                         
         BE    VOFF50                                                           
*                                                                               
VOFF05   XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VOFF10   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BNE   VOFFX                                                            
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   VOFFX                                                            
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    VOFF30               YES                                         
*                                                                               
VOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     VOFF10                                                           
*                                                                               
VOFF30   L     R6,AIO1                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVCLTOFF,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   SVCLTOFF,C'A'       IF THERE IS ONE                              
         BNL   VOFF40                                                           
*                                                                               
         MVC   SVCLTOFF,COFFICE    USE MEDIA OFFICE                             
*                                                                               
VOFF40   DS    0H                                                               
         CLI   LAOFFICE,0          IS THIS CLT LIMITED ACCESS                   
         BE    VOFF50               NO                                          
         CLC   SVCLTOFF,LAOFFICE   SAME OFFICE                                  
         BNE   VOFF20                                                           
         B     VOFF70                                                           
*                                                                               
VOFF50   DS    0H                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R1,WORKSEC                                                       
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,OFFICE                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BE    VOFF70                                                           
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VOFF100                                                          
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    VOFF100                                                          
*                                                                               
         MVI   ERROR,SECLOCK                                                    
         GOTO1 ERREX                                                            
*                                                                               
VOFF70   DS    0H                                                               
         CLC   OFFICE,SVCLTOFF     IS THIS THE RIGHT OFFICE                     
         BNE   VOFF20                                                           
         B     VOFFX                                                            
*                                                                               
VOFF100  CR    RB,RC               SET CC NE                                    
*                                                                               
VOFFX    DS    0H                                                               
         BRAS  RE,INITNET                                                       
*                                                                               
         XIT1                                                                   
INITNET  DS    0H                                                               
         MVI   LKEY+1,20                                                        
         MVI   DATADISP+1,27                                                    
         MVI   LSTATUS+1,1                                                      
         MVI   SYSDIR,C'U'                                                      
         MVI   SYSDIR+1,C'N'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVI   SYSFIL,C'U'                                                      
         MVI   SYSFIL+1,C'N'                                                    
         MVI   SYSFIL+2,C'T'                                                    
         BR    RE                                                               
INITSPT  MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVI   LSTATUS+1,1                                                      
         MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVI   SYSFIL,C'S'                                                      
         MVI   SYSFIL+1,C'P'                                                    
         MVI   SYSFIL+2,C'T'                                                    
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE PROGRAM                                                              
*                                                                               
VPROG    NTR1                                                                   
         USING NPGRECD,R4                                                       
*                                                                               
         GOTO1 ANY                 PUTS INPUT INTO WORK LEFT-JUSTIFIED          
         XC    KEY,KEY             PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         MVC   NPGKTYP,=X'0D20'    NETWORK PROGRAM RECORD                       
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,BNET                                                     
         MVC   NPGKPROG,WORK                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA SYSTEM                
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     TEST KEYS WITHOUT END DATE                   
         BE    *+8                                                              
         B     PROGERR             PROGRAM DOES NOT EXIST                       
         MVC   PROGRAM,WORK        SAVE IT                                      
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
         PRINT GEN                                                              
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*                                                                               
HDHK     NTR1                                                                   
         MVC   H4+10(4),NET                                                     
         B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
*                                                                               
CLTOFCER XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'CLTOFCMS),CLTOFCMS                                     
         B     ERREXIT                                                          
CLTOFCMS DC    C'* ERROR * ENTER CLIENT OR OFFICE BUT NOT BOTH *'               
*                                                                               
INVOFERR XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'INVOFMSG),INVOFMSG                                     
         B     ERREXIT                                                          
INVOFMSG DC    C'* ERROR * INVALID OFFICE *'                                    
*                                                                               
NOFAXERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOFAXMS),NOFAXMS                                       
         B     ERREXIT                                                          
NOFAXMS  DC    C'* ERROR * FAX NUMBER REQUIRED *'                               
*                                                                               
FAXAERR  XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'FAXPARMS),FAXPARMS                                     
         B     ERREXIT                                                          
FAXPARMS DC    C'* ERROR * MUST ENTER ALL OF TEL, NOT PARTIAL *'                
*                                                                               
FAXNERR  XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'TELINVAL),TELINVAL TEL NMBR INVALID                    
         B     ERREXIT                                                          
TELINVAL DC    C'* ERROR * FAX NUMBER MUST BE TEN DIGITS *'                     
*                                                                               
FAXNTNUM XC    CONHEAD,CONHEAD     CLEAR ERROR LINE                             
         MVC   CONHEAD(L'TELNNUM),TELNNUM TEL NMBR MUST BE NUMERI               
         B     ERREXIT                                                          
TELNNUM  DC    C'* ERROR * FAX NUMBER MUST BE NUMERIC *'                        
*                                                                               
PROGERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRGERRMS),PRGERRMS                                     
         B     ERREXIT                                                          
PRGERRMS DC    C'* ERROR * PROGRAM NOT FOUND *'                                 
*                                                                               
NETERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NETERRMS),NETERRMS                                     
         B     ERREXIT                                                          
NETERRMS DC    C'* ERROR * NETWORK NOT FOUND *'                                 
*                                                                               
EMLERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'EMLERRMS),EMLERRMS                                     
         B     ERREXIT                                                          
EMLERRMS DC    C'* ERROR * NOT VALID EMAIL FORMAT *'                            
*                                                                               
TYPERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TYPERRMS),TYPERRMS                                     
         B     ERREXIT                                                          
TYPERRMS DC    C'* ERROR * TYPE NOT VALID WITHOUT EMAIL *'                      
*                                                                               
RSPERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RSPERRMS),RSPERRMS                                     
         B     ERREXIT                                                          
RSPERRMS DC    C'* ERROR * RESPONSE NOT VALID WITHOUT EMAIL *'                  
*                                                                               
FAXERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FAXERRMS),FAXERRMS                                     
         B     ERREXIT                                                          
FAXERRMS DC    C'* ERROR * DUPLICATE FAX NUMBERS NOT VALID *'                   
*                                                                               
EMLER1   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'EMLER1MS),EMLER1MS                                     
         B     ERREXIT                                                          
EMLER1MS DC    C'* ERROR * DUPLICATE EMAIL ADDR NOT VALID *'                    
*                                                                               
ERREXIT  GOTO1 ERREX2                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*SGTEXT  DC    C'AUTONOTE*MNAS,SMUR: **DPT UPDATE AA NNNN DP PPPPPP CCC         
*               **'                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'______________'                                           
         SSPEC H5,3,PAGE                                                        
         SSPEC H1,35,C'NETWORK FAX LIST'                                        
         SSPEC H2,35,C'________________'                                        
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,3,C'NETWORK'                                                  
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H8,3,C'PROGRAM'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H7,12,C'D'                                                       
         SSPEC H8,12,C'P'                                                       
         SSPEC H9,12,C'-'                                                       
         SSPEC H8,14,C'NETWORK ADDRESS'                                         
         SSPEC H9,14,C'---------------'                                         
         SSPEC H8,45,C'CONTACT INFORMATION'                                     
         SSPEC H9,45,C'-------------------'                                     
         SSPEC H8,104,C'TYPE'                                                   
         SSPEC H9,104,C'----'                                                   
         SSPEC H8,111,C'RSPN'                                                   
         SSPEC H9,111,C'----'                                                   
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
*                                                                               
* DSECT TO COVER ONE LINE OF DETAIL DISPLAY                                     
DFAXD    DSECT                                                                  
DFAXH    DS    CL8                                                              
DFAX     DS    CL10                                                             
DEMLH    DS    CL8                                                              
DEML     DS    CL57                                                             
DTYPH    DS    CL8                                                              
DTYP     DS    CL1                                                              
DRSPH    DS    CL8                                                              
DRSP     DS    CL1                                                              
DFAXLEN  EQU   *-DFAXD                                                          
*                                                                               
* OFFLINE REPORT                                                                
*                                                                               
**PRTLINE  DSECT                                                                
**         DS    CL2                                                            
**PPRG     DS    CL6                                                            
**         DS    CL3                                                            
**PDPC     DS    CL2                                                            
**         DS    CL1                                                            
**PNETADDR DS    CL30                                                           
**         DS    CL2                                                            
**PNAME1   DS    CL30                                                           
**         DS    CL3                                                            
**PNAME2   DS    CL30                                                           
**         ORG   PNETADDR+2                                                     
**PFAX     DS    CL20                                                           
*                                                                               
*                                                                               
* OFFLINE REPORT                                                                
*                                                                               
PRTLINE  DSECT                                                                  
         DS    CL2                                                              
PPRG     DS    CL6                                                              
         DS    CL3                                                              
PDPC     DS    CL2                                                              
         DS    CL1                                                              
PNETADDR DS    CL30                                                             
         DS    CL2                                                              
PNAME1   DS    CL30                                                             
         DS    CL3                                                              
PNAME2   DS    CL30                                                             
         ORG   PNETADDR+2                                                       
PFAX     DS    CL20                                                             
         DS    CL2                                                              
PEMLTAG  DS    CL5                                                              
         DS    CL1                                                              
PEMAIL   DS    CL57                                                             
         DS    CL2                                                              
PTYP     DS    CL5                                                              
         DS    CL2                                                              
PRSP     DS    CL5                                                              
*                                                                               
*ONLINE LIST LINE                                                               
*                                                                               
LSTLINE  DSECT                                                                  
LNET     DS    CL4                                                              
         DS    CL1                                                              
LPRG     DS    CL6                                                              
         DS    CL2                                                              
LDPC     DS    CL2                                                              
         DS    CL2                                                              
LCLT     DS    CL3                                                              
         DS    CL2                                                              
LOFC     DS    CL2                                                              
         DS    CL2                                                              
LNETADDR DS    CL30                                                             
         DS    CL1                                                              
LFAX     DS    CL14                                                             
         EJECT                                                                  
       ++INCLUDE SPTRNFAX                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRNEQPRG                                                     
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA8BD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
*                                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR2BRR DS    A                                                                
SEQNUM   DS    XL1                                                              
FLDH     DS    XL8                                                              
FLD      DS    CL64                                                             
MYSVKEY  DS    CL48                                                             
MYWORK   DS    CL20                                                             
SVOVFAX  DS    CL20                OVERNIGHT FAX                                
SVIFAX   DS    CL20                INTERNATIONAL FAX                            
NETWORK  DS    CL4                                                              
NET      DS    CL4                                                              
PROGRAM  DS    CL6                                                              
*AYPART  DS    CL2                                                              
CLIENT   DS    CL2                                                              
OFFICE   DS    X                                                                
SVOFFICE DS    X                                                                
LAOFFICE DS    X                   LIMITED ACCESS OFFICE                        
BNET     DS    H                                                                
FAXSW    DS    CL1                                                              
FAXCTR   DS    PL3                                                              
OVFAXCTR DS    PL3                                                              
IFAXCTR  DS    PL3                                                              
*                                                                               
FAXTBL   DS   4CL20                                                             
FAXTBLEN EQU   *-FAXTBL                                                         
*                                                                               
EMLTBL   DS    4CL60                                                            
EMLTBLEN EQU   *-EMLTBL                                                         
*                                                                               
SVPREFIX DS    CL1                                                              
DONEFLAG DS    CL1                                                              
NAMESW   EQU   X'80'                                                            
FAXSW1   EQU   X'40'                                                            
OVFAXSW  EQU   X'20'               OVERNIGHT FAX                                
INTFAXSW EQU   X'10'               INTERNATIONAL FAX                            
FAXFNDSW EQU   X'08'               FAX FOUND SWITCH                             
*                                                                               
NEXTFAX  EQU   (TRAFAX2H-TRAFAX1H)                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPTRA2B   12/19/11'                                      
         END                                                                    
