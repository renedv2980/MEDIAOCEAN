*          DATA SET RECFI00    AT LEVEL 019 AS OF 03/17/98                      
*PHASE T81200C                                                                  
         PRINT NOGEN                                                            
         TITLE 'CFI  (T81200) -- JDS EC TIMESTAMP'                              
*=================================================================*             
* THIS PROGRAM RECEIVES EC MESSAGES FROM JDS. ALL MESSAGES ARE    *             
* RECEIVED IN THE REPA SYSTEM.                                    *             
* THE POWER CODE IS EXTRACTED FROM THE MESSAGE, AND THE PROGRAM   *             
* DETERMINES WHICH REP SYSTEM SHOULD PROCESS THE MESSAGE (AS THE  *             
* REPFILE IS UPDATED, IT NEEDS WRITE ACCESS TO THE REP SYSTEM.    *             
* IF THE SYSTEM IS UPDATIVE ON REPA, PROGRAM SWITCHES TO THAT     *             
* SYSTEM AND UPDATES THE CONTRACT.                                *             
* IF NOT UPDATIVE ON REPA, PROGRAM CREATES A WORKER FILE (NWK)    *             
* WITH A NAME LIKE JDSFDDT, WHERE F IS THE FACPAK ID (B OR C)     *             
* OF THE SYSTEM WHERE THE REP IS UPDATIVE, DD IS THE DAY NUMBER,  *             
* AND T INDICATES IT IS A SCRIPT FILE.                            *             
* THE SIGNON IDEAS ARE JDSREPA, JDSREPB, AND JDSREPC AS REQUIRED  *             
*=================================================================*             
         SPACE 1                                                                
**********************************************************************          
* HISTORY OF CHANGES                                                 *          
**********************************************************************          
* NOV03/95 (BU ) --- ORIGINAL ENTRY                                  *          
*                                                                    *          
* NOV27/95 (WSB) --- ORIGINAL CODING, TAKES STORED AND SENT DATE AND *          
*                      TIME FROM SCREEN AND UPDATES THE ELEMENT      *          
*                                                                    *          
* DEC18/95 (BU ) --- GETEL DISPLACEMENT IS 34, NOT 27                *          
*                                                                    *          
* DEC19/95 (WSB) --- MADE IO AREAS 2000 BYTES LONG                   *          
*                                                                    *          
* FEB13/96 (WSB) --- ADD SUPPORT FOR REPB--USE WORKER FILES          *          
*                                                                    *          
* APR22/96 (WSB) --- ADD SUPPORT FOR ANY NUMBER OF REP FACPAKS ---   *          
*                    * ONLY NEED TO RELINK IF ADD NEW ONES *         *          
*                                                                    *          
* APR26/96 (WSB) --- IF GET CODES 'HR' OR 'PT' FROM JDS, REPLACE     *          
*                    THEM WITH CODES 'SZ' AND 'PV', RESPECTIVELY     *          
*                                                                    *          
* JUL25/96 (WSB) --- IN SUBROUTINE RECWRITE, DON'T ADDREC FIRST,     *          
*                    BECAUSE IT MIGHT UNNECESSARILY ADD A RECOVERY   *          
*                    REC.  INSTEAD TO A READHI FIRST.                *          
*                                                                    *          
* AUG26/96 (WSB) --- IF GET CODE 'KZ' LOOK UP DARE STATION RECORD TO *          
*                    FIND PROPER KATZ COMPANY POWERCODE (AM,CQ,NK)   *          
*                                                                    *          
* APR08/97 (BU ) --- PASS 'FN' (FOXNW) THROUGH AS 'NW'               *          
*                    PASS 'JB' (FOXNW) THROUGH AS '04'               *          
*                                                                    *          
* FEB26/98 (JRD) --- UPGRADE FOR 4K CONTRACTS                        *          
*                                                                    *          
*                    ***  END TOMBSTONE  ***                         *          
**********************************************************************          
T81200   CSECT                                                                  
         NMOD1 WORKX-WORKD,*CFI*,R8,RR=R3                                       
         USING WORKD,RC                                                         
         ST    R3,RELO                                                          
         LR    RE,RC                                                            
         AH    RE,=Y(IOAREA-WORKD)                                              
         ST    RE,AIO1                                                          
         LR    RE,RC                                                            
         AH    RE,=Y(IOAREA2-WORKD)                                             
         ST    RE,AIO2                                                          
*                                                                               
*     INITIALIZATION                                                            
*                                                                               
         MVC   PARAMS,0(R1)        SAVE PARAMETER LIST                          
*                                                                               
         L     RA,PARAM2           A(TWA)                                       
         USING T812FFD,RA                                                       
*                                                                               
         L     R6,PARAM5                                                        
         USING COMFACSD,R6         A(COMFACS)                                   
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   VSCANNER,CSCANNER                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VGETFACT,CGETFACT                                                
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VDATCON,CDATCON                                                  
         DROP  R6                                                               
*                                                                               
         MVC   ATIA,PARAM4         A(TIA)                                       
         L     R7,ATIA                                                          
         LA    R7,64(R7)           GO PAST 64 BYTE HEADER IN TIA                
         MVC   83(80,R7),86(R7)    MOVE 2ND SBA RIGHT AFTER 1ST                 
         MVC   LINEDATA,0(R7)      MOVE TO NEUTRAL AREA                         
         LA    R7,LINEDATA         AND POINT TO IT                              
         USING CFID,R7             LAY CFI LINE DSECT OVER IT                   
*                                                                               
*                                                                               
         GOTO1 VGETFACT,DMCB,0     GET WHICH SYSTEM WE ARE ON                   
         L     R3,DMCB                                                          
         USING FACTSD,R3                                                        
         MVC   THISSYS,FASYSID     SYSTEM NUM                                   
         DROP  R3                                                               
*                                                                               
         CLI   THISSYS,4           IS THIS REPA?                                
         BE    MAIN90              YES                                          
*                                                                               
         MVC   MSGFORID,THISSYS    NO, SET UP FOR SUBROUTINE                    
         BAS   RE,GETFACNM         GET FACPAK ONE CHAR NAME                     
         BAS   RE,WRKRREAD         READ A WORKER FILE                           
         B     MAIN100             SKIP SCREEN STUFF                            
*                                                                               
MAIN90   OI    CFICF1H+6,X'01'     SET FIELD TO MODIFIED                        
         MVC   CFIMSG,=CL60'#CFI READY'                                         
         OI    CFIMSGH+6,X'80'     DISPLAY 'CFI READY' PROMPT                   
*                                                                               
MAIN100  BAS   RE,CTRLSET          CHECK/SWITCH BET CTRL + REP                  
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,RDCONREC         READ THE CONTRACT RECORD                     
         BNE   EXIT                RECORD NOT FOUND                             
*                                                                               
         BAS   RE,PROCREC          GET THE DATES & TIMES AND ADD TO REC         
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*  CTRLSET:  SETS REP SE # BY LOOKING IN CONTROL FILE.                          
*                                                                               
CTRLSET  NTR1                                                                   
         GOTO1 VSWITCH,DMCB,=C'CON',0                                           
*                                                                               
         CLC   CFIPWRCD,=C'KZ'     IS CODE ON SCREEN 'KZ' (KATZ)?               
         BNE   *+16                NO                                           
         BAS   RE,GETCOMP          GO FIND WHICH KATZ COMPANY IT IS             
         BNE   NO                  COULDN'T FIND IT--IGNORE                     
         B     CTRL0100                                                         
*                                                                               
         CLC   CFIPWRCD,=C'HR'     IS CODE ON SCREEN 'HR' (SELTEL)?             
         BNE   *+14                NO                                           
         MVC   CFIPWRCD,=C'SZ'     YES, CHANGE TO CODE 'SZ'                     
         B     CTRL0100                                                         
*                                                                               
         CLC   CFIPWRCD,=C'PT'     IS CODE ON SCREEN 'PT' (PETRY)?              
         BNE   *+14                NO                                           
         MVC   CFIPWRCD,=C'PV'     YES, CHANGE TO CODE 'PV'                     
         B     CTRL0100                                                         
*                                                                               
         CLC   CFIPWRCD,=C'NW'     IS CODE ON SCREEN 'NW' (NEWORLD)?            
         BNE   *+14                NO                                           
         MVC   CFIPWRCD,=C'FN'     YES, CHANGE TO CODE 'FN'                     
         B     CTRL0100                                                         
*                                                                               
         CLC   CFIPWRCD,=C'04'     IS CODE ON SCREEN '04' (NW LOCAL)?           
         BNE   *+14                NO                                           
         MVC   CFIPWRCD,=C'JB'     YES, CHANGE TO CODE 'JB'                     
         B     CTRL0100                                                         
*                                                                               
CTRL0100 EQU   *                                                                
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CT5KEY,R4                                                        
         MVI   CT5KTYP,C'5'        FIND CONTROL FILE SYSTEM ACCESS REC          
*                                                                               
         MVC   CT5KALPH,CFIPWRCD   MOVE IN POWER CODE                           
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 ADATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO1                    
*                                  RETRIEVE CONTROL FILE RECORD                 
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R5,AIO1                                                          
         CLC   WORK(25),0(R5)      CHECK THE KEY                                
         BNE   NO                  IF POWER CODE NOT THERE, IGNORE              
         LA    R5,28(R5)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0120 EQU   *                                                                
         CLI   0(R5),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0160            NO                                           
         CLI   2(R5),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0200            YES                                          
CTRL0160 EQU   *                                                                
         ZIC   R0,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,R0                                                            
         CLI   0(R5),0             END OF RECORD?                               
         BNE   CTRL0120            NO                                           
*        DC    H'0'                NO X'21' - DUMP IT OUT                       
         B     NO                  DON'T DIE                                    
CTRL0200 EQU   *                                                                
         MVC   SENUM,3(R5)         GET SE NUMBER FOR MESSAGE                    
*                                                                               
         CLI   THISSYS,4           IS THIS REPA?                                
         BNE   CTRL0320            NO, GO AHEAD AND DO THE SWITCH               
*                                                                               
         BAS   RE,GETFACUP         YES, FIND WHICH FACPAK MSG IS FOR            
         CLI   MSGFORID,4          IS THIS MESSAGE FOR REPA?                    
         BE    CTRL0320            YES, CONTINUE                                
*                                                                               
         BAS   RE,GETFACNM         NO, GET FACPAK ONE CHAR NAME                 
         BAS   RE,WRKROPEN         CREATE A WORKER FILE                         
         BAS   RE,WRKRLINE                                                      
         BAS   RE,WRKRCLSE                                                      
         B     NO                                                               
*                                                                               
CTRL0320 DS    0H                                                               
         GOTO1 VSWITCH,DMCB,(SENUM,0),0                                         
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    YES                 YES                                          
*                                                                               
*        CLI   4(R1),2             SYSTEM NOT OPENED?                           
*        BE    NO                  RETURN WITH A 'NE'                           
*                                                                               
*        DC    H'0'                OTHERWISE DEATH                              
         MVC   CFIMSG,=CL60'DID NOT SWITCH CORRECTLY'                           
         OI    CFIMSGH+6,X'80'     DISPLAY ERROR MESSAGE                        
         B     NO                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* BORROWED FROM REREPED02                                                       
*                                                                               
*   ACCESS DARE STATION RECORD ON CONTROL FILE.  DETERMINE AND                  
*        SET THE COMPANY CODE FOR LATER USE                                     
*                                                                               
GETCOMP  NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAKEYD,R4                                                       
         MVI   STAKTYP,X'5A'       INSERT RECORD TYPE                           
         MVI   STAKMEDA,C'T'       INSERT MEDIA CODE                            
         MVC   STAKSTIN,CFISTATN   INSERT STATION LETTERS                       
         MVI   STAKSTIN+4,C'T'     INSERT 'TV'                                  
         PRINT GEN                                                              
         GOTO1 ADATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',WORK,AIO1                    
         PRINT NOGEN                                                            
         L     R5,AIO1                                                          
         CLC   WORK(25),0(R5)      CHECK THE KEY                                
         BNE   NO                  DIFFERS - DUMP IT OUT                        
GCOM0040 EQU   *                                                                
         LA    R2,36(R5)           SET A(DISK ADDRESS IN KEY)                   
         GOTO1 ADATAMGR,DMCB,(0,=C'GETREC'),=C'GENFILE',(R2),AIO1,     +        
               (0,DMWORK)                                                       
         L     R2,AIO1                                                          
         LA    R2,42(R2)           SET TO 1ST ELEMENT IN RECORD                 
GCOM0060 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    NO                  YES - NO REP ELEMENT                         
         CLI   0(R2),X'10'         REP ELEMENT?                                 
         BE    GCOM0080            YES - PROCESS IT                             
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF               ADD ELT LENGTH                               
         B     GCOM0060            GO BACK FOR NEXT ELEMENT                     
GCOM0080 EQU   *                                                                
         CLC   =C'KAM',2(R2)       KATZ AMERICAN?                               
         BNE   GCOM0100            NO                                           
         MVC   CFIPWRCD,=C'AM'     YES - SET COMPANY = AMERICAN                 
         B     GCOM0160                                                         
GCOM0100 EQU   *                                                                
         CLC   =C'KCO',2(R2)       KATZ CONTINENTAL?                            
         BNE   GCOM0120            NO                                           
         MVC   CFIPWRCD,=C'CQ'     YES - SET COMPANY = CONTINENTAL              
         B     GCOM0160                                                         
GCOM0120 EQU   *                                                                
         CLC   =C'KNA',2(R2)       KATZ NATIONAL?                               
         BNE   GCOM0160            NO                                           
         MVC   CFIPWRCD,=C'NK'     YES - SET COMPANY = NATIONAL                 
         B     GCOM0160                                                         
GCOM0160 EQU   *                                                                
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*   GETFACUP:  GET FACPAK THAT HAS UPDATIVE ACCESS GIVEN AN SE NUMBER           
*              INPUT:  SENUM                                                    
*              OUTPUT: MSGFORID                                                 
*                                                                               
** GET A(SYSFAC), USE TO GET SELIST, AND FIND THE PROPER SE NUMBER              
** IN THE LIST TO GET THE FACPAK THAT HAS THE UPDATIVE ACCESS                   
GETFACUP NTR1                                                                   
         GOTO1 VSWITCH,DMCB,X'FEFFFFFF',0     GET A(SYSFAC)                     
         L     R3,DMCB             A(SYSFAC)                                    
         USING SYSFACD,R3                                                       
         L     R4,VSELIST          A(SELIST)                                    
         DROP  R3                                                               
         USING SELISTD,R4                                                       
         LH    R2,0(R4)            LENGTH OF TABLE ENTRY                        
         L     R3,2(R4)            A(END OF TABLE)                              
         LA    R4,6(R4)            A(FIRST ENTRY)                               
*                                                                               
         CLC   SESYS,SENUM         MATCH ON SPECIFIC NUMBER?                    
         BE    *+10                YES                                          
         BXLE  R4,R2,*-10          NO, TRY NEXT ENTRY                           
         DC    H'0'                SE NUMBER NOT IN LIST                        
         MVC   MSGFORID,SEFACUP    GET FACPAK FOR UPDATIVE ACCESS               
         DROP  R4                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*   GETFACNM:  GET FACPAK ONE CHAR NAME GIVEN THE ID CODE                       
*              INPUT:  MSGFORID                                                 
*              OUTPUT: MSGFORNM                                                 
*                                                                               
GETFACNM NTR1                                                                   
         ZIC   R3,MSGFORID         FACPAK ID CODE                               
         LA    R4,L'FACIDTAB                                                    
         MR    R2,R4               INDEX INTO TABLE                             
         LA    R4,FACIDTAB                                                      
         AR    R3,R4               ADD TO START OF TABLE                        
         USING FACITABD,R3                                                      
         MVC   MSGFORNM,FACISN1    GET FACPAK ONE CHAR NAME                     
         DROP  R3                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*   RDCONREC:  READ IN THE CONTRACT RECORD USING THE CONTRACT NUMBER            
*              FROM THE SCREEN                                                  
*                                                                               
RDCONREC NTR1                                                                   
         L     R2,AIO1                                                          
         USING RCONREC,R2                                                       
*                                                                               
         XC    RCONKEY,RCONKEY     CLEAR THE CONTRACT KEY AREA                  
         MVI   RCONPTYP,X'8C'      SET KEY TYPE 8C                              
         MVC   RCONPREP,CFIPWRCD   INSERT REP ID FROM SCREEN                    
*                                                                               
*            COMPLEMENT/REVERSE CONTRACT NUMBER AND INSERT IN KEY               
         GOTO1 VHEXIN,DMCB,CFICONTR,TEMPHEX,8,=C'TOG'                           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),TEMPHEX                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   RCONPCON,WORK+15                                                 
*                                                                               
         MVC   KEY(27),RCONKEY                                                  
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 ADATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                  
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND? (ENTIRE KEY CHECKED)              
         BNE   NO                  NO                                           
         GOTO1 ADATAMGR,DMCB,(0,GETREC),=C'REPFILE',KEY+28,            X        
               AIO1,IOWORK            READ THE RECORD                           
         B     YES                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*     PROCREC: CHECKS TO SEE IF DATE/TIME ELEMENT ALREADY EXISTS.               
*              IF SO, IT UPDATES IT, IF NOT, IT ADDS A NEW ONE.                 
*              THE RECORD IS THEN WRITTEN BACK TO THE FILE                      
*                                                                               
PROCREC  NTR1                                                                   
         L     R6,AIO1                                                          
         MVI   ELCODE,X'16'        FIND DATE/TIME STORED/SENT ELEMENT           
         BAS   RE,GETEL            WAS THE ELEMENT FOUND?                       
         BNE   PR10                NO,  CREATE NEW ELEMENT                      
*                                                                               
         BAS   RE,WRDATTIM         YES, MODIFY CURRENT ELEMENT                  
         B     PR20                                                             
*                                                                               
PR10     LA    R6,ELTAREA          BUILD THE NEW ELEMENT HERE                   
         USING RCONE2EL,R6                                                      
         MVI   RCONE2CO,X'16'      MOVE IN ELEMENT CODE                         
         MVI   RCONE2LN,18         MOVE IN ELEMENT LENGTH                       
         DROP  R6                                                               
*                                                                               
         BAS   RE,WRDATTIM         ADD THE DATE/TIME INFO                       
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),AIO1,ELTAREA,            +        
               =C'ADD=CODE'        ADD ELEMENT TO RECORD                        
*                                                                               
PR20     BAS   RE,RECWRITE         WRITE OUT THE RECORD                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*   WRDATTIM:  SUBROUTINE TO TAKE THE STORED AND SENT DATES AND                 
*              TIMES FROM THE SCREEN AND PLACE THEM IN AN ELEMENT               
*                                                                               
WRDATTIM NTR1                                                                   
         USING RCONE2EL,R6                                                      
         GOTO1 VDATCON,DMCB,(4,CFISTDAT),(2,RCONE2DS)  STORED DATE              
         MVC   RCONE2TS(2),CFISTHOU        STORED HOUR                          
         MVC   RCONE2TS+2(2),CFISTMIN      STORED MINUTE                        
         MVC   RCONE2TS+4(2),CFISTSEC      STORED SECOND                        
*                                                                               
         GOTO1 VDATCON,DMCB,(4,CFISEDAT),(2,RCONE2DX)  SENT DATE                
         MVC   RCONE2TX(2),CFISEHOU        SENT HOUR                            
         MVC   RCONE2TX+2(2),CFISEMIN      SENT MINUTE                          
         MVC   RCONE2TX+4(2),CFISESEC      SEND SECOND                          
         DROP  R6                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*   RECWRITE:  CHECKS FOR KEYS, DELETED RECORDS, ETC....                        
*                                                                               
RECWRITE NTR1                                                                   
         PRINT GEN                                                              
         L     RE,AIO1                                                          
         MVC   KEY,0(RE)           SET KEY FROM NEW RECORD                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 ADATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY              
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    RECW0100            YES                                          
*                                                                               
         GOTO1 ADATAMGR,DMCB,(X'88',ADDREC),=C'REPFILE',KEY,           X        
               AIO1,IOWORK                                                      
*                                  ATTEMPT TO ADD NEW RECORD                    
         CLI   PAR3,0              ERROR RETURN?                                
         BZ    RECW0400            NO  - SUCCESSFUL                             
         DC    H'0'                YES - CAN'T PROCESS: DUMP                    
*                                                                               
*                                  OVERWRITE RECORD                             
RECW0100 NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         GOTO1 ADATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,        X        
               AIO2,IOWORK                                                      
*                                  RETRIEVE INTO IO AREA # 2                    
         L     RE,AIO1                                                          
         NI    29(RE),X'FF'-X'80'                                               
*                                  RESTORE CONTROL IN RECORD                    
         GOTO1 ADATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,        X        
               AIO1,IOWORK                                                      
*                                  REWRITE FROM NEW REC: IO AREA # 1            
         GOTO1 ADATAMGR,DMCB,(X'88',DMWRITE),=C'REPDIR',KEYSAVE,KEY             
*                                  REWRITE CLEARED KEY                          
         PRINT NOGEN                                                            
RECW0400 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   OPEN WORKER FILE                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKROPEN NTR1                                                                   
         L     R4,AIO2                                                          
         USING WLHDRD,R4           BUILD WORKER FILE                            
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'    START OF FILE                           
*                                                                               
         BAS   RE,GETUSID                                                       
         MVC   WLUSRID,USIDNUM     PROPER USERID FOR FACPAK                     
*                                                                               
         MVC   WLSYSPRG(3),=C'JDS'                                              
         MVC   WLSUBPRG,MSGFORNM   SET TO APPROPRIATE FACPAK                    
         GOTO1 VDATCON,DMCB,(5,0),(1,TDAY)                                      
         MVC   WLDAY,TDAY+2                                                     
         MVI   WLCLASS,C'T'        CLASS T FOR WRKF SCRIPTS                     
         MVI   WLTYPE,C'A'         TYPE A FOR IMMEDIATE EXEC                    
         MVC   WLPSWD,SPACESX                                                   
         OI    WLATTB,WLATOBJ                                                   
                                                                                
         MVI   FIXED,C'Y'                                                       
         BAS   RE,WRKR                                                          
         MVC   WRKFNO,WLREPRNO     WORKER FILE NUMBER                           
         MVI   FIXED,C'N'                                                       
         DROP  R4                                                               
                                                                                
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'JDSREPX '    PARTIAL SCRIPT NAME                     
         MVC   16(1,R1),MSGFORNM   SPECIFIC FACPAK                              
         MVI   18(R1),C'S'         INSERT MODE/USER SETS FINAL STATUS           
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'N'         DO NOT INSERT ERRORS AT FILE END             
*                                                                               
         L     R1,AIO2                                                          
         SH    R1,=H'4'                                                         
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
                                                                                
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000001'                                               
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
         MVC   30(7,R1),=C'JDSREPX'                                             
         MVC   36(1,R1),MSGFORNM     SPECIFIC FACPAK                            
                                                                                
         L     R1,AIO2                                                          
         SH    R1,=H'4'                                                         
         MVC   0(2,R1),=H'41'        37 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
                                                                                
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000002'                                               
         EDIT  WRKFNO,(5,30(R1)),0,ALIGN=LEFT   WORKER FILE ID                  
                                                                                
         L     R1,AIO2                                                          
         SH    R1,=H'4'                                                         
         MVC   0(2,R1),=H'39'                 35 + 4 BYTES FOR QSAM             
         BAS   RE,WRKR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   ADD WORKER FILE LINE                              *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKRLINE NTR1                                                                   
         MVC   LINELEN,=AL2(LINELENQ)                                           
         XC    LINENULL,LINENULL                                                
*                                                                               
         L     R4,AIO2                                                          
         MVC   0(4,R4),=F'2102'                                                 
         MVC   4(6,R4),=C'000003'                                               
         MVC   10(LINELENQ,R4),LINESAVE     DATA TO ADD                         
*                                                                               
         L     R4,AIO2                                                          
         SH    R4,=H'4'                                                         
         LA    RF,LINELENQ+14                                                   
         STH   RF,0(R4)                                                         
         BAS   RE,WRKR                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   CLOSE WORKER FILE                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKRCLSE NTR1                                                                   
         L     R4,AIO2                                                          
         USING WLHDRD,R4                                                        
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         MVI   FIXED,C'Y'                                                       
         BAS   RE,WRKR                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                    ADD LINE TO WORKER FILE                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKR     NTR1                                                                   
         L     R3,AIO2                                                          
         CLI   FIXED,C'Y'                                                       
         BE    *+12                                                             
         L     R3,AIO2                                                          
         SH    R3,=H'4'                                                         
         GOTO1 ADATAMGR,DMCB,DMPRINT,WRKFILE,0,(R3),ATIA                        
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                    READ WORKER FILE                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKRREAD NTR1                                                                   
         GOTO1 VSCANNER,DMCB,(0,CFICF1H),(1,WORK)  GET WORKER FILE....          
         TM    WORK+2,X'80'    NUMERIC?            ...NUM FROM SCREEN           
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         MVC   WRKFNO,WORK+6       READ THIS SPECIFIC WORKER FILE               
*                                                                               
         XC    WRKRINDX,WRKRINDX                                                
         LA    R2,WRKRINDX                                                      
         USING UKRECD,R2                                                        
*                                                                               
         BAS   RE,GETUSID                                                       
         MVC   UKUSRID,USIDNUM     PROPER USERID FOR FACPAK                     
*                                                                               
         MVC   UKSYSPRG(3),=C'JDS'                                              
         MVC   UKSUBPRG,MSGFORNM   SET TO APPROPRIATE FACPAK                    
         MVC   UKFILENO,WRKFNO     GET THIS WORKER FILE                         
         OI    UKFLAG,X'80'                                                     
*                                                                               
         GOTO1 ADATAMGR,DMCB,DINDEX,WRKFILE,WRKRINDX,AIO2,ATIA                  
         NI    UKFLAG,X'FF'-X'80'              ...READ THE WORKER FILE          
         DROP  R2                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'             DIE IF ERRORS (SHOULDN'T BE EOF EITHER)         
*                                                                               
         L     R1,AIO2                                                          
         XC    0(12,R1),0(R1)                                                   
         MVC   0(4,R1),=F'4'     RANDOM READ OF 4TH RECORD IN FILE              
         MVC   4(4,R1),=C'REC '                                                 
         GOTO1 ADATAMGR,DMCB,RANDOM,WRKFILE,WRKRINDX,AIO2,ATIA                  
         CLI   8(R1),0                        ...READ THE RECORD                
         BE    *+6                                                              
         DC    H'0'             DIE IF ERRORS (SHOULDN'T BE EOF EITHER)         
*                                                                               
         L     RE,AIO2                                                          
         MVC   LINEDATA,18(RE)        GET THE DATA FROM FILE                    
*                                                                               
         GOTO1 ADATAMGR,DMCB,=C'PROC',WRKFILE,WRKRINDX,AIO2,ATIA                
         CLI   8(R1),0                   ...CHANGE STATUS TO PROCESSED          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADATAMGR,DMCB,DCLOSE,WRKFILE,0,AIO2,ATIA   CLOSE IT              
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*   GETUSID:   GET USER ID GIVEN FACPAK SIGN-ON NAME                            
*              INPUT:  MSGFORNM                                                 
*              OUTPUT: USIDNUM                                                  
*                                                                               
GETUSID  NTR1                                                                   
         XC    KEY(25),KEY                                                      
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),=CL10'JDSREPX '                                       
         MVC   KEY+21(1),MSGFORNM                                               
         GOTO1 ADATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,AIO1                     
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R6,AIO1                                                          
         LA    R6,28(R6)                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   USIDNUM,2(R6)                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
*   LOCAL VALUES                                                                
*                                                                               
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMWRITE  DC    CL8'DMWRT   '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKFILE'                                                     
DINDEX   DC    CL8'INDEX'                                                       
DREAD    DC    CL8'READ'                                                        
DCLOSE   DC    CL8'CLOSE'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
FOXZEROS DC    20C'0'                                                           
SPACESX  DC    132C' '                                                          
*                                                                               
       ++INCLUDE FACIDTAB                                                       
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
WORKD    DSECT                                                                  
PARAMS   DS    0XL24                                                            
PARAM1   DS    A                                                                
PARAM2   DS    A                                                                
PARAM3   DS    A                                                                
PARAM4   DS    A                                                                
PARAM5   DS    A                                                                
PARAM6   DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
         ORG   DMCB                                                             
PAR1     DS    F                                                                
PAR2     DS    F                                                                
PAR3     DS    F                                                                
PAR4     DS    F                                                                
PAR5     DS    F                                                                
PAR6     DS    F                                                                
*                                                                               
RELO     DS    A                                                                
AIO1     DS    A                                                                
AIO2     DS    A                                                                
ATIA     DS    A                                                                
AUTL     DS    A                                                                
FULL     DS    F                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
KEY      DS    CL32                                                             
KEYSAVE  DS    CL32                                                             
HALF     DS    H                                                                
BYTE     DS    C                                                                
*                                                                               
ADATAMGR DS    A                                                                
VSCANNER DS    A                                                                
VHEXIN   DS    A                                                                
VGETFACT DS    A                                                                
VSWITCH  DS    A                                                                
VHELLO   DS    A                                                                
VDATCON  DS    A                                                                
*                                                                               
DMWORK   DS    12D                                                              
WORK     DS    CL64                                                             
*                                                                               
ELTAREA  DS    CL200               ELEMENT BUILD AREA                           
ELCODE   DS    X                   ELEMENT CODE                                 
TEMPHEX  DS    F                                                                
FIXED    DS    CL1                                                              
TDAY     DS    CL3                 YYMMDD PWOS                                  
TDAYEBC  DS    CL6                 TODAY'S DATE EBCDIC YYMMDD                   
WRKFNO   DS    XL2                 WORKER FILE NUMBER                           
WRKRINDX DS    CL42                                                             
THISSYS  DS    XL1                 WHICH SYSTEM WE ARE ON                       
SENUM    DS    XL1                 SE NUM OF SYSTEM MSG IS FOR                  
MSGFORID DS    XL1                 ID # OF FACPAK MSG IS FOR                    
MSGFORNM DS    CL1                 ONE CHAR NAME OF FACPAK MSG IS FOR           
USIDNUM  DS    XL2                 USER ID NUMBER (FOR WORKER FILE)             
*                                                                               
LINESAVE DS    0CL104                                                           
LINELEN  DS    CL2                                                              
LINENULL DS    CL2                                                              
LINEDATA DS    CL100                                                            
LINELENQ EQU   *-LINESAVE                                                       
*                                                                               
IOWORK   DS    12D                 IO WORK AREA                                 
IOAREA   DS    4000C                                                            
IOAREA2L DS    F                                                                
IOAREA2  DS    4000C                                                            
*                                                                               
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FASYSFAC                                                       
         EJECT                                                                  
       ++INCLUDE FASELIST                                                       
         EJECT                                                                  
       ++INCLUDE FACIDTABD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTGENSTAD                                                      
         EJECT                                                                  
       ++INCLUDE RECFIFFD                                                       
         EJECT                                                                  
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE DMWRKFL                                                        
         EJECT                                                                  
       ++INCLUDE DMWRKFK                                                        
         EJECT                                                                  
CFID     DSECT                                                                  
         DS    CL3                 3 BYTE FIELD HEADER                          
         DS    CL14                                                             
CFIPWRCD DS    CL2                 POWER CODE                                   
         DS    CL1                                                              
CFISTATN DS    CL4                 STATION                                      
         DS    CL4                                                              
CFICONTR DS    CL8                 CONTRACT NUMBER                              
         DS    CL10                                                             
CFISTDAT DS    CL8                 STORED DATE                                  
         DS    CL1                                                              
CFISTHOU DS    CL2                 STORED HOUR                                  
         DS    CL1                                                              
CFISTMIN DS    CL2                 STORED MINUTE                                
         DS    CL1                                                              
CFISTSEC DS    CL2                 STORED SECOND                                
         DS    CL9                                                              
CFISEDAT DS    CL8                 SENT DATE                                    
         DS    CL1                                                              
CFISEHOU DS    CL2                 SENT HOUR                                    
         DS    CL1                                                              
CFISEMIN DS    CL2                 SENT MINUTE                                  
         DS    CL1                                                              
CFISESEC DS    CL2                 SENT SECOND                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019RECFI00   03/17/98'                                      
         END                                                                    
