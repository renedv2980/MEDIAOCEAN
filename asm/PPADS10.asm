*          DATA SET PPADS10    AT LEVEL 057 AS OF 03/16/99                      
*PHASE T40B10A,*                                                                
*        TITLE 'PPADS10 - CONTACTS'                                             
         TITLE 'PPADS10 - CONTACTS - HISTORY'                                   
**********************************************************************          
*                                                                    *          
*        PPADS10 (T40B10) --- CONTACTS                               *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
*   OCT98  (BOBY)--- LET THERE BE LIGHT                              *          
*                                                                    *          
**********************************************************************          
         TITLE 'PPADS10 - CONTACTS - INIT'                                      
**********************************************************************          
*                                                                    *          
*        PPADS10 (T40B10) --- CONTACTS                               *          
*                                                                    *          
*        INITIALIZATION                                              *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
T40B10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40B10**,RR=R3                                                 
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO                                                          
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         MVI   IPSTAT,0            INIT INPUT STATUS                            
*                                                                               
         GOTO1 VHELPCHK CHECK IF IN MIDDLE OF HELP CALL                         
*                                                                               
         MVI   ACTELOPT,C'N'       DON'T ADD GENCON ACTIVITY ELEMENT            
*                                                                               
         TITLE 'PPADS10 - CONTACTS - VALMODE'                                   
**********************************************************************          
*                                                                    *          
*        PPADS10 (T40B10) --- CONTACTS                               *          
*                                                                    *          
*        DETERMINE CALLING MODE                                      *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VALMODE  DS    0H                                                               
*                                                                               
         CLI   MODE,PROCPFK        PFKEY HIT                                    
         BE    PKEY                                                             
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    DREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
*                                  UNKNOWN MODE                                 
         XIT1                                                                   
*                                                                               
         TITLE 'PPADS10 - CONTACTS - PKEY'                                      
***********************************************************************         
*                                                                     *         
*        PFKEY HIT                                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PKEY     DS    0H                                                               
*                                                                               
*        CLEAR ITERATION SWITCH                                                 
*        NEED TO BECAUSE IT IS IN RESTORED SAVEAREA                             
*        AND WE DO NOT NEED TO BE CONFUSED BY LAST TIME'S VALUE                 
*                                                                               
         MVI   ITERSW,0                                                         
*                                                                               
PKEYX    DS    0H                  ENSURE WORK AND SAVE FIELDS SET              
         XIT1                                                                   
*                                                                               
         TITLE 'PPADS10 - CONTACTS - DKEY'                                      
***********************************************************************         
*                                                                     *         
*              DISPLAY KEY ROUTINE                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DKEY     DS    0H                                                               
*                                                                               
*        FIND KEY VALUES                                                        
*                                                                               
         L     R4,AIO              POINT TO HEADER RECORD                       
         USING GPUBKEY,R4          ESTABLISH RECORD                             
*                                                                               
         MVC   SRDTYP,GPUBPUBT     SET SRDS TYPE                                
         MVC   SRDNUM,GPUBPUB      SET SRDS PUB CODE                            
*                                                                               
*        FIND HEADER ELEMENT                                                    
*                                                                               
         MVI   ELCODE,GPUBHEQU     ELEMENT CODE                                 
         LR    R6,R4               START OF RECORD                              
         BRAS  RE,GETEL            FIND ELEMENT                                 
         BE    *+6                 MUST FIND IT                                 
         DC    H'0'                                                             
*                                                                               
         USING GPUBHEL,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   SRDTIT,GPUBHALN     SET PUB NAME                                 
*                                                                               
*        DISPLAY SRDS PUBLICATION TYPE                                          
*                                                                               
DKTYP    DS    0H                                                               
*                                                                               
         LA    R2,CNTTYPH          TYPE                                         
*                                                                               
         GOTO1 VDSPSTYP            COMMON DISPLAY ROUTINE                       
*                                                                               
DKTYPX   DS    0H                                                               
*                                                                               
*        DISPLAY SRDS PUBLICATION TITLE                                         
*                                                                               
DKTIT    DS    0H                                                               
*                                                                               
         LA    R2,CNTTITH          TITLE                                        
*                                                                               
         GOTO1 VDSPSTIT            COMMON DISPLAY ROUTINE                       
*                                                                               
DKTITX   DS    0H                                                               
*                                                                               
*        DISPLAY SRDS DATA TYPE                                                 
*                                                                               
DKDAT    DS    0H                                                               
*                                                                               
         LA    R2,CNTDATAH         SRDS DATA TYPE                               
*                                                                               
         GOTO1 VVALSDAT            RE-VALIDATE IN CASE SRDDATA                  
*                                  RESTORED WITH SCREEN                         
         GOTO1 VDSPSDAT            COMMON DISPLAY ROUTINE                       
*                                                                               
DKDATX   DS    0H                                                               
*                                                                               
DKEYX    B     VKEY                ENSURE WORK AND SAVE FIELDS SET              
*                                                                               
         TITLE 'PPADS10 - CONTACTS - VKEY'                                      
**********************************************************************          
*                                                                    *          
*        VALIDATE KEY ROUTINE                                        *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VKEY     DS    0H                                                               
*                                                                               
VKTYP    DS    0H                  VALIDATE SRDS PUBLICATION TYPE               
*                                                                               
         LA    R2,CNTTYPH          TYPE - REQUIRED                              
*                                                                               
         GOTO1 VVALSTYP            COMMON VALIDATION ROUTINE                    
*                                                                               
VKTYPX   DS    0H                                                               
*                                                                               
VKTIT    DS    0H                  VALIDATE SRDS PUBLICATION TITLE              
*                                                                               
         LA    R2,CNTTITH          TITLE - NOT REQUIRED                         
*                                                                               
         GOTO1 VVALSTIT            COMMON VALIDATION ROUTINE                    
*                                                                               
VKTITX   DS    0H                                                               
*                                                                               
VKDATA   DS    0H                  VALIDATE DATA TYPE                           
*                                                                               
         LA    R2,CNTDATAH         DATA  - REQUIRED                             
*                                                                               
         GOTO1 VVALSDAT            COMMON VALIDATION ROUTINE                    
*                                                                               
VKDATX   DS    0H                                                               
*                                                                               
*        BUILD HEADER RECORD KEY                                                
*                                                                               
VKKEY    DS    0H                                                               
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS CONTROL PUB KEY                 
         USING GPUBKEYD,R4                                                      
*                                                                               
         MVI   GPUBREC,GPUBRECQ    SET RECORD CODE                              
         MVI   GPUBTYP,GPUBTYPQ    SET RECORD TYPE                              
*                                                                               
         MVC   GPUBPUBT,SRDTYP     SET PUB TYPE                                 
         MVC   GPUBPUB,SRDNUM      SET PUB NUMBER                               
         XC    GPUBAGY,GPUBAGY     SKIP AGENCY OVERRIDE                         
         MVI   GPUBSTYP,0          SET FOR HEADER RECORD                        
*                                                                               
*        CHECK FOR CHANGE OF KEY                                                
*        JUST RE-ENTERING A KEY FIELD CAUSES RE-START OF DISPLAY                
*                                                                               
VKNEW    DS    0H                                                               
*                                                                               
         MVI   NEWKEY,C'N'         ASSUME KEY UNCHANGED                         
*                                                                               
         CLC   HDRKEY,KEY          RESTART WITH NEW KEY                         
         BE    *+12                                                             
         MVI   NEWKEY,C'Y'            FORCE NEW KEY                             
         B     VKNEWX                                                           
*                                                                               
         TM    CNTDATAH+4,X'80'    IF DATA TYPE INPUT THIS TIME                 
         BO    *+8                                                              
         TM    CNTTYPH+4,X'80'     IF PUB  TYPE INPUT THIS TIME                 
         BO    *+8                                                              
         TM    CNTTITH+4,X'80'     IF TITLE     INPUT THIS TIME                 
         BNO   VKNEWX                                                           
*                                                                               
         MVI   NEWKEY,C'Y'            INDICATE NEW KEY                          
*                                                                               
VKNEWX   DS    0H                                                               
*                                                                               
         MVC   HDRKEY,KEY          SAVE HEADER KEY                              
*                                                                               
VKEYX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PPADS10 - CONTACTS - DREC'                                      
***********************************************************************         
*                                                                     *         
*              DISPLAY RECORD ROUTINE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DREC     DS    0H                                                               
*                                                                               
         OI    GENSTAT2,RETEQSEL   DEFAULT IS RETURN HERE NEXT TIME             
*                                                                               
         CLI   PFAID,12            IF PF12 GO TO NEXT SELECTION                 
         BE    *+8                                                              
         CLI   PFAID,24                                                         
         BNE   DRPFKYN                                                          
*                                                                               
         CLI   ITERSW,0               SKIP IF NOT FIRST TIME                    
         BNE   DRPFKYN                                                          
*                                                                               
         MVI   ITERSW,X'FF'           RESET ITERATION SWITHC                    
         OI    GENSTAT2,NEXTSEL       GO TO NEXT SELECTION NOW                  
*                                                                               
         B     DRECX                                                            
*                                                                               
DRPFKYN  DS    0H                                                               
*                                                                               
         LA    R4,KEY              ESTABLISH PUB RECORD KEY                     
         USING GPUBKEYD,R4                                                      
*                                                                               
         CLC   GPUBKEY,HDRKEY      MUST FIND RECORD                             
         BNE   DRNOTFE                                                          
*                                                                               
*        DISPLAY MAIN ADDRESS                                                   
*                                                                               
         L     R4,AIO              ESTABLISH HEADER RECORD                      
         USING GPUBKEYD,R4                                                      
*                                                                               
         MVC   CNTMCOM,SPACES      INIT PUB NAME                                
         MVC   CNTMADR,SPACES      INIT  FIRST  LINE OF ADDRESS                 
         MVC   CNTMCSZ,SPACES      INIT CITY STATE AND ZIP FIELD                
*                                                                               
         LR    R6,R4               FIND MAIN PUB ELEMENT                        
         MVI   ELCODE,GPUBHEQU                                                  
         BRAS  RE,GETEL                                                         
         BNE   DRMNAMX                                                          
*                                                                               
         USING GPUBHD,R6                                                        
*                                                                               
         MVC   CNTMCOM,GPUBHALN    PUB NAME                                     
*                                                                               
DRMNAMX  DS    0H                                                               
*                                                                               
         MVI   CNTMCOMH+6,FOUTTRN  FORCE TRANSMISSION OF FIELD                  
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,GPUBREQU     FIND ADDRESS ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNE   DRMADRX                                                          
*                                                                               
         USING GPUBRD,R6                                                        
*                                                                               
         MVC   CNTMADR,GPUBRAL1     FIRST  LINE OF ADDRESS                      
*                                                                               
         CLC   CNTMADR,SPACES       FIND A PRINTABLE LINE                       
         JH    *+10                                                             
         MVC   CNTMADR,GPUBRAL2     SECOND LINE OF ADDRESS                      
*                                                                               
         CLC   CNTMADR,SPACES       FIND A PRINTABLE LINE                       
         JH    *+10                                                             
         MVC   CNTMADR,GPUBRAL3     THIRD  LINE OF ADDRESS                      
*                                                                               
*        DISPLAY CITY/STATE/ZIP                                                 
*                                                                               
         LA    RF,L'GPUBRCTY       CITY FIELD LENGTH                            
         LA    RE,L'CNTMCSZ        CITY OUTPUT FIELD LENGTH                     
*                                                                               
         CR    RF,RE               USE SMALLER                                  
         JNH   *+6                                                              
         LR    RF,RE                                                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   CNTMCSZ(0),GPUBRCTY  DISPLAY CITY                                
*                                                                               
*        FIND LAST NON-SPACE IN CITY NAME                                       
*                                                                               
         LA    RF,L'CNTMCSZ         LENGTH OF CITY FIELD                        
         LA    R1,CNTMCSZ+L'CNTMCSZ-1 LAST CHARACTER IN CITY FIELD              
*                                                                               
         CLI   0(R1),C' '          FIND NON BLANK                               
         JH    *+10                                                             
         BCTR  R1,0                BACK UP A POSITION                           
         BRCT  RF,*-10                                                          
*                                                                               
         MVI   1(R1),C','          ADD COMMA                                    
         LA    R1,3(R1)            REPOSITION OUTPUT POINTER                    
*                                                                               
         LA    RE,CNTMCSZ+L'CNTMCSZ                                             
         SR    RE,R1               LENGTH OF REMAINING SPACE                    
*                                                                               
         LA    RF,L'GPUBRST        ASSUME PRINTING STATE CODE                   
         LA    R7,GPUBRST                                                       
*                                                                               
         CLC   GPUBRST,SPACES      IF NO STATE CODE                             
         JH    *+12                                                             
         LA    RF,L'GPUBRPRV          USE PROVINCE FIELD                        
         LA    R7,GPUBRPRV                                                      
*                                                                               
         CR    RF,RE               IF NOT ENOUGH ROOM TO PRINT STATE            
         JNH   *+6                                                              
         LR    RF,RE                  DEFAULT TO WHAT IS LEFT                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                DETERMINE WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),0(R7)       PRINT STATE/PROVINCE                         
*                                                                               
         LA    RF,L'CNTMCSZ         LENGTH OF CITY FIELD                        
         LA    R1,CNTMCSZ+L'CNTMCSZ-1 LAST CHARACTER IN CITY FIELD              
*                                                                               
         CLI   0(R1),C' '          FIND NON BLANK                               
         JH    *+10                                                             
         BCTR  R1,0                BACK UP A POSITION                           
         BRCT  RF,*-10                                                          
*                                                                               
         LA    R1,2(R1)            REPOSITION OUTPUT POINTER                    
*                                                                               
         LA    RE,CNTMCSZ+L'CNTMCSZ                                             
         SR    RE,R1               LENGTH OF REMAINING SPACE                    
*                                                                               
         LA    RF,L'GPUBRZIP       ASSUME PRINTING ZIP CODE                     
         LA    R7,GPUBRZIP                                                      
*                                                                               
         CR    RF,RE               IF NOT ENOUGH ROOM TO PRINT ZIP              
         BNH   *+6                                                              
         LR    RF,RE                  DEFAULT TO WHAT IS LEFT                   
*                                                                               
         LR    R0,RE               SAVE SPACE AVAILABLE                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                DETERMINE WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),0(R7)       PRINT ZIP CODE                               
*                                                                               
         CHI   RF,8                IF 9 DIGIT USA ZIP CODE - EX LENGTH          
         JL    DRMZIPX                                                          
         CHI   R0,10               AND THERE IS ROOM TO PRINT                   
         JL    DRMZIPX                                                          
         CLC   5(4,R7),=4C'0'         AND LAST 4 DIGITS GIVEN                   
         JL    DRMZIPX                                                          
         JH    *+14                                                             
         MVC   5(4,R1),=4C' '         WIPE OUT 0000                             
         J     DRMZIPX                                                          
*                                                                               
         MVI   5(R1),C'-'                                                       
         MVC   6(4,R1),5(R7)                                                    
*                                                                               
DRMZIPX  DS    0H                                                               
*                                                                               
DRMADRX  DS    0H                                                               
*                                                                               
         MVI   CNTMCOMH+6,FOUTTRN  FORCE TRANSMISSION OF FIELD                  
         MVI   CNTMADRH+6,FOUTTRN  FORCE TRANSMISSION OF FIELD                  
         MVI   CNTMCSZH+6,FOUTTRN  FORCE TRANSMISSION OF FIELD                  
*                                                                               
*        PRINT SHIPPING ADDRESS                                                 
*                                                                               
*        PRINT COMPANY NAME                                                     
*                                                                               
         MVC   CNTSCOM,SPACES      INIT SHIPPING COMPANY NAME                   
         MVC   CNTSADR,SPACES      INIT ADDRESS LINE                            
         MVC   CNTSCSZ,SPACES      INIT DISPLAY CITY                            
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,GPUBSEQU     FIND SHIPPING NAME ELEMENT                   
         BRAS  RE,GETEL                                                         
         BNE   DRSNAMX                                                          
*                                                                               
         USING GPUBSD,R6                                                        
*                                                                               
         MVC   CNTSCOM,GPUBSCOM     SHIPPING COMPANY NAME                       
*                                                                               
DRSNAMX  DS    0H                                                               
*                                                                               
*        PRINT SHIPPING COMPANY ADDRESS                                         
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,GPUBTEQU     FIND SHIPPING ADDRESS ELEMENT                
         BRAS  RE,GETEL                                                         
         BNE   DRSADRX                                                          
*                                                                               
         USING GPUBTD,R6                                                        
*                                                                               
         MVC   CNTSADR,GPUBTADR     ADDRESS LINE                                
*                                                                               
*        DISPLAY CITY/STATE/ZIP                                                 
*                                                                               
         LA    RF,L'GPUBTCTY       CITY FIELD LENGTH                            
         LA    RE,L'CNTSCSZ        CITY OUTPUT FIELD LENGTH                     
*                                                                               
         CR    RF,RE               USE SMALLER                                  
         BNH   *+6                                                              
         LR    RF,RE                                                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   CNTSCSZ(0),GPUBTCTY  DISPLAY CITY                                
*                                                                               
*        FIND LAST NON-SPACE IN CITY NAME                                       
*                                                                               
         LA    RF,L'CNTSCSZ        LENGTH OF CITY FIELD                         
         LA    R1,CNTSCSZ+L'CNTSCSZ-1 LAST CHARACTER IN CITY FIELD              
*                                                                               
         CLI   0(R1),C' '          FIND NON BLANK                               
         JH    *+10                                                             
         BCTR  R1,0                BACK UP A POSITION                           
         BRCT  RF,*-10                                                          
*                                                                               
         MVI   1(R1),C','          ADD COMMA                                    
         LA    R1,3(R1)            REPOSITION OUTPUT POINTER                    
*                                                                               
         LA    RE,CNTSCSZ+L'CNTSCSZ                                             
         SR    RE,R1               LENGTH OF REMAINING SPACE                    
*                                                                               
         LA    RF,L'GPUBTST        ASSUME PRINTING STATE CODE                   
         LA    R7,GPUBTST                                                       
*                                                                               
         CR    RF,RE               IF NOT ENOUGH ROOM TO PRINT STATE            
         BNH   *+6                                                              
         LR    RF,RE                  DEFAULT TO WHAT IS LEFT                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                DETERMINE WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),0(R7)       PRINT STATE                                  
*                                                                               
         LA    RF,L'CNTSCSZ        LENGTH OF CITY FIELD                         
         LA    R1,CNTSCSZ+L'CNTSCSZ-1 LAST CHARACTER IN CITY FIELD              
*                                                                               
         CLI   0(R1),C' '          FIND NON BLANK                               
         JH    *+10                                                             
         BCTR  R1,0                BACK UP A POSITION                           
         BRCT  RF,*-10                                                          
*                                                                               
         LA    R1,2(R1)            REPOSITION OUTPUT POINTER                    
*                                                                               
         LA    RE,CNTSCSZ+L'CNTSCSZ                                             
         SR    RE,R1               LENGTH OF REMAINING SPACE                    
*                                                                               
         LA    RF,L'GPUBTZIP       ASSUME PRINTING ZIP CODE                     
         LA    R7,GPUBTZIP                                                      
*                                                                               
         CR    RF,RE               IF NOT ENOUGH ROOM TO PRINT ZIP              
         BNH   *+6                                                              
         LR    RF,RE                  DEFAULT TO WHAT IS LEFT                   
*                                                                               
         LR    R0,RE               SAVE SPACE AVAILABLE                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                DETERMINE WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),0(R7)       PRINT ZIP CODE                               
*                                                                               
         CHI   RF,8                IF 9 DIGIT USA ZIP CODE - EX LENGTH          
         JL    DRSZIPX                                                          
         CHI   R0,10               AND THERE IS SPACE AVAILABLE                 
         JL    DRSZIPX                                                          
         CLC   5(4,R7),=4C'0'         AND LAST 4 DIGITS GIVEN                   
         JL    DRSZIPX                                                          
         JH    *+14                                                             
         MVC   5(4,R1),=4C' '         WIPE OUT 0000                             
         J     DRSZIPX                                                          
*                                                                               
         MVI   5(R1),C'-'                                                       
         MVC   6(4,R1),5(R7)                                                    
*                                                                               
DRSZIPX  DS    0H                                                               
*                                                                               
DRSADRX  DS    0H                                                               
*                                                                               
         MVI   CNTSCOMH+6,FOUTTRN  FORCE TRANSMISSION OF FIELD                  
         MVI   CNTSADRH+6,FOUTTRN  FORCE TRANSMISSION OF FIELD                  
         MVI   CNTSCSZH+6,FOUTTRN  FORCE TRANSMISSION OF FIELD                  
*                                                                               
         BRAS  RE,LINSET           PRINT PERSONNEL DATA                         
*                                                                               
DRECX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
DRNOTFE  DS    0H                  CONTACTS RECORD NOT FOUND                    
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         LHI   RF,PPECNTNF                                                      
         B     DRERR                                                            
*                                                                               
DRERR    DS    0H                                                               
         STCM  RF,3,GERROR         SET ERROR CODE                               
         GOTO1 VCURSERR                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS10 - CONTACTS - BUMP'                                      
***********************************************************************         
*                                                                     *         
*        BUMP SCREEN POINTER TO NEXT FIELD                            *         
*                                                                     *         
*NTRY    R2==> CURRENT FIELD                                          *         
*                                                                     *         
*EXIT    R2==> NEXT FIELD - UNPROTECTED OR PROTECTED                  *         
*                           DEPENDING ON ENTRY POINT                  *         
*        CC '='  INDICATES END OF SCREEN REACHED                      *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         JNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         TITLE 'PPADS10 - CONTACTS - GETEL'                                     
***********************************************************************         
*                                                                     *         
*        GETEL MACRO                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         GETEL R6,42,ELCODE        GETEL MACRO                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS10 - CONTACTS - LINSET'                                    
**********************************************************************          
*                                                                    *          
*   LINUP INTERFACE                                                  *          
*     - BUILD LUBLK, CREATE ELEMENT TABLE, CALL LINUP                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         DS    0D                                                               
LINSET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         XC    GERROR,GERROR       INIT ERROR FIELD                             
*                                                                               
         LA    R5,LUBLK            POINT TO  LINUP CONTROL BLOCK                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         XC    LUBLKD(LUBLKL),LUBLKD   CLEAR LINUP CONTROL BLOCK                
*                                                                               
         MVC   LUNEW,NEWKEY        SET NEW OR OLD RECORD INDICATOR              
*                                                                               
         MVC   LUATWA,ATWA         PASS A(TWA)                                  
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         ST    R1,LUATIOB                                                       
*                                                                               
         MVI   LUNLINS,NLINS       SET NUMBER OF LINES ON SCREEN                
         MVI   LUNFLDS,6               FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,CNTCTT1H         A(FIRST FIELD)                               
         LA    R4,NLINS            NUMBER OF LINES                              
*                                                                               
*        DO COLUMN OF DISPLACEMENTS                                             
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
         BRAS  RE,BUMP             BUMP TO START OF NEXT LINE                   
         BRAS  RE,BUMP                                                          
         BRAS  RE,BUMP                                                          
         BRAS  RE,BUMP                                                          
         BRAS  RE,BUMP                                                          
         BRAS  RE,BUMP                                                          
         BRCT  R4,*-40             ADD NEXT LINE TO LIST                        
*                                                                               
         XC    0(2,R3),0(R3)       FORCE NULLS AT END OF LIST                   
*                                                                               
         MVI   LUSCROLL,LUHALFQ    SCROLL FACTOR OF A HALF IS DEFAULT           
*                                                                               
         CLI   CNTSCRL,C'P'        CHECK FOR PAGE SCROLL                        
         JE    *+8                                                              
         CLI   CNTSCRL,X'97'         LOWERCASE 'P'                              
         JNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   CNTSCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         JE    *+8                                                              
         CLI   CNTSCRL,X'88'            LOWERCASE 'H'                           
         JNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    CNTSCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         JNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CNTSCRLH+5     FIELD INPUT LENGTH                           
         JZ    LS051               NO ENTRY                                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                ESTABLISH WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         B     *+10                                                             
         PACK  DUB,CNTSCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,LUSCROLL         PASS SCROLL AMOUNT                           
*                                                                               
LS051    DS    0H                                                               
*                                                                               
         CLI   PFAID,19            CHECK FOR UP KEY                             
         JE    *+8                                                              
         CLI   PFAID,7             CHECK FOR UP KEY                             
         JNE   *+8                                                              
         MVI   LUPFKEY,LUPFUPQ                                                  
*                                                                               
         CLI   PFAID,20            CHECK FOR DOWN KEY                           
         JE    *+8                                                              
         CLI   PFAID,8             CHECK FOR DOWN KEY                           
         JNE   *+8                                                              
         MVI   LUPFKEY,LUPFDNQ                                                  
*                                                                               
*        CLI   LUPFKEY,0           IF VALID PFKEY HIT                           
*        JE    *+8                                                              
*        OI    GENSTAT2,RETEQSEL   RE-DISPLAY SAME SCREEN                       
*                                                                               
         CLI   MODE,VALREC         SET LINUP MODE                               
         JNE   *+8                                                              
         MVI   LUAPMODE,LUAPVALQ   VALIDATE                                     
*                                                                               
         CLI   MODE,DISPREC        SET LINUP MODE - DISPREC                     
         JE    *+8                                                              
         CLI   MODE,XRECADD        RE-DISPLAY AFTER ADD                         
         JE    *+8                                                              
         CLI   MODE,XRECPUT        RE-DISPLAY AFTER CHANGE                      
         JE    *+8                                                              
         CLI   MODE,XRECDEL        RE-DISPLAY AFTER DELETE                      
         JE    *+8                                                              
         CLI   MODE,RECDEL         DELETE RECORD                                
         JE    *+8                                                              
         CLI   MODE,XRECREST       RE-DISPLAY AFTER RESTORE                     
         JNE   *+8                                                              
         MVI   LUAPMODE,LUAPDSPQ   DISPLAY                                      
*                                                                               
         MVI   LUCNTL,LUBACKQ      WINDOW SUPPORTS BACKWARD SCROLLING           
*                                                                               
         TM    IPSTAT,LUWCURSQ     CHECK IF CURSOR FOUND YET                    
         JNO   *+8                 NO - NO CURSOR SELECT                        
         OI    LUCNTL,LUCURSQ      YES - MAKE CURSOR SENSITIVE                  
*                                                                               
         LA    RF,LINHOOK          PROCESSING ROUTINE                           
         ST    RF,LUHOOK                                                        
*                                                                               
         LHI   RF,LSVTABL                                                       
         STCM  RF,3,LUSVLEN        SAVED BYTES PER LINE                         
*                                                                               
         LA    RF,LSVTAB           LINUP SAVE AREA                              
         ST    RF,LUSVTAB                                                       
*                                                                               
         XC    SVELTKEY,SVELTKEY   INIT ELEMENT KEY SAVEAREA                    
*                                                                               
*              BUILD TABLE OF ELEMENTS                                          
*                                                                               
         BRAS  RE,LSBLDTAB            BUILD ELEM TABLE                          
*                                                                               
*                                     FIRST LINE UP CALL                        
*                                     ------------------                        
*                                                                               
         MVI   SVDIR,0             INIT DIRECTION SAVEAREA                      
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD     LINUP                                     
*                                                                               
         OC    IPSTAT,LUWSTAT      OR IN WINDOW INPUT STATUS                    
*                                                                               
         CLI   LUAPMODE,LUAPVALQ      TEST VALIDATING                           
         JNE   LSMOR                                                            
*                                                                               
         TM    IPSTAT,LUWVERRQ     CONTINUE IF NO ERRORS                        
         JNO   LS22                                                             
*                                  ELSE                                         
         MVC   LSVTAB,SVLSVTAB     RESTORE LINUP SAVE TABLE                     
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         J     LSX                 AND DON'T UPDATE ELEMS                       
*                                                                               
LS22     DS    0H                                                               
*                                                                               
*        ALWAYS UPDATE THE RECORD                                               
*                                                                               
****     TM    IPSTAT,LUSNPVQ      UPDATE RECORD IF THERE WAS AT LEAST          
****     BNO   *+8                 ONE NON-PREVIOUSLY VALIDATED FIELD           
****     TM    IPSTAT,LUSDATQ      AND DATA EMATRED IN SOME FIELD               
****     BNO   LS23                                                             
*                                                                               
*****    GOTO1 =A(LSWRTTAB),RR=RELO  WRITES CHANGES TO RECORD                   
*                                                                               
LS23     DS    0H                                                               
*                                                                               
         TM    IPSTAT,LUSNPVQ      IF ALL PREVIOUSLY VALIDATED                  
         JO    LSNCHA                                                           
         CLI   ACTNUM,ACTCHA       AND ACTION IS CHANGE                         
         JNE   LSNCHA              THEN WANT TO RE-DISPLAY IN CASE OF           
*                                    NEED TO SCROLL                             
         TM    LUSTAT,LUCLEARQ     SKIP IF CLEAR COMMAND ISSUED                 
         JO    LSNCHA                                                           
*                                    NEED TO SCROLL                             
         MVI   LUAPMODE,LUAPDSPQ   SET FOR DISPLAY                              
         MVI   MODE,DISPREC        SET FOR DISPLAY RECORD                       
         MVI   LUWSTAT,0           RESET WINDOW STAT                            
*                                                                               
         MVI   SVDIR,0             INIT DIRECTION SAVEAREA                      
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD SCROLL IF NEEDED                              
*                                                                               
LSNCHA   DS    0X                                                               
*                                                                               
LSMOR    DS    0X                  SET 'MORE' FIELDS                            
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         JO    LSLOW                 CLEARED - LEAVE UPPER AS IS                
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   ONLY IF IN DISPLAY MODE                      
         JNE   LSMORX                                                           
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,CNTCMR1H         POINT TO FIRST MORE FIELD                    
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST TABLE ENTRY                   
         USING ELTABD,R4           ESTABLISH ENTRY                              
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT START AT START            
         JNH   *+10                OF TABLE THEN SET UP INDICATOR               
         MVC   FLD(2),=C'<<'                                                    
*                                                                               
         MVC   CNTCMR1,FLD         DISPLAY IT                                   
         OI    CNTCMR1H+6,FOUTTRN  TRANSMIT FIELD                               
*                                                                               
LSLOW    DS    0H                                                               
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,CNTCMRLH         POINT TO LAST MORE FIELD                     
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         JO    LSLOWOUT              CLEARED                                    
*                                                                               
         ZIC   RF,LUNLINS          GET NUMBER OF LINES ON SCREEN                
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MHI   RF,LSVTABL          GET INDEX                                    
         AR    R3,RF               POINT TO LAST ELEMENT IN TABLE               
         L     R4,ELTLAST          POINT TO LAST ELEMENT IN TABLE               
*                                                                               
         OC    LSVKEY,LSVKEY       NULLS INDICATE END OF TABLE ALREADY          
         JZ    LSLOWOUT            ON SCREEN                                    
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT END AT END                
         JNL   *+10                OF TABLE THEN SET DOWN INDICATOR             
         MVC   FLD(2),=C'>>'                                                    
*                                                                               
LSLOWOUT DS    0H                                                               
*                                                                               
         MVC   CNTCMRL,FLD         DISPLAY IT                                   
         OI    CNTCMRLH+6,FOUTTRN  TRANSMIT FIELD                               
*                                                                               
LSMORX   DS    0X                                                               
*                                                                               
LSX      DS    0H                                                               
*                                                                               
         CLI   LUERR,0             RESET CC                                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                  LINES IN WINDOW                              
NLINS    EQU   ((CNTCTTLH-CNTCTT1H)/(CNTCTT2H-CNTCTT1H))+1                      
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS10 - CONTACTS - LINHOOK'                                   
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP PROCESSING HOOK                                  *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LINHOOK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         CLI   LUMODE,LUVALQ       VALIDATE                                     
         JE    LHVAL                                                            
         CLI   LUMODE,LUDSPQ       DISPLAY                                      
         JE    LHDIS                                                            
         CLI   LUMODE,LUMOREQ      MORE TO DISPLAY                              
         JE    LHMORE                                                           
*                                                                               
         DC    H'0'                INVALID MODE                                 
*                                                                               
         TITLE 'PPADS10 - CONTACTS - LHVAL'                                     
**********************************************************************          
*                                                                    *          
*        LINHOOK - LINUP VALIDATION HOOK ROUTINE                     *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LHVAL    DS    0H                                                               
*                                                                               
*        VALIDATION MODE NOT AVAILABLE                                          
*                                                                               
LHVALX   DS    0H                                                               
         J     LHOOKX                                                           
*                                                                               
         TITLE 'PPADS10 - CONTACTS - LHDIS'                                     
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP DISPLAY HOOK ROUTINE                             *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LHDIS    DS    0H                                                               
*                                                                               
         MVI   LUSTAT,0            INIT STATUS                                  
*                                                                               
         L     R4,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,R4                                                       
         XC    0(LSVTABL,R4),0(R4)  CLEAR IT                                    
*                                                                               
         BRAS  RE,LHSRCH           FIND ELEM TO DISPLAY                         
         BRAS  RE,LHDISLIN         BUILD SCREEN LINE                            
*                                                                               
LHDISX   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
         TITLE 'PPADS10 - CONTACTS - LHMORE'                                    
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP MORE    HOOK ROUTINE                             *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
*                                  DO 'MORE' MESSAGE                            
*                                  -----------------                            
LHMORE   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
LHOOKX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS10 - BUILD TABLE OF ELEMENTS ON FILE - LSBLDTAB'           
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSBLDTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         XC    BSPPRMS(BSPPRML),BSPPRMS INIT BINSRCH PARAMETERS                 
*                                                                               
         L     R1,AIO2             STORE TABLE IN I/O2/3                        
         ST    R1,BSPATAB          PASS TABLE ADDRESS                           
         LA    R1,ELTABL           PASS ENTRY LENGTH                            
         ST    R1,BSPLENR                                                       
         LA    R1,ELTKEYL          PASS KEY LENGTH                              
         ST    R1,BSPLENK                                                       
         MVI   BSPKEYD,0           PASS KEY DISPLACEMENT                        
         LA    R1,ELTMAX           PASS MAXIMUM COUNT                           
         ST    R1,BSPMAX                                                        
*                                                                               
*        INITIALIZE TABLE AREA                                                  
*                                                                               
         LA    R0,ELTMAX           MAX ENTRIES IN TABLE                         
         L     R1,BSPATAB          TABLE AREA                                   
         LA    RF,ELTABL           LENGTH OF ONE ENTRY IN TABLE                 
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         BASR  RE,0                                                             
*                                                                               
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         XC    0(0,R1),0(R1)       CLEAR ENTRY                                  
         LA    R1,1(RF,R1)         NEXT ENTRY                                   
         BRCT  R0,*-18                                                          
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         SR    R0,R0               INITIALIZE COUNTER                           
*                                                                               
         L     R6,AIO1             POINT TO HEADERT RECORD                      
         MVI   ELCODE,GPUBEEQU     SET PERSONNEL ID                             
         BRAS  RE,GETEL            FIND FIRST PERSON                            
*                                                                               
LSBTLOOP DS    0H                                                               
*                                                                               
         USING GPUBED,R6           ESTABLISH EXTENSIONS ELEMENT                 
*                                                                               
         JNE   LSBTDONE            NO ELEMENTS LEFT                             
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,GPUBELN          ELEMENT LENGTH                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                ESTABLISH WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   ELTELEM(0),GPUBEEL  ADD ELEMENT TO TABLE                         
*                                                                               
         AHI   R0,1                BUMP COUNTER                                 
         STC   R0,ELTSORT          SET KEY                                      
*                                                                               
         GOTO1 VBINSRCH,BSPPRMS,('BSPADD',WRKELTAB)                             
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FIT INTO TABLE               
         JNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
LSBTCONT DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL                                                        
*                                                                               
         J     LSBTLOOP                                                         
*                                                                               
LSBTDONE DS    0H                                                               
*                                                                               
         CLI   ELCODE,GPUBEEQU     DONE IF NOT PERSONNEL ELM SOUGHT             
         JNE   *+20                                                             
         L     R6,AIO1             RE-POINT TO HEADER RECORD                    
         MVI   ELCODE,GPUBLEQU     SET FOR EXTENSIONS ELEMENT                   
         BRAS  RE,GETEL            FIND FIRST PERSON                            
         B     LSBTLOOP                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
*        ADD IN SHIPPING CONTACT NAME                                           
*                                                                               
         L     R6,AIO1             POINT TO HEADERT RECORD                      
         MVI   ELCODE,GPUBSEQU     SET SHIPPING ID                              
         BRAS  RE,GETEL            FIND ELEMENT                                 
         BNZ   LSBTSHPX            NONE FOUND                                   
*                                                                               
         USING GPUBSD,R6           ESTABLISH SHIPPING ELEMENT                   
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         LA    R3,ELTELEM          POINT TO ELEMENT IN TABLE                    
         USING GPUBED,R3           ESTABLISH AS EXTENSIONS ELEMENT              
*                                                                               
         MVC   GPUBEN1,GPUBSN1     FIRST  NAME                                  
         MVC   GPUBEN2,GPUBSN2     MIDDLE NAME                                  
         MVC   GPUBEN3,GPUBSN3     LAST   NAME                                  
         MVC   GPUBETLE(L'GPUBSTLE),GPUBSTLE  TITLE                             
*                                                                               
         AHI   R0,1                BUMP COUNTER                                 
         STC   R0,ELTSORT          SET KEY                                      
*                                                                               
         GOTO1 VBINSRCH,BSPPRMS,('BSPADD',WRKELTAB)                             
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FIT INTO TABLE               
         JNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
LSBTSHPX DS    0H                                                               
*                                                                               
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         JZ    *+10                                                             
         BCTR  R1,0                MINUS ONE                                    
         MHI   R1,ELTABL           TIMES ENTRY LENGTH                           
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
*                                                                               
LSBLDTBX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS10 - BUILD TABLE OF ELEMENTS ON FILE - LHSRCH'             
***********************************************************************         
*                                                                     *         
*        ROUTINE TO SEARCH TABLE FOR ELEMENT                          *         
*        AND SET ADDRESS IN ELTENT                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHSRCH   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         XC    ELTENT,ELTENT       INIT ELEMENT ADDRESS RETURN                  
         NI    LUSTAT,255-LUEOLQ   SET OFF END OF LIST INDICATOR                
*                                                                               
         OC    BSPNOR,BSPNOR       IF NO ENTRIES                                
         JZ    LHSRCHX             RETURN EMPTY-HANDED                          
*                                                                               
         L     R4,BSPATAB          DEFAULT TO FIRST ENTRY IN TABLE              
         USING ELTABD,R4                                                        
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
         USING LSVTABD,R3                                                       
*                                                                               
         MVC   HALF,LSVKEY         COPY KEY                                     
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         JZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                                                               
         OC    HALF,HALF           NO PREVIOUS ENTRY MEANS FIRST TIME           
         JNZ   LHSRCH02            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         JNE   LHSRCH11               AFTER AN UP SCROLL                        
*                                                                               
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         J     LHSRCHX                                                          
*                                                                               
LHSRCH02 DS    0H                                                               
*                                                                               
         CLC   HALF,HIVALS         IF PREVIOUS IS X'FF'S                        
         JNE   LHSRCH05                                                         
*                                                                               
         L     RE,BSPNOR              USE LAST ENTRY IN TABLE                   
         L     RF,BSPLENR             RECORD LENGTH                             
         MR    RE,RE                  INDEX TO LAST ENTRY                       
         LA    R4,0(RF,R4)            A(LAST TABLE ENTRY)                       
         J     LHSRCH10                                                         
*                                                                               
LHSRCH05 DS    0H                                                               
*                                                                               
         GOTO1 VBINSRCH,BSPPRMS,('BSPRDHI',HALF)                                
*                                                                               
         L     R4,BSPAREC          GET TABLE ENTRY ADDRESS                      
*                                                                               
         CLI   BSPCNTL,BSPNF       DEFAULT TO FIRST OF TABLE IF                 
         JNE   LHSRCH10            PREVIOUS ENTRY WAS NOT FOUND                 
*                                                                               
         L     R4,BSPATAB          NO, POINT TO FIRST-(END OF TABLE)            
         J     LHSRCH11            DONE (NO MOVEMENT)                           
*                                                                               
LHSRCH10 DS    0H                                                               
*                                                                               
         CLI   LUDIR,C'-'          CHECK FOR BACKWARD SCROLLING                 
         JE    LHSRCH16                                                         
*                                                                               
         CLI   LUDIR,C'='          SKIP BUMPING ENTRY IF RE-DISPLAYING          
         JE    *+8                                                              
         LA    R4,ELTABL(R4)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
LHSRCH11 DS    0H                                                               
*                                                                               
         C     R4,ELTLAST          DONE IF NOT AT END OF TABLE                  
         JL    LHSRCH30                                                         
         JE    LHSRCH12            AT END OF TABLE TELL LINUP                   
*                                  PAST END - ONLY IF SCROLLING DOWN            
*                                  AND PRIOR SCREEN ENDED WITH LAST             
*                                  ELEMENT IN TABLE - USE DEFAULT               
*                                  OR UP SCROLLING AND TABLE HAS ONLY           
*                                  ONE ELEMENT - STOP WITH NO DISPLAY           
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         JNE   *+12                AFTER AN UP SCROLL                           
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         J     LHSRCHX                                                          
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST ENTRY IN TABLE                
         CLC   BSPNOR,=F'1'        DONE IF MORE THAN ONE ENTRY IN TABLE         
         JH    LHSRCH30                                                         
*                                  EXIT WITHOUT FINDING ELEMENT                 
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         J     LHSRCHX                                                          
*                                                                               
LHSRCH12 DS    0H                                                               
*                                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         J     LHSRCH30                                                         
*                                                                               
LHSRCH16 DS    0H                  GOING BACKWARDS                              
*                                                                               
         C     R4,BSPATAB          IF AT START                                  
         JNH   LHSRCH18               DONT GO FURTHER                           
*                                                                               
         AHI   R4,-ELTABL             BACK UP AN ENTRY                          
*                                                                               
LHSRCH17 DS    0H                                                               
*                                                                               
         C     R4,BSPATAB          IF AT START                                  
         JH    *+8                                                              
LHSRCH18 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
*                                                                               
LHSRCH30 DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTDELQ+ELTADDQ DISPLAY CHANGED ELEMENTS                  
         JO    LHSRCH40                                                         
         TM    ELTCTL,ELTDELQ      BYPASS DELETED ELEMENTS                      
         JNO   LHSRCH40                                                         
         TM    LUSTAT,LUEOLQ       EXIT IF AT END OF LIST                       
         JO    LHSRCHX                                                          
         CLI   LUDIR,C'='          QUIT IF RE-DISPLAY                           
         JE    LHSRCH40                                                         
         J     LHSRCH10            ELSE GO FIND NEXT ELEMENT                    
*                                                                               
LHSRCH40 DS    0H                                                               
*                                                                               
         ST    R4,ELTENT           RETURN TABLE ENTRY ADDRESS                   
         L     R3,LUACTAB          POINT TO CURRENT SAVE TABLE ENTRY            
         MVC   LSVKEY,ELTKEY       SAVE APPROPRIATE DATA                        
         MVC   LSVKEYNW,ELTKEYNW   SAVE APPROPRIATE DATA                        
*                                                                               
LHSRCHX  DS    0H                                                               
         MVC   SVDIR,LUDIR         SAVE LAST TIME DIRECTION                     
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
HIVALS   DC    32X'FF'             HIGH VALUES                                  
*                                                                               
         TITLE 'PPADS10 - CONTACTS - LHDISLIN'                                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHDISLIN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         L     R2,LUACLIN          A(FIRST FIELD)                               
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
*        TITLE FOR JOB TITLE FIELD                                              
*                                                                               
*        INIT FIELD                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               HEADER LENGTH                                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENED HEADER                
         BNO   *+8                                                              
         AHI   RF,-8                  TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),SPACES   INIT FIELD                                   
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         JZ    LHDLTTCX            CHECK IF NONE FOUND                          
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
*        PRINT TITLE FOR JOB TITLE                                              
*                                                                               
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),LHDLTTLC DISPLAY TITLE FOR JOB TITLE                  
*                                                                               
LHDLTTCX DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION OF FIELD                  
*                                                                               
*        JOB TITLE                                                              
*                                                                               
         BRAS  RE,BUMP             BUMP TO TITLE FIELD                          
*                                                                               
*        INIT FIELD                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               HEADER LENGTH                                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENED HEADER                
         BNO   *+8                                                              
         AHI   RF,-8                  TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),SPACES   INIT FIELD                                   
*                                                                               
*        PRINT JOB TITLE                                                        
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         JZ    LHDLTTLX            CHECK IF NONE FOUND                          
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING GPUBED,R3           ESTABLISH AS EXTENSIONS ELEMENT              
*                                                                               
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),GPUBETLE DISPLAY JOB TITLE                            
*                                                                               
LHDLTTLX DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION OF FIELD                  
*                                                                               
*        PRINT TITLE FOR CONTACT NAME                                           
*                                                                               
         BRAS  RE,BUMP             BUMP TO NAME TITLE FIELD                     
*                                                                               
*        INIT FIELD                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               HEADER LENGTH                                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENED HEADER                
         BNO   *+8                                                              
         AHI   RF,-8                  TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),SPACES   INIT FIELD                                   
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         JZ    LHDLTNCX            CHECK IF NONE FOUND                          
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
*        PRINT TITLE FOR NAME FIELD                                             
*                                                                               
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),LHDLTNMC DISPLAY TITLE FOR JOB TITLE                  
*                                                                               
LHDLTNCX DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION OF FIELD                  
*                                                                               
         BRAS  RE,BUMP             BUMP TO NAME                                 
*                                                                               
*        INIT FIELD                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               HEADER LENGTH                                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENED HEADER                
         BNO   *+8                                                              
         AHI   RF,-8                  TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),SPACES   INIT FIELD                                   
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         JZ    LHDLNAMX            CHECK IF NONE FOUND                          
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING GPUBED,R3           ESTABLISH AS EXTENSIONS ELEMENT              
*                                                                               
         CHI   RF,L'GPUBEN1-1      USE SMALLER EXECUTE LENGTH                   
         BNH   *+8                                                              
         LA    RF,L'GPUBEN1-1                                                   
*                                                                               
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),GPUBEN1   DISPLAY FIRST NAME                          
*                                                                               
*        FIND LAST NON-SPACE IN FIRST NAME                                      
*                                                                               
         LA    RF,L'CNTCCN1        LENGTH OF CONTACT NAME FIELD                 
         LA    R1,FLDDATA+L'CNTCCN1-1 LAST CHARACTER IN NAME FIELD              
*                                                                               
         CLI   0(R1),C' '          FIND NON BLANK                               
         JH    *+12                                                             
         BCTR  R1,0                BACK UP A POSITION                           
         BRCT  RF,*-10                                                          
         BCTR  R1,0                NO DATA - RECTIFY POINTER                    
*                                                                               
         LA    R1,2(R1)            REPOSITION OUTPUT POINTER                    
*                                                                               
         LA    RE,FLDDATA+L'CNTCCN1                                             
         SR    RE,R1               LENGTH OF REMAINING SPACE                    
*                                                                               
         LA    RF,L'GPUBEN2        PRINT MIDDLE NAME                            
         LA    R7,GPUBEN2                                                       
*                                                                               
         CR    RF,RE               IF NOT ENOUGH ROOM TO PRINT NAME             
         BNH   *+6                                                              
         LR    RF,RE                  DEFAULT TO WHAT IS LEFT                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                DETERMINE WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),0(R7)       PRINT MIDDLE NAME                            
*                                                                               
         LA    RF,L'CNTCCN1        LENGTH OF CONTACT NAME FIELD                 
         LA    R1,FLDDATA+L'CNTCCN1-1 LAST CHARACTER IN NAME FIELD              
*                                                                               
         CLI   0(R1),C' '          FIND NON BLANK                               
         JH    *+12                                                             
         BCTR  R1,0                BACK UP A POSITION                           
         BRCT  RF,*-10                                                          
         BCTR  R1,0                NO DATA - RECTIFY POINTER                    
*                                                                               
         LA    R1,2(R1)            REPOSITION OUTPUT POINTER                    
*                                                                               
         LA    RE,FLDDATA+L'CNTCCN1                                             
         SR    RE,R1               LENGTH OF REMAINING SPACE                    
*                                                                               
         LA    RF,L'GPUBEN3        PRINT LAST NAME                              
         LA    R7,GPUBEN3                                                       
*                                                                               
         CR    RF,RE               IF NOT ENOUGH ROOM TO PRINT NAME             
         BNH   *+6                                                              
         LR    RF,RE                  DEFAULT TO WHAT IS LEFT                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                DETERMINE WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),0(R7)       PRINT LAST NAME                              
*                                                                               
LHDLNAMX DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION OF FIELD                  
*                                                                               
*        PRINT PHONE NUMBER                                                     
*                                                                               
*        PRINT TITLE FOR CONTACT PHONE NUMBER                                   
*                                                                               
         BRAS  RE,BUMP             BUMP TO PHONE TITLE FIELD                    
*                                                                               
*        INIT FIELD                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               HEADER LENGTH                                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENED HEADER                
         BNO   *+8                                                              
         AHI   RF,-8                  TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),SPACES   INIT FIELD                                   
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         JZ    LHDLTPCX            CHECK IF NONE FOUND                          
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
*        PRINT TITLE FOR NAME FIELD                                             
*                                                                               
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),LHDLTPHC DISPLAY TITLE FOR JOB TITLE                  
*                                                                               
LHDLTPCX DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION OF FIELD                  
*                                                                               
         BRAS  RE,BUMP             BUMP TO PHONE FIELD                          
*                                                                               
*        INIT FIELD                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               HEADER LENGTH                                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENED HEADER                
         BNO   *+8                                                              
         AHI   RF,-8                  TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),SPACES   INIT FIELD                                   
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         JZ    LHDLTELX            CHECK IF NONE FOUND                          
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING GPUBED,R3           ESTABLISH AS EXTENSIONS ELEMENT              
*                                                                               
         LA    R1,FLDDATA          START OF OUTPUT FIELD                        
*                                                                               
         CLC   GPUBEAC,SPACES      IF THERE IS AN AREACODE                      
         BNH   *+22                                                             
         MVI   0(R1),C'('             PRINT IT                                  
         MVC   1(L'GPUBEAC,R1),GPUBEAC                                          
         MVI   1+L'GPUBEAC(R1),C')'                                             
         LA    R1,2+L'GPUBEAC(R1)     BUMP OUTPUT POINTER                       
*                                                                               
         CLC   GPUBEEX,SPACES      IF THERE IS AN EXCHANGE                      
         BNH   *+14                                                             
         MVC   0(L'GPUBEEX,R1),GPUBEEX                                          
         LA    R1,L'GPUBEEX(R1)       BUMP OUTPUT POINTER                       
*                                                                               
         CLC   GPUBENUM,SPACES     IF THERE IS A NUMBER                         
         BNH   *+18                                                             
         MVI   0(R1),C'-'             PRINT IT                                  
         MVC   1(L'GPUBENUM,R1),GPUBENUM                                        
         LA    R1,1+L'GPUBENUM(R1)    BUMP OUTPUT POINTER                       
*                                                                               
         CLC   GPUBEEXT,=16C'0'    IF THERE IS A EXTENSION                      
         BNH   *+18                                                             
         MVI   1(R1),C'X'             PRINT IT                                  
         MVC   2(L'GPUBEEXT,R1),GPUBEEXT                                        
         LA    R1,2+L'GPUBEEXT(R1)    BUMP OUTPUT POINTER                       
*                                                                               
LHDLTELX DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION OF FIELD                  
*                                                                               
LHDLX    DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
LHDLTTLC DC    XL8'E389A39385000000'  'TITLE' IN LOWERCASE                      
LHDLTNMC DC    XL8'D581948500000000'  'NAME'  IN LOWERCASE                      
LHDLTPHC DC    XL8'D788969585000000'  'PHONE' IN LOWERCASE                      
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS10 - TEXT POSTING - DSECTS'                                
***********************************************************************         
*                                                                     *         
*        HIDDEN DSECTS                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* PPADSFFD                                                                      
* DDGENTWA                                                                      
* PPADSD6D                                                                      
* PPADSWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PPADSFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PPADSD0D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE PPADSWORKD                                                     
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
         DS    16A                 RELOCATION FACTORS                           
LSVTAB   DS    XL(NLINS*LSVTABL)   LINE UP SAVE TABLE                           
*                                                                               
*               WORK AREA                                                       
*                                                                               
         DS    0F                                                               
HDRKEY   DS    XL(L'KEY)           MASTER KEY SAVEAREA                          
*                                                                               
FLD      DS    CL80                FIELD WORKAREA                               
ALINCUR  DS    A                   A(LINE WITH CURSOR)                          
LINLAST  DS    A                   A(LAST LINE WITH TEXT)                       
*                                                                               
WRKELTAB DS    XL(ELTABL)          WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
SVELTKEY DS    XL(L'ELTKEY)        ELEMENT KEY SAVEAREA                         
         DS    0F                                                               
*                                                                               
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
LUBLK    DS    XL(LUBLKL)          LINUP CONTROL BLOCK                          
         DS    0F                                                               
LINDSPS  DS    XL((NLINS+1)*2)                                                  
SVLSVTAB DS    XL(NLINS*LSVTABL)      HOLD COPY OF LINUP SAVE TABLE             
ELTMAX   EQU   40                     MAX NUMBER OF ELEMENTS IN TABLE           
DISPSW   DS    X                   DISPLAY SWITCH                               
DISPREDO EQU   X'80'               RE-DISPLAY SCREEN                            
SVDIR    DS    XL1                 DIRECTION SAVEAREA                           
*                                                                               
*DDFLDIND                                                                       
*DDFLDHDR                                                                       
*FATIOB                                                                         
*FAGETTXTD                                                                      
*CTGENPUB                                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENPUBS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE DDLINUPD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL2                 SORT VALUE - LINE #                          
LSVKEYL  EQU   *-LSVTABD                                                        
LSVKEYNW DS    XL2                 NEW KEY AFTER WRITING RECORD                 
LSVTABL  EQU   *-LSVTABD                                                        
*                                                                               
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL2                 SORT VALUE- LINE #                           
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTKEYNW DS    XL2                 NEW KEY AFTER WRITING RECORD                 
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTELEM  DS    XL(GPUBELNQ)        ELEMENT                                      
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057PPADS10   03/16/99'                                      
         END                                                                    
