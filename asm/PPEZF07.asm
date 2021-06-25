*          DATA SET PPEZF07    AT LEVEL 096 AS OF 09/23/97                      
*PHASE T43007A,*                                                                
*        TITLE 'T43007 - INVOICE HEADER MAINT'                                  
         TITLE 'T43007 - INVOICE HEADER MAINT'                                  
***********************************************************************         
*                                                                     *         
*  TITLE: T43007 - PRINT EPIC INVOICE HEADER MAINT                              
*  COMMENTS: THIS PROGRAM LISTS INVOICES AND CAN OVERIDE CLIENT       *         
*            AND/OR PRODUCT FOR EACH INVOICE.                         *         
*                                                                     *         
*  OUTPUTS: UPDATED INVOICE BATCHES                                   *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO INVOICE RECORD                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER, PZBLOCK         *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (PPPZF00-T43000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2     - WORKER RECORD                                *         
*             AIO3     - PZBLOCK                                      *         
*             WRKFBUFR - WORKER BUFFER                                *         
*                      - INDEX RECORD                                 *         
*                                                                     *         
***********************************************************************         
         TITLE 'T43007 - INVOICE HEADER MAINT - INITIALIZATION'                 
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
T43007   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3007**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RC,SVRC                                                          
         ST    R2,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
         CLC   =A(WRKFEND-SYSD),LSYSD                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(WRKFBUFR-SYSD)   SET UP WORKER BUFFER ADDRESS              
         LA    R1,SYSD(R1)                                                      
         ST    R1,WRKFBUFA                                                      
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - MODE'                           
***********************************************************************         
*                                                                     *         
*        DETERMINE PROCESSING MODE                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    MODINVAL                                                         
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    MODINVAL                                                         
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    MODINVAL                                                         
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    MODINVAL                                                         
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    MODINVAL                                                         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
MODINVAL MVI   ERROR,INVACT        INVALID MODE                                 
         LA    R2,CONACTH                                                       
         B     MODERRX                                                          
*                                                                               
MODERRX  DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - VKEY'                           
***********************************************************************         
*                                                                     *         
*        VKEY - VALIDATE KEY                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKEY     DS    0H                                                               
*                                                                               
*        IF NO FIELDS ARE CHANGED, JUST KEEP ON WITH DISPLAY *                  
*                                                                               
         TM    DIVMEDH+4,X'20'     BATCH MEDIA                                  
         BZ    VK000                                                            
         TM    DIVPUBH+4,X'20'     BATCH PUB                                    
         BZ    VK000                                                            
         TM    DIVBDTH+4,X'20'     BATCH DATE                                   
         BZ    VK000                                                            
         TM    DIVBSQH+4,X'20'     BATCH SEQ                                    
         BZ    VK000                                                            
         B     VKXIT               NO CHANGES TO SCREEN - EXIT                  
*                                                                               
*        VALIDATE KEY FIELDS                                                    
*                                                                               
VK000    MVI   NEWDISP,C'N'        ASSUME NO RE-DISPLAY                         
*                                                                               
         MVI   CURSYST,C'M'        INDICATE WE ARE IN A MEDIA SYS-PRINT         
*                                                                               
         GOTO1 VALIFAS             SWITCH TO MEDIA SYSTEM                       
*                                                                               
         LA    R2,DIVMEDH          POINT TO MEDIA FIELD HEADER                  
*                                                                               
         GOTO1 VALIMED             VALIDATE MEDIA                               
*                                                                               
         MVC   MEDKEYSV,KEY        SAVE MEDIA KEY                               
*                                                                               
VKMEDX   DS    0H                                                               
*                                                                               
         OI    DIVMEDH+4,X'20'     INDICATE VALID FIELD                         
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
VKPUB    DS    0H                                                               
*                                                                               
         LA    R2,DIVPUBH          POINT TO PUB FIELD                           
*                                                                               
         XC    RQPUB,RQPUB         KILL FILTER PUB                              
*                                                                               
         CLI   5(R2),0             SKIP IF PUB NOT ENTERED                      
         BE    VKPUBX                                                           
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         GOTO1 VALIPUB             VALIDATE PUB ENTRY                           
*                                                                               
         CLC   RQPUB,QPUB          IF PUB HAS CHANGED                           
         BE    VKPUBX                                                           
*                                                                               
         MVI   NEWDISP,C'Y'           FORCE RE-DISPLAY OF SCREEN                
*                                                                               
         MVC   RQPUB,QPUB          SAVE PUB ID                                  
         XC    DIVPUBN,DIVPUBN     PUT OUT NEW PUBNAME                          
         MVC   DIVPUBN(L'PUBPNM),PUBPNM                                         
         OI    DIVPUBNH+6,X'80'    FORCE FIELD TRANSMISSION                     
         XC    DIVZONE,DIVZONE     PUT OUT NEW ZONE NAME                        
         MVC   DIVZONE(L'PUBPZNM),PUBPZNM                                       
         OI    DIVZONEH+6,X'80'    FORCE FIELD TRANSMISSION                     
*                                                                               
VKPUBX   OI    4(R2),X'20'         INDICATE PUB IS VALID                        
*                                                                               
         LA    R2,DIVBDTH          POINT TO BATCH DATE                          
*                                                                               
         MVI   DTPLUS,C'N'         ASSUME EXACT DATE ENTERED                    
*                                                                               
         CLI   5(R2),0             IF NO DATE                                   
         BNE   VK120                                                            
*                                                                               
         MVI   NEWDISP,C'Y'        THEN NEW BATCH - FORCE RE-DISPLAY            
         XC    RQDTE,RQDTE                                                      
*                                                                               
         B     VK200                                                            
*                                                                               
VK120    ZIC   RF,5(R2)               LOOK FOR FINAL +                          
*                                                                               
         LA    RE,FHDRLEN-1(R2,RF)    POINT TO LAST CHAR                        
*                                                                               
         CLI   0(RE),C'+'          IF DATE ENDS IN '+'                          
         BNE   VK130                                                            
*                                                                               
         MVI   0(RE),C' '             ELIMINATE '+' FROM DATE                   
         BCTR  RF,0                   DECREMENT INPUT LENGTH                    
         STC   RF,5(R2)                                                         
         MVI   DTPLUS,C'Y'            LOOK FOR THIS DATE OR LATER               
*                                                                               
VK130    GOTO1 DATVAL,DMCB,(0,FHDRLEN(R2)),WORK VALIDATE LENGTH                 
*                                                                               
         OC    DMCB,DMCB           MUST HAVE A VALID DATE                       
         BZ    VKBADATE                                                         
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(2,DUB)  CONVERT TO COMPRESSED DATE         
*                                                                               
         CLC   RQDTE,DUB           IF DATE WAS CHANGED                          
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'           FORCE SCREEN RE-DISPLAY                   
*                                                                               
         MVC   RQDTE,DUB                                                        
*                                                                               
VK200    OI    4(R2),X'20'         INDICATE DATE IS VALID                       
*                                                                               
         LA    R2,DIVBSQH          SEQUENCE NUMBER FIELD                        
*                                                                               
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK210                                                            
*                                                                               
         MVI   NEWDISP,C'Y'           NEW BATCH-FORCE SCREEN RE-DISPLAY         
         XC    RQSEQ,RQSEQ            INIT SEQUENCE NUMBER                      
         XC    RQBSEQ,RQBSEQ                                                    
*                                                                               
         B     VK300                                                            
*                                                                               
VK210    MVC   WORK(8),=8C'0'      VALIDATE SQN AS NUMERIC                      
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   VKNUMERR                                                         
*                                                                               
         EX    R1,VKPK             PACK SQN                                     
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         CVB   R0,DUB              CVB                                          
         UNPK  FULL,DUB            FILL OUT TO 8 DIGITS                         
*                                                                               
         CLC   RQSEQ,FULL          IF SQN HAS CHANGED                           
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'           FORCE RE-DISPLAY OF SCREEN                
*                                                                               
         MVC   RQSEQ,FULL          SAVE SEQUENCE NUMBER                         
         STCM  R0,3,RQBSEQ                                                      
*                                                                               
         B     VK300                                                            
*                                                                               
VKMVN    MVN   WORK(0),8(R2)       EXECUTED INSTRUCTIONS TO VALIDATE            
VKCLC    CLC   WORK(0),8(R2)         NUMERIC                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*                                                                               
VK300    OI    4(R2),X'20'         INDICATE SQN FIELD IS VALID                  
*                                                                               
         XC    ATOT,ATOT           ZERO ALL TOTALS                              
         XC    SVLSTBAT(SVLSTBTL),SVLSTBAT   ZERO LAST BATCH DATA               
*                                                                               
VKXIT    DS    0H                                                               
*                                                                               
         MVI   CURSYST,C'M'        INDICATE A MEDIA SYSTEM                      
*                                                                               
         GOTO1 VALIFAS             SWITCH TO MEDIA SYSTEM                       
*                                                                               
         MVC   KEY,MEDKEYSV        RETURN MEDIA RECORD KEY                      
*                                                                               
         B     XIT                                                              
*                                                                               
VKNUMERR MVI   ERROR,NOTNUM        NOT NUMERIC                                  
         B     VKERRX                                                           
*                                                                               
VKBADATE MVI   ERROR,INVDATE       INVALID DATE                                 
         B     VKERRX                                                           
*                                                                               
VKERRX   DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - DREC'                           
*                                                                               
***********************************************************************         
*                                                                     *         
*        DREC - DISPLAY RECORD                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DREC     DS    0H                                                               
*                                                                               
         GOTO1 =A(PRPZ),RR=RELO    FIND RECORD, CALL PZMOD, ETC.                
*                                                                               
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - VREC'                           
***********************************************************************         
*                                                                     *         
*        VREC - VALIDATE RECORD                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREC     DS    0H                                                               
*                                                                               
         OC    FUIDNUM,FUIDNUM     HAS ID BEEN SET AS OPTION                    
         BNZ   VREUIDER                                                         
*                                                                               
         OC    SVIHBMOS,SVIHBMOS   WAS MONTH OF SERVICE BLANK                   
         BZ    VREMOSER                                                         
*                                                                               
*        IF NO FIELD HAS BEEN ALTERED, THERE IS NO NEED TO VALIDATE             
*                                                                               
         TM    DIVCCDH+4,X'20'     TEST NEED TO VALIDATE CLIENT                 
         BZ    VR006                                                            
         TM    DIVPCDH+4,X'20'     OR PRODUCT                                   
         BZ    VR006                                                            
         TM    DIVECDH+4,X'20'     OR ESTIMATE                                  
         BZ    VR006                                                            
         TM    DIVCSTH+4,X'20'     OR RECONVERT/CANCEL/DELETE                   
         BZ    VR010                                                            
*                                                                               
         NI    GENSTAT2,X'FF'-RETEQSEL   SET NO RETURN THIS SELECTION           
*                                                                               
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
*                                                                               
         B     XIT                                                              
*                                                                               
*        NO CHANGES ALLOWED FOR CONVERTED INVOICE                               
*        UNLESS IT IS BEING RECONVERTED                                         
*                                                                               
VR006    CLI   CONVRTSW,C'Y'       THIS INVOICE CONVERTED                       
         BNE   VR010                NO, CHANGES ALLOWED                         
*                                                                               
         CLI   RECONVSW,C'Y'       OKAY IF ALREADY SET FOR RE-CONVERST          
         BE    VR010                                                            
*                                                                               
         LA    R2,DIVCSTH          RECONVERT/CANCEL/DELETE FIELD                
*                                                                               
         CLI   5(R2),0             ANY REQUEST ENTERED                          
         BE    VRECVCHG             NO, NO CHANGES ALLOWED                      
*                                                                               
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,VR2CLC           RECONVERT                                    
         BNE   VRECVCHG             NO, NO CHANGES ALLOWED                      
*                                                                               
VR010    DS    0H                                                               
*                                                                               
*        VALIDATE DDS CLIENT CODE                                               
*                                                                               
         CLI   DIVCCDH+5,0         SKIP IF WE HAVE DDS CLIENT CODE              
         BNE   VRCLRX                                                           
*                                                                               
         XC    SVIHCVAD,SVIHCVAD   CLEAR SAVED CONVERTED CLIENT CODE            
*                                                                               
         CLC   SVIHLADC,SPACES     IF THERE IS A LOOKED UP CLIENT               
         BNH   *+24                                                             
         MVC   DIVCCD(3),SVIHLADC     USE IT                                    
         OC    DIVCCD(3),SPACES       FORCE NULLS TO SPACES                     
         MVI   DIVCCDH+5,L'SVIHLADC   SET LENGTH                                
         B     VRCLRX                                                           
*                                                                               
         CLI   DIVPCDH+5,0         SKIP IF WE HAVE A DDS PRODUCT CODE           
         BNE   VRCLRX                                                           
*                                                                               
         XC    SVIHCVPR,SVIHCVPR   CLEAR SAVED CONVERTED PRODUCT CODE           
*                                                                               
         CLC   SVIHLPRC,SPACES     IF THERE IS A LOOKED UP PRODUCT              
         BNH   *+24                                                             
         MVC   DIVPCD(3),SVIHLPRC     USE IT                                    
         OC    DIVPCD(3),SPACES       FORCE NULLS TO SPACES                     
         MVI   DIVPCDH+5,L'SVIHLPRC                                             
         B     VRCLRX                                                           
*                                                                               
         CLI   DIVECDH+5,0         SKIP IF WE HAVE A DDS ESTIMATE CODE          
         BNE   VRCLRX                                                           
*                                                                               
         XC    SVIHCVES,SVIHCVES   CLEAR SAVED CONVERTED ESTIMATE CODE          
*                                                                               
*        ALL FIELDS CLEARED AND NO LOOKED UP DEFAULTS                           
*                                                                               
         OI    DIVCCDH+4,X'20'     SET CLIENT VALIDATED                         
         OI    DIVCCDH+1,X'08'     HIGHLIGHT FIELD                              
         OI    DIVCCDH+6,X'80'     FORCE TRANSMISION                            
*                                                                               
         XC    DIVCNMD,DIVCNMD     CLEAR CLIENT NAME                            
         OI    DIVCNMH+6,X'80'     TRANSMIT CLIENT NAME                         
*                                                                               
         OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
         OI    DIVPCDH+1,X'08'     HIGHLIGHT FIELD                              
         OI    DIVPCDH+6,X'80'     FORCE TRANSMISION                            
*                                                                               
         XC    DIVPNMD,DIVPNMD     CLEAR PRODUCT NAME                           
         OI    DIVPNMDH+6,X'80'    TRANSMIT PRODUCT NAME                        
*                                                                               
         OI    DIVECDH+4,X'20'     SET ESTIMATE VALIDATED                       
         OI    DIVECDH+1,X'08'     HIGHLIGHT FIELD                              
         OI    DIVECDH+6,X'80'     FORCE TRANSMISION                            
*                                                                               
         XC    DIVENMD,DIVENMD     CLEAR ESTIMATE DESCRIPTION                   
         OI    DIVENMDH+6,X'80'    TRANSMIT ESTIMATE DESCRIPTION                
*                                                                               
         B     VRCPEX                                                           
*                                                                               
*        IF ANY FIELD ENTERED, MUST HAVE A CLIENT                               
*                                                                               
VRCLRX   DS    0H                                                               
*                                                                               
         LA    R2,DIVCCDH                                                       
*                                                                               
         XC    DIVCNMD,DIVCNMD     INIT CLIENT NAME                             
         OI    DIVCNMDH+6,X'80'    TRANSMIT NAME                                
*                                                                               
         CLI   DIVCCDH+5,0         NEED CLIENT IF PRD OR EST ENTERED            
         BE    VRECLTNE                                                         
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         NI    DIVCCDH+1,X'FF'-X'0C' TURN OFF HIGHLIGHTING                      
         OI    DIVCCDH+6,X'80'     FORCE TRANSMISION                            
         XC    SVIHCVAD,SVIHCVAD   INIT SAVED CONVERTED CLIENT                  
*                                                                               
         CLC   SVIHLADC,QCLT       IF CLIENT IS NOT LOOKED UP DEFAULT           
         BE    *+14                                                             
         MVC   SVIHCVAD,QCLT          SAVE AS CONVERTED CLIENT CODE             
         OI    DIVCCDH+1,X'08'        HIGHLIGHT FIELD                           
*                                                                               
         OI    DIVCCDH+4,X'20'     SET AS VALIDATED FIELD                       
*                                                                               
         MVC   DIVCNMD(20),CLTNM   DISPLAY CLIENT NAME                          
         OI    DIVCNMDH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
*                                                                               
VRCCDX   DS    0H                                                               
*                                                                               
*        PRODUCT VALIDATION                                                     
*                                                                               
         LA    R2,DIVPCDH                                                       
*                                                                               
         XC    QPRD,QPRD           INIT PRODUCT SAVEAREAS                       
         XC    SVIHCVPR,SVIHCVPR                                                
*                                                                               
         XC    DIVPNMD,DIVPNMD     INIT PRODUCT NAME                            
         OI    DIVPNMDH+6,X'80'    TRANSMIT PRODUCT NAME                        
*                                                                               
         NI    DIVPCDH+1,X'FF'-X'0C'   TURN OFF HIGH INTENSITY                  
         OI    DIVPCDH+6,X'80'     FORCE TRANSMISION                            
*                                                                               
         CLI   5(R2),0             SKIP IF PRODUCT ENTERED                      
         BNE   VRPRD10                                                          
*                                                                               
         CLC   SVIHLPRC,SPACES     IF THERE IS A LOOKED UP PRODUCT              
         BNH   VRPRDX                                                           
*                                                                               
         MVC   DIVPCD(3),SVIHLPRC     USE IT                                    
         MVI   DIVPCDH+5,L'SVIHLPRC                                             
*                                                                               
VRPRD10  DS    0H                                                               
*                                                                               
         CLC   DIVPCD(3),=C'***'   SPECIAL ENTRY INDICATES VARIOUS PRDS         
         BNE   VRPRD15                                                          
*                                                                               
         MVC   DIVPNMD(7),=C'VARIOUS'    SPECIAL NAME                           
*                                                                               
         B     VRPRD20                                                          
*                                                                               
VRPRD15  DS    0H                                                               
*                                                                               
         CLI   5(R2),3             IF MORE THAN 3, ERROR                        
         BH    VREPRDSZ                                                         
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         MVC   DIVPNMD,WORK+4      TRANSMIT PRODUCT NAME                        
         MVC   QPRD,WORK           SAVE PRODUCT CODE                            
*                                                                               
VRPRD20  DS    0H                                                               
*                                                                               
         CLC   SVIHLPRC,DIVPCD     IF NOT EQUAL TO LOOKED UP PRODUCT            
         BE    *+20                                                             
         MVC   SVIHCVPR,DIVPCD        SAVE AS CONVERTED PRODUCT CODE            
         OC    SVIHCVPR,SPACES                                                  
         OI    DIVPCDH+1,X'08'        HIGHLIGHT FIELD                           
*                                                                               
VRPRDX   DS    0H                                                               
*                                                                               
         OI    DIVPCDH+4,X'20'     SET VALIDATED                                
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
*                                                                               
*        VALIDATE ESTIMATE                                                      
*                                                                               
         XC    SVIHCVES,SVIHCVES   INIT ESTIMATE SAVE AREA                      
         XC    DIVENMD,DIVENMD     INIT ESTIMATE DESCRIPTION                    
         OI    DIVENMDH+6,X'80'    TRANSMIT ESTIMATE NAME FIELD                 
*                                                                               
         LA    R2,DIVECDH          POINT TO DDS ESTIMATE CODE                   
*                                                                               
         OI    DIVECDH+1,X'08'     TURN OFF HIGH INTENSITY                      
         OI    DIVECDH+6,X'80'     FORCE TRANSMISION                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INPUT LENGTH                                 
         BZ    VRESTX              OKAY IF NOT ENTERED                          
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         MVC   ZEROS,=C'000'                                                    
*                                                                               
         EX    RF,VRMVN                                                         
         EX    RF,VRCLC            CK IF NUMERIC                                
         BNE   VRENUMER             NO, ERROR                                   
*                                                                               
         B     VR170                                                            
*                                                                               
*        EXECUTED INSTRUCTIONS FOR TESTING NUMERIC                              
*                                                                               
VRMVN    MVN   ZEROS(0),8(R2)                                                   
VRCLC    CLC   8(0,R2),ZEROS                                                    
VRPACK   PACK  DUB,8(0,R2)                                                      
*                                                                               
* VALIDATE ESTIMATE                                                             
*                                                                               
VR170    EX    RF,VRPACK                                                        
         CVB   RF,DUB                                                           
         STCM  RF,3,SVIHCVES       SAVE BINARY ESTIMATE                         
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PESTRECD,R4                                                      
*                                                                               
         MVC   PESTKAGY,QAGY                                                    
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,PESTKIDQ                                                
         MVC   PESTKCLT,SVIHCVAD                                                
         MVC   PESTKPRD,SVIHCVPR                                                
         MVC   PESTKEST,SVIHCVES                                                
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VRESTNF                                                          
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING PESTRECD,R4                                                      
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   SVESTDTS,PESTST     SAVE START AND END DATES                     
*                                                                               
         CLC   SVESTSTR,SVIHDIED   EST STR AFTER MOS END                        
         BH    VRESTDTS                                                         
         CLC   SVESTEND,SVIHDISD   EST END BEFORE MOS START                     
         BL    VRESTDTS                                                         
*                                                                               
         MVC   DIVENMD,PESTNAME    DISPLAY ESTIMATE NAME                        
*                                                                               
VRESTX   DS    0H                                                               
*                                                                               
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         OI    DIVECDH+4,X'20'     SET ESTIMATE VALIDATED                       
*                                                                               
VRCPEX   DS    0H                                                               
*                                                                               
         OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
*                                                                               
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
*                                                                               
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
*                                                                               
*        VALIDATE CONVERTED STATUS                                              
*                                                                               
         TM    DIVCSTH+4,X'20'     CK FOR CANCEL, DELETE, OR RECONVERT          
         BO    VR270               PREVIOUSLY VALIDATED                         
*                                                                               
         LA    R2,DIVCSTH                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    VR266               NOT ENTERED                                  
*                                                                               
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,VR2CLCA          CANCEL                                       
         BE    VR260                                                            
*                                                                               
         EX    RF,VR2CLCB          DELETE                                       
         BNE   VR254                                                            
*                                                                               
         TM    SVIHCVST,EZIHCVQ       TEST CONVERTED                            
         BO    VRECVDEL                                                         
*                                                                               
         MVI   DELETESW,C'Y'          SET INVOICE DELETED                       
         MVI   RECONVSW,C'N'          SET RECONVERT OFF                         
         MVI   CHANGESW,C'Y'          SET INVOICE CHANGED                       
*                                                                               
         B     VR266                                                            
*                                                                               
VR254    EX    RF,VR2CLC           RECONVERT                                    
         BNE   VR256                                                            
*                                                                               
         CLI   CONVRTSW,C'Y'       WAS THIS CONVERTED                           
         BNE   VRCONREC                                                         
*                                                                               
         MVI   RECONVSW,C'Y'                                                    
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         OI    DIVCSTSH+6,X'80'                                                 
         MVC   DIVCSTS,=C'RECONVERT'                                            
*                                                                               
         B     VR258                                                            
*                                                                               
VR256    EX    RF,VR2CLCC          RESTORE                                      
         BNE   VRCONVRT                                                         
*                                                                               
         CLI   DELETESW,C'Y'       WAS THIS DELETED                             
         BNE   VRDELREC                                                         
*                                                                               
         MVI   DELETESW,C'N'                                                    
         MVI   RESTORSW,C'Y'                                                    
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         MVI   KEEPSW,C'N'                                                      
         OI    DIVCSTSH+6,X'80'                                                 
         MVC   DIVCSTS,=C'RESTORED '                                            
*                                                                               
VR258    OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
*                                                                               
         B     VR266                                                            
*                                                                               
VR260    CLI   RECONVSW,C'Y'                                                    
         BNE   VRENOCNV                                                         
*                                                                               
         MVI   RECONVSW,C'N'                                                    
         OI    DIVCSTSH+6,X'80'                                                 
         MVC   DIVCSTS,SPACES                                                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
*                                                                               
VR266    OI    DIVCSTH+4,X'20'                                                  
         B     VR270                                                            
*                                                                               
VR2CLC   CLC   8(0,R2),=C'RECONVERT '                                           
VR2CLCA  CLC   8(0,R2),=C'CANCEL '                                              
VR2CLCB  CLC   8(0,R2),=C'DELETE '                                              
VR2CLCC  CLC   8(0,R2),=C'RESTORE '                                             
*                                                                               
VR270    GOTO1 =A(PRPZ),RR=RELO    FIND RECORD, CALL PZMOD, ETC.                
*                                                                               
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CHGOKMS),CHGOKMS                                       
         LA    R2,DIVCCDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
VRCLTNF  MVI   ERROR,PZECLTNF      CLIENT   NOT FOUND                           
         B     VRERRX                                                           
VREPRDSZ MVI   ERROR,PZEPRDLN      PRODUCT CODE AT MOST 3 LONG                  
         B     VRERRX                                                           
VRESTNF  MVI   ERROR,PZEESTNF      ESTIMATE NOT FOUND                           
         B     VRERRX                                                           
VRCONVRT MVI   ERROR,PZECNVNV      CONVERSION FIELD INVALID                     
         B     VRERRX                                                           
VRCONREC MVI   ERROR,PZECNVNO      INVOICE NOT CONVERTED YET                    
         B     VRERRX                                                           
VRDELREC MVI   ERROR,PZEDELD       INVOICE IS DELETED                           
         B     VRERRX                                                           
VRENOCNV MVI   ERROR,PZENOCNV      NO CANCEL UNLESS RECONVERTING                
         B     VRERRX                                                           
VRECVCHG MVI   ERROR,PZECVTCH      NO CHANGE TO CONVERTED INVOICE               
         LA    R2,DIVCCDH                                                       
         B     VRERRX                                                           
*                                                                               
* CAN'T CHANGE RECS ON OTHER USERS *                                            
*                                                                               
VREUIDER MVI   ERROR,PZEUSRID      USER NOT AUTHORIZED                          
         LA    R2,CONRECH                                                       
         B     VRERRX                                                           
*                                                                               
VREMOSER MVI   ERROR,PZEMOSNV      INVALID MONTH OF SERVICE                     
         LA    R2,CONRECH                                                       
         B     VRERRX                                                           
*                                                                               
VRECVDEL MVI   ERROR,PZECNVDL      CAN'T DELETE CONVERTED INVOICE               
         B     VRERRX                                                           
*                                                                               
VRESTDTS MVI   ERROR,PZEESTDT      ESTIMATE DATES OUTSIDE MOS                   
         B     VRERRX                                                           
*                                                                               
VRECLTNE MVI   ERROR,PZECLTNE      CLIENT REQUIRED                              
         B     VRERRX                                                           
*                                                                               
VRENUMER MVI   ERROR,NOTNUM        NOT NUMERIC                                  
         B     VRERRX                                                           
*                                                                               
VRERRX   DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - DKEY'                           
***********************************************************************         
*                                                                     *         
*        DKEY - DISPLAY KEY                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DKEY     DS    0H                                                               
*                                                                               
*        FILTERS LINE                                                           
*                                                                               
         LA    R2,DIVFTRH          POINT TO FILTERS FIELD                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,SVFLTRSL       GET LENGTH OF FILTERS FIELD                  
         BZ    DKFLTRSX            SKIP IF NO FILTERS                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SVFLTRS     DISPLAY FILTERS FROM LIST SCREEN             
*                                                                               
DKFLTRSX DS    0H                                                               
*                                                                               
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    4(R2),X'20'         INDICATE FILTERS ARE VALID                   
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         MVC   SVLSTBAT(SVLSTBTL),INVLIST FORCES RE-DISPLAY OF LAST             
*                                     LIST SCREEN                               
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
CHGOKMS  DC    C'* IF CHANGES OK, HIT ENTER TO RESUME *'                        
*                                                                               
         DROP  R7,RB,RC                                                         
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - PRPZ'                           
***********************************************************************         
*                                                                     *         
*        PRPZ - FIND RECORD, CALL PZMOD                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
PRPZ     NMOD1 0,**PRPZ**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         OC    VPZMOD,VPZMOD       SKIP IF PZMOD LOADED ALREADY                 
         BNZ   PRPZ1                                                            
*                                                                               
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA    GET PZMOD                          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RF,VPZMOD                                                        
*                                                                               
PRPZ1    DS    0H                                                               
*                                                                               
         MVI   USEIO,C'Y'                                                       
*                                                                               
         XC    CURWKIXD,CURWKIXD                                                
         LA    R4,CURWKIXD                                                      
         USING EZWKRIXD,R4                                                      
*                                                                               
         ZIC   RF,SELLISTN         RELATIVE LINE NUMBER                         
         MH    RF,=Y(INVENTL)                                                   
         LA    R5,INVLIST(RF)                                                   
         USING INVLISTD,R5                                                      
*                                                                               
         MVC   SVINVLST,INVLISTD   SAVE INVOICE BEING DIAPLAYED                 
*                                                                               
         MVC   EZWIUID,INVUID                                                   
*                                                                               
         MVC   SRCEPUB,INVPZPUB    PUB                                          
         MVC   SRCEMED,INVMEDIA    MEDIA                                        
         MVC   EZWIPUB,INVPZPUB    SOURCE PUB CODE                              
         MVC   EZWIMED,INVMEDIA    MEDIA                                        
         MVI   EZWIDAY,X'98'       DAY = 98                                     
         MVC   CURWKIXD+8(2),INVBSEQ                                            
         DROP  R5                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',EPICWK,CURWKIXD,AIO2,WRKFBUFA             
         TM    DMCB+8,X'80'        TEST EOF                                     
         BZ    *+6                                                              
         DC    H'0'                BATCH HAS DISAPPEARED                        
*                                                                               
         GOTO1 DATAMGR,(R1),=C'READ',EPICWK,CURWKIXD,AIO2,WRKFBUFA              
*                                    ADDRESSES STILL LEFT FROM INDEX            
         TM    DMCB+8,X'80'        TEST EOF ON FIRST READ                       
         BZ    *+6                                                              
         DC    H'0'                BATCH HAS BEEN CLOBBERED                     
*                                                                               
         L     R5,WRKFBUFA                                                      
         USING W_RECD,R5                                                        
         MVC   SVWCMNT,W_DESC                                                   
         LA    R5,W_DESC                                                        
         DROP  R5                                                               
*                                                                               
         L     R6,AIO3             SET PZBLOCK                                  
         USING EZBLOCKD,R6                                                      
*                                                                               
         LR    R0,R6               CLEAR PZBLOCK                                
         LH    R1,=Y(EZBLOCKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,=A(DRHOOK)       DISPLAY HOOK ROUTINE                         
*                                                                               
         CLI   MODE,DISPREC                                                     
         BE    *+8                                                              
         L     RE,=A(VRHOOK)       VALIDATE HOOK ROUTINE                        
*                                                                               
         A     RE,RELO                                                          
         ST    RE,EZHOOK                                                        
         MVC   EZAGY,AGENCY                                                     
         MVI   EZOFF,C'N'                                                       
         MVI   KEEPSW,C'Y'                                                      
         MVC   EZWKRFIL,EPICWK                                                  
         MVC   EZWKRIND,EZWKRIXD                                                
         L     R1,WRKFBUFA                                                      
         ST    R1,EZWKRBUF                                                      
         MVC   EZWKRREC,AIO2                                                    
         L     RE,=A(IOA4-(CONHEADH-64))                                        
         LA    RE,0(RE,RA)                                                      
         ST    RE,EZAREC                                                        
         L     RE,ACOMFACS                                                      
         ST    RE,EZCOMFCS                                                      
         MVI   EZLOOKSW,0          INIT LOOK UP SWITCH                          
         MVI   EZTEST,C'Y'                                                      
*                                                                               
         TM    FTRFLAG,FTRTRACE                                                 
         BZ    *+8                                                              
         OI    EZTRACE,X'F0'                                                    
*                                                                               
         MVI   EZWRITE,C'N'                                                     
         MVI   FOUNDSW,C'N'                                                     
         MVI   CLIENTSW,C'N'                                                    
         L     RF,4(,RD)           BACK UP CHAIN 1                              
         L     R7,48(,RF)                                                       
         L     RB,64(,RF)                                                       
         GOTO1 VPZMOD,DMCB,(R6)                                                 
         XIT1                  RET AT BATCH END-DONE ON RET FROM PZMOD          
         DROP  R4,R6,RB,RC                                                      
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - VRHOOK'                         
***********************************************************************         
*                                                                     *         
*        VRHOOK - VALIDATE REC PZMOD HOOK                             *         
*                                                                     *         
***********************************************************************         
         DS    0H                                                               
VRHOOK   NMOD1 0,**#VRHK*                                                       
*                                                                               
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
*                                                                               
         CLI   DELETESW,0          PROBLEM IF SWITCHES NOT SET TO               
         BNE   *+6                   SOMETHING                                  
         DC    H'0'                                                             
*                                                                               
         CLI   RECONVSW,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CONVRTSW,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R0,SVR0                                                          
*                                                                               
         L     R6,AIO3             SET PZBLOCK                                  
         USING EZBLOCKD,R6                                                      
*                                                                               
         MVC   QMED,EZMED                                                       
*                                                                               
*        DETERMINE HOOK MODE                                                    
*                                                                               
         CLI   EZMODE,EZINVP       PROCESS INVOICE HEADER                       
         BE    VRHINVP              NO                                          
*                                                                               
         CLI   EZMODE,EZSPTP       PROCESS INSERTION                            
         BE    VRHSPTP              NO                                          
*                                                                               
         CLI   EZMODE,EZINVL       PROCESS INVOICE LAST                         
         BE    VRHINVL              NO                                          
*                                                                               
         CLI   EZMODE,EZBATL       PROCESS BATCH END                            
         BE    VRHBATL                                                          
*                                                                               
         B     VRHX                UNWANTED HOOK                                
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - VRHINVP'                        
***********************************************************************         
*                                                                     *         
*        VRHINVP - INVOICE HEADER VALIDATION                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRHINVP  DS    0H                                                               
*                                                                               
         ZIC   R5,SELLISTN         GET LINE NUMBER                              
         MH    R5,=Y(INVENTL)                                                   
         LA    R5,INVLIST(R5)                                                   
         USING INVLISTD,R5                                                      
*                                                                               
*        IF THIS INVOICE IS BEING CHANGED, CHECK IT LATER *                     
*                                                                               
         CLC   EZINVSEQ,INVRSEQ      ARE WE AT RIGHT INVOICE                    
         BE    VRH160                                                           
*                                                                               
*        BATCH WITH ALL INVOICES CONVERTED OR DELETED IS                        
*          CONSIDERED 'DONE' OR IN 'KEEP' STATUS. USUALLY IGNORED IN            
*          LOOKUPS.                                                             
*        ANY INVOICE IN BATCH THAT HAS ACTIVITY (SEE BELOW)                     
*           FORCES BATCH TO 'ACTIVE' OR 'UNKEEP' STATUS.                        
*                                                                               
*        MUST CHECK ALL INVOICES IN BATCH TO GET STATUS                         
*                                                                               
VRH100   DS    0H                                                               
*                                                                               
*        INVOICE FORCES BATCH OUT OF KEEP STATUS IF                             
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   INVOICE TO BE RECONVERTED                    
         BO    VRH140               YES                                         
*                                                                               
         TM    EZIHCVST,EZIHCVQ+EZIHCDEL INV NOT CONVERTED NOR DELETED          
         BZ    VRH140                                                           
*                                                                               
         B     VRH145              ELSE OKAY TO KEEP                            
*                                                                               
VRH140   DS    0H                                                               
*                                                                               
         MVI   KEEPSW,C'N'         BATCH TO BE SET TO 'UNKEEP' STATUS           
*                                                                               
VRH145   DS    0H                                                               
*                                                                               
         CLC   EZINVSEQ,INVRSEQ      ARE WE AT RIGHT INVOICE                    
         BL    VRH360                                                           
         BH    VRH400                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
VRH160   DS    0H                  CORRECT INVOICE FOUND                        
*                                                                               
*        BATCH KEEP STATUS CHANGED IF                                           
*                                                                               
         CLI   RESTORSW,C'Y'       THIS IS A RESTORE DELETED REQUEST            
         BNE   *+8                                                              
         MVI   KEEPSW,C'N'                                                      
*                                                                               
         MVI   FOUNDSW,C'Y'        INDICATE INVOICE HEADER FOUND                
*                                                                               
         L     R5,EZWKRREC         INVOICE HEADER RECORD                        
         LA    R5,7(,R5)           SKIP RECLEN(4),RECCODE(2),DELIM(1)           
*                                                                               
*        INIT CLIENT OVERRIDE, RECONVERT AND DELETE STATUS                      
*                                                                               
         NI    EZIHCVST-EZIHCNVS(R5),X'FF'-EZIHCOVR-EZIHRCVQ-EZIHCDEL           
*                                                                               
         OC    SVIHCVAD,SVIHCVAD   IS THERE AN OVERRIDE CLT?                    
         BZ    *+8                  NO                                          
         OI    EZIHCVST-EZIHCNVS(R5),EZIHCOVR   SET OVERRIDE SWITCH             
*                                                                               
         CLI   RECONVSW,C'Y'       IF RECONVERT REQUEST                         
         BNE   VRH170                                                           
*                                                                               
         OI    EZIHCVST-EZIHCNVS(R5),EZIHRCVQ SET STATUS                        
         MVI   KEEPSW,C'N'         BATCH TO 'UNKEEP'                            
*                                                                               
         B     VRH180                                                           
*                                                                               
VRH170   TM    EZIHCVST-EZIHCNVS(R5),EZIHCVQ    CONVERTED?                      
         BO    VRH180                            YES                            
*                                                                               
         CLI   DELETESW,C'Y'       DELETE REQUEST                               
         BE    VRH180               YES                                         
*                                                                               
         MVI   KEEPSW,C'N'                                                      
*                                                                               
VRH180   DS    0H                                                               
*                                                                               
         CLI   DELETESW,C'Y'       DELETE REQUEST                               
         BNE   *+8                                                              
         OI    EZIHCVST-EZIHCNVS(R5),EZIHCDEL                                   
*                                                                               
                                                                                
         CLC   EZIHCVAD-EZIHCNVS(3,R5),SVIHCVAD IF CLIENT CHANGED               
         B     *+8                                                              
         MVI   CLIENTSW,C'Y'       SET SWITCH                                   
*                                                                               
         MVC   EZIHCVAD-EZIHCNVS(3,R5),SVIHCVAD                                 
         MVC   EZIHCVPR-EZIHCNVS(3,R5),SVIHCVPR                                 
         MVC   EZIHCVES-EZIHCNVS(2,R5),SVIHCVES                                 
         MVI   EZIHCVND-EZIHCNVS(R5),X'80' STOP DROP TRAILING BLANKS            
*                                                                               
         CLI   CHANGESW,C'Y'       WAS INVOICE CHANGED                          
         BNE   VRHX                 NO                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'WRITE',EZWKRFIL,EZWKRIND,EZWKRREC,      X        
               EZWKRBUF                                                         
         CLI   DMCB+8,0            TEST ANY ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* IF RESTORE OR RECONVERT, MUST SET OFF BATCH CONVERT/KEEP IF NEEDED *          
*                                                                               
         CLI   RESTORSW,C'Y'       RESTORE DELETED REQUEST                      
         BE    VRH200               YES                                         
         CLI   RECONVSW,C'Y'       RECONVERT REQUEST                            
         BNE   VRHX                 NO                                          
*                                                                               
VRH200   LA    R1,CURWKIXD                                                      
*                                                                               
         TM    UKSTAT-UKRECD(R1),X'08'   THIS FILE ON KEEP                      
         BZ    VRHX                       NO                                    
*                                                                               
         MVC   EZWKRIND,CURWKIXD                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'UNKEEP',EZWKRFIL,EZWKRIND,EZWKRREC,     X        
               EZWKRBUF                                                         
         CLI   DMCB+8,0            TEST ANY ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     VRHX                                                             
*                                                                               
VRH360   MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
         B     VRHX                                                             
*                                                                               
VRH400   CLI   FOUNDSW,C'Y'                                                     
         BE    VRHX                                                             
         DC    H'0'                                                             
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - VRHSPTP'                        
***********************************************************************         
*                                                                     *         
*        VRHSPTP - INSERTION DETAIL PROCESSING                        *         
*        IF CLIENT HAS CHANGED AND DETAIL HAS PRODUCT OR ESTIMATE     *         
*            OVERRIDES, THEY MUST BE CLEARED                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRHSPTP  DS    0H                                                               
*                                                                               
         CLC   EZSPCVPR,SPACES     SKIP IF PRODUCT NOT OVERRIDDEN               
         BH    *+14                                                             
         OC    EZSPCVES,EZSPCVES   AND ESTIMATE NOT OVERRIDDEN                  
         BZ    VRHSPTPX                                                         
*                                                                               
         CLI   CLIENTSW,0          SKIP IF CLIENT NOT CHANGED                   
         BE    VRHSPTPX                                                         
*                                                                               
         L     R5,EZWKRREC         INSERTON DETAIL RECORD                       
         LA    R5,7(,R5)           SKIP RECLEN(4),RECCODE(2),DELIM(1)           
         USING EZSPFRST,R5         ESTABLISH INSERTION DETAILS                  
*                                                                               
*        CLEAR PRODUCT AND ESTIMATE OVERRIDES                                   
*                                                                               
         XC    EZSPCVPR,EZSPCVPR  CLEAR PRODUCT OVERRIDE                        
         XC    EZSPCVES,EZSPCVES  CLEAR ESTIMATE OVERRRIDE                      
*                                                                               
         DROP  R5                                                               
*                                                                               
*        WRITE CHANGES TO FILE                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'WRITE',EZWKRFIL,EZWKRIND,EZWKRREC,      X        
               EZWKRBUF                                                         
         CLI   DMCB+8,0            TEST ANY ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRHSPTPX DS    0H                                                               
         B     VRHX                                                             
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - VRHINVL'                        
***********************************************************************         
*                                                                     *         
*        VRHINVL - LAST  FOR INVOICE PROCESSING                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRHINVL  DS    0H                                                               
*                                                                               
         B     VRHX                                                             
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - VRHBATL'                        
***********************************************************************         
*                                                                     *         
*        VRHBATL - END OF BATCH - TEST IF KEEP STATUS SHOULD CHANGE   *         
*                                                                     *         
*        AT BATCH END, SET OR RESET BOTH CONVERTED AND KEEP STATUS    *         
*                                                                     *         
*        IF RESTORE, OR RECONVERT FOR BATCH IN KEEP STATUS, UNKEEP    *         
*                                                                     *         
*        IF DELETE, AND ALL OTHER INVOICES IN BATCH CONVERTED OR      *         
*           DELETED, SET BATCH CONVERT BIT ON AND STATUS TO KEEP      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRHBATL  CLI   KEEPSW,C'Y'         IF ALL INVOICES CONVERTED OR                 
         BE    VRH550               DELETED, SHOULD BE STATUS KEEP              
*                                                                               
VRH520   LA    R1,CURWKIXD                                                      
*                                                                               
         TM    UKSTAT-UKRECD(R1),X'08'   THIS FILE ON KEEP                      
         BZ    VRHX                       NO                                    
*                                                                               
         MVC   EZWKRIND,CURWKIXD                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'UNKEEP',EZWKRFIL,EZWKRIND,EZWKRREC,     X        
               EZWKRBUF                                                         
         CLI   DMCB+8,0            TEST ANY ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     VRHX                                                             
*                                                                               
* ENTIRE BATCH DELETED OR CONVERTED, MUST SET TO CONVERTED & KEEP *             
*                                                                               
VRH550   LA    R1,CURWKIXD                                                      
*                                                                               
         TM    UKSTAT-UKRECD(R1),X'08'   THIS FILE ON KEEP                      
         BO    VRHX                       YES                                   
*                                                                               
         MVC   EZWKRIND,CURWKIXD                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'KEEP',EZWKRFIL,EZWKRIND,EZWKRREC,       X        
               EZWKRBUF                                                         
         CLI   DMCB+8,0            TEST ANY ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRHX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - DRHOOK'                         
***********************************************************************         
*                                                                     *         
*        DRHOOK - DISPLAY REC PZMOD HOOK                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
DRHOOK   NMOD1 0,**#DRHK*                                                       
*                                                                               
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
*                                                                               
         ST    R0,SVR0                                                          
*                                                                               
         L     R6,AIO3             SET PZBLOCK                                  
         USING EZBLOCKD,R6                                                      
*                                                                               
         MVC   QMED,EZMED                                                       
*                                                                               
         CLI   EZMODE,EZSPTP       INVOICE DETAIL                               
         BE    DRHSPTP                                                          
*                                                                               
         CLI   EZMODE,EZINVL       END OF INVOICE                               
         BE    DRHINVL                                                          
*                                                                               
         CLI   EZMODE,EZINVP       PROCESS INVOICE HEADER                       
         BE    DRHINVP                                                          
*                                                                               
         B     DRHX                UNUSED PZMOD HOOK                            
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - DRHSPTP'                        
***********************************************************************         
*                                                                     *         
*        DRHOOK - PROCESS INVOICE DETAIL                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRHSPTP  DS    0H                                                               
*                                                                               
         CLC   EZSPCVPR,SPACES     IF PRODUCT NOT OVERRIDDEN                    
         BH    *+10                                                             
         CLC   EZSPLPRC,SPACES     AND NONE LOOKED UP                           
         BH    *+8                                                              
         MVI   PRDSW,C'M'             SET FLAG                                  
*                                                                               
         B     DRHX                                                             
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - DRHINVP'                        
***********************************************************************         
*                                                                     *         
*        DRHOOK - PROCESS INVOICE HEADER                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRHINVP  DS    0H                                                               
*                                                                               
         MVI   PRDSW,0             INIT PRODUCT MISSING SWITCH                  
*                                                                               
         ZIC   RF,SELLISTN         GET LINE NUMBER                              
         MH    RF,=Y(INVENTL)                                                   
         LA    R5,INVLIST(RF)                                                   
         USING INVLISTD,R5                                                      
*                                                                               
         CLC   EZINVSEQ,INVRSEQ      ARE WE AT RIGHT INVOICE                    
         BL    DRH360                                                           
         BE    DRH010                                                           
*                                                                               
         CLI   FOUNDSW,C'Y'        WAS INVOICE FOUND                            
         BE    DRH300               YES                                         
         DC    H'0'                                                             
*                                                                               
DRH010   DS    0H                                                               
*                                                                               
         MVC   SVINVSEQ,EZRECSEQ     SAVE INVOICE HEADER SEQ                    
         XC    RECORDSW,RECORDSW     BUG CATCHER                                
         MVI   FOUNDSW,C'Y'                                                     
*                                                                               
         B     DRHX                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - DRHINVL'                        
***********************************************************************         
*                                                                     *         
*        DRHOOK - LAST FOR INVOICE                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRHINVL  DS    0H                                                               
         ZIC   RF,SELLISTN         GET LINE NUMBER                              
         MH    RF,=Y(INVENTL)                                                   
         LA    R5,INVLIST(RF)                                                   
         USING INVLISTD,R5                                                      
*                                                                               
         MVC   SVIHADVN,EZIHAVNM   25 CHAR ADVERTISER NAME                      
         MVC   SVIHAVCD,EZIHAVCD   8  CHAR ADVERTISER CODE                      
         MVC   SVIHCNVS,EZIHCNVS   SAVE CONVERSION STATUS FIELD                 
         MVC   SVSNMED,EZSNMED     STATION MEDIA                                
         XC    QPRD,QPRD           INIT PRODUCT SAVEAREA                        
*                                                                               
         MVC   SVIHDISD,EZIHISDT   INVOICE START DATE                           
         MVC   SVIHDIED,EZIHIEDT   INVOICE END   DATE                           
*                                                                               
         MVC   SRCEPUB,INVPZPUB    PUB                                          
*                                                                               
*        DISPLAY PUB ID AND NAME                                                
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(C'0',INVPUB),(0,DIVPUB) PUB CODE                  
*                                                                               
         LA    R2,DIVPUBH          POINT TO PUB FIELD                           
*                                                                               
         LA    R0,L'DIVPUB         MAX FIELD LENGTH                             
         LA    R1,8-1+L'DIVPUB(R2) LAST BYTE OF FIELD                           
*                                                                               
         CLI   0(R1),C' '          FIND END OF FIELD                            
         BH    *+10                                                             
         BCTR  R1,0                BACK UP A BYTE                               
         BCT   R0,*-10                                                          
*                                                                               
         STC   R0,5(R2)            SET FIELD LENGTH                             
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         GOTO1 VALIPUB             VALIDATE PUB ENTRY                           
*                                                                               
         OI    DIVPUBH+4,X'20'     FLAG AS VALIDATED FIELD                      
*                                                                               
         XC    DIVPUBN,DIVPUBN     PUT OUT NEW PUBNAME                          
         MVC   DIVPUBN(L'PUBPNM),PUBPNM                                         
         OI    DIVPUBNH+6,X'80'    FORCE FIELD TRANSMISSION                     
         XC    DIVZONE,DIVZONE     PUT OUT NEW ZONE NAME                        
         MVC   DIVZONE(L'PUBPZNM),PUBPZNM                                       
         OI    DIVZONEH+6,X'80'    FORCE FIELD TRANSMISSION                     
*                                                                               
*        DISPLAY BATCH DATE                                                     
*                                                                               
         XC    WORK(L'DIVBDT),WORK          BATCH DATE                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVBDTE),(5,WORK)                                 
*                                                                               
         CLC   DIVBDT,WORK                                                      
         BE    *+14                                                             
         MVC   DIVBDT,WORK                                                      
         OI    DIVBDTH+6,X'80'                                                  
*                                                                               
         XC    ELEM(L'DIVBSQ),ELEM          BATCH SEQ                           
         SR    R0,R0                                                            
         ICM   R0,3,INVBSEQ                                                     
         EDIT  (R0),(5,ELEM),ALIGN=LEFT                                         
*                                                                               
         CLC   DIVBSQ,ELEM                                                      
         BE    *+14                                                             
         MVC   DIVBSQ,ELEM                                                      
         OI    DIVBSQH+6,X'80'                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         CLC   DIVCDE,EZIHAVCD     SOURCE CLIENT CODE                           
         BE    *+14                                                             
         MVC   DIVCDE,EZIHAVCD                                                  
         OI    DIVCDEH+6,X'80'                                                  
*                                                                               
         CLC   DIVCNM,EZIHAVNM     SOURCE CLIENT NAME                           
         BE    *+14                                                             
         MVC   DIVCNM,EZIHAVNM                                                  
         OI    DIVCNMH+6,X'80'                                                  
*                                                                               
*        DISPLAY DDS CLIENT CODE AND NAME IF PRESENT                            
*                                                                               
         XC    DIVCCD,DIVCCD       INIT SCREEN CLIENT CODE AND NAME             
         XC    DIVCNMD,DIVCNMD                                                  
*                                                                               
         MVC   SVIHCVAD,EZIHCVAD   SAVE CONVERTED CLIENT CODE                   
*                                                                               
         XC    SVIHLADC,SVIHLADC   INIT LOOKED UP CLIENT CODE SAVEAREA          
*                                                                               
         CLC   EZIHCVAD,EZIHLADC   IF LOOKED UP CLIENT DIFFERENT                
         BE    *+10                                                             
         MVC   SVIHLADC,EZIHLADC      SAVE LOOKED UP CLIENT CODE                
*                                                                               
         CLC   EZIHCVAD,SPACES     IF THERE IS A CONVERTED CLIENT CODE          
         BNH   *+28                                                             
         MVC   DIVCCD(3),EZIHCVAD     DISPLAY IT                                
         OC    DIVCCD(3),SPACES       FORCE NULLS TO SPACES                     
         MVI   DIVCCDH+5,L'SVIHLADC   SET LENGTH                                
         OI    DIVCCDH+1,X'08'        FORCE HIGH   INTENSITY                    
         B     DRHCCD1                                                          
*                                                                               
         CLC   EZIHLADC,SPACES     IF THERE IS A LOOKED UP CLIENT CODE          
         BNH   *+28                                                             
         MVC   DIVCCD(3),EZIHLADC     DISPLAY IT                                
         OC    DIVCCD(3),SPACES       FORCE NULLS TO SPACES                     
         MVI   DIVCCDH+5,L'SVIHLADC   SET LENGTH                                
         NI    DIVCCDH+1,X'FF'-X'0C'  FORCE NORMAL INTENSITY                    
         B     DRHCCD1                                                          
*                                                                               
         OI    DIVCCDH+1,X'08'     FORCE HIGH   INTENSITY                       
         B     DRHCCDX             NO DDS CLIENT CODE                           
*                                                                               
DRHCCD1  DS    0H                                                               
*                                                                               
*        READ CLIENT HEADER FOR NAME                                            
*                                                                               
         OC    FUIDNUM,FUIDNUM     HAS ID BEEN SET AS OPTION                    
         BNZ   DRHCCDX                                                          
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCLTRECD,R4                                                      
*                                                                               
         MVC   PCLTKAGY,QAGY                                                    
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,PCLTKIDQ                                                
         MVC   PCLTKCLT,DIVCCD                                                  
         OC    PCLTKCLT,SPACES                                                  
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   DRHCCDX                                                          
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   DIVCNMD,PCLTNAME    DISPLAY CLIENT NAME                          
         MVC   CLTNM,PCLTNAME      SAVE CLIENT NAME                             
*                                                                               
DRHCCDX  OI    DIVCNMDH+6,X'80'    TRANSMIT                                     
         OI    DIVCCDH+4,X'20'     SET CLIENT VALIDATED                         
         OI    DIVCCDH+6,X'80'     TRANSMIT                                     
         MVI   DIVCCDH+5,3         SET CLIENT CODE LENGTH                       
*                                                                               
*        DISPLAY SOURCE PRODUCT CODE AND NAME                                   
*                                                                               
         MVC   DIVPPE,EZIHPRCD     DISPLAY SOURCE PRODUCT CODE                  
         OI    DIVPPEH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         MVC   DIVPNM,EZIHPRNM     DISPLAY SOURCE PRODUCT NAME                  
         OI    DIVPNMH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY DDS PRODUCT CODE AND NAME                                      
*                                                                               
         XC    DIVPCD,DIVPCD       INIT DDS PRODUCT CODE                        
         XC    DIVPNMD,DIVPNMD     INIT DDS PRODUCT NAME                        
*                                                                               
         MVC   SVIHCVPR,EZIHCVPR   SAVE CONVERTED PRODUCT CODE                  
*                                                                               
         XC    SVIHLPRC,SVIHLPRC   INIT LOOKED UP PROD CODE SAVEAREA            
*                                                                               
         CLC   EZIHCVPR,EZIHLPRC   IF LOOKED UP PROD DIFFERENT                  
         BE    *+10                                                             
         MVC   SVIHLPRC,EZIHLPRC      SAVE LOOKED UP PROD CODE                  
*                                                                               
         OC    FUIDNUM,FUIDNUM     IS USER = OPTION                             
         BNZ   DRHPCDX              YES                                         
*                                                                               
         CLC   EZIHCVPR,SPACES     USE CONVERTED PRODUCT IF PRESENT             
         BNH   *+24                                                             
         MVC   DIVPCD(3),EZIHCVPR                                               
         OC    DIVPCD(3),SPACES       FORCE NULLS TO SPACES                     
         OI    DIVPCDH+1,X'08'        FORCE HIGH INTENSITY                      
         B     DRHPCD1                                                          
*                                                                               
         CLC   EZIHLPRC,SPACES     USE LOOKED UP PRODUCT IF PRESENT             
         BNH   *+24                                                             
         MVC   DIVPCD(3),EZIHLPRC                                               
         OC    DIVPCD(3),SPACES       FORCE NULLS TO SPACES                     
         NI    DIVPCDH+1,X'FF'-X'0C'        FORCE NORMAL INTENSITY              
         B     DRHPCD1                                                          
*                                                                               
         OI    DIVPCDH+1,X'08'        FORCE HIGH INTENSITY                      
         B     DRHPCDX                                                          
*                                                                               
DRHPCD1  DS    0H                                                               
*                                                                               
         CLC   =C'***',DIVPCD      IF VARIOUS PRODUCTS                          
         BNE   DRHPCD1A                                                         
         CLI   PRDSW,C'M'          AND A DETAIL IS MISSING A PRODUCT            
         BNE   *+10                                                             
         MVC   DIVPCD,SPACES          SHOW AS NO PRODUCT                        
*                                                                               
DRHPCD1A DS    0H                                                               
*                                                                               
         MVC   QPRD,DIVPCD         SAVE DDS PRODUCT CODE                        
*                                                                               
         CLC   QPRD,=C'***'        IF PRODUCT IS VARIOUS                        
         BNE   DRHPCD2                                                          
         MVC   DIVPNMD(7),=C'VARIOUS'    SET SPECIAL NAME                       
         B     DRHPCDX                 SKIP READING FILE RECORD                 
*                                                                               
DRHPCD2  DS    0H                                                               
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PPRDRECD,R4                                                      
*                                                                               
         MVC   PPRDKAGY,QAGY                                                    
         MVC   PPRDKMED,QMED                                                    
         MVI   PPRDKRCD,PPRDKIDQ                                                
         MVC   PPRDKCLT,DIVCCD                                                  
         MVC   PPRDKPRD,DIVPCD                                                  
         OC    PPRDKPRD,SPACES                                                  
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   DRHPCDX                                                          
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   DIVPNMD,PPRDNAME    DISPLAY DDS PRODUCT NAME                     
*                                                                               
DRHPCDX  DS    0H                                                               
*                                                                               
         OI    DIVPCDH+6,X'80'     TRANSMIT PRODUCT CODE FIELD                  
         OI    DIVPCDH+4,X'20'     INDICATE FIELD IS VALID                      
         OI    DIVPNMDH+6,X'80'    TRANSMIT PRODUCT NAME FIELD                  
*                                                                               
*        DISPLAY SOURCE ESTIMATE                                                
*                                                                               
         MVC   DIVEST,EZIHEST      DISPLAY SOURCE ESTIMATE                      
         OI    DIVESTH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY DDS ESTIMATE AND NAME IF PRESENT                               
*                                                                               
         XC    DIVECD,DIVECD       INIT DDS ESTIMATE CODE                       
         XC    DIVENMD,DIVENMD     INIT DDS ESTIMATE NAME                       
*                                                                               
         MVC   SVIHCVES,EZIHCVES   SAVE CONVERTED ESTIMATE CODE                 
*                                                                               
         OC    EZIHCVES,EZIHCVES   IF NO ESTIMATE GIVEN                         
         BNZ   *+12                                                             
         OI    DIVECDH+1,X'08'        FORCE HIGH INTENSITY                      
         B     DRHECDX                SKIP                                      
*                                                                               
         EDIT  (B2,EZIHCVES),(3,DIVECD),ALIGN=LEFT                              
*                                                                               
         OC    FUIDNUM,FUIDNUM     HAS ID BEEN SET AS OPTION                    
         BNZ   DRHECDX                                                          
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PESTRECD,R4                                                      
*                                                                               
         MVC   PESTKAGY,QAGY                                                    
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,PESTKIDQ                                                
         MVC   PESTKCLT,DIVCCD                                                  
         MVC   PESTKPRD,DIVPCD                                                  
         MVC   PESTKEST,EZIHCVES                                                
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   DRHECDX                                                          
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING PESTRECD,R4                                                      
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         OI    DIVECDH+1,X'08'     FORCE HIGH INTENSITY                         
*                                                                               
         MVC   DIVENMD,PESTNAME    DISPLAY ESTIMATE NAME                        
*                                                                               
DRHECDX  DS    0H                                                               
*                                                                               
         OI    DIVECDH+6,X'80'     DISPLAY DDS ESTIMATE CODE                    
         OI    DIVECDH+4,X'20'     SET DDS ESTIMATE CODE VALIDATED              
         OI    DIVENMDH+6,X'80'    DISPLAY DDS ESTIMATE NAME                    
*                                                                               
         CLC   DIVINO(L'EZIHINV),EZIHINV                                        
         BE    *+14                                                             
         MVC   DIVINO(L'EZIHINV),EZIHINV                                        
         OI    DIVINOH+6,X'80'                                                  
*                                                                               
         CLC   EZIHINST,=CL25'BILLING INVOICE REVERSAL'                         
         BE    *+10                                                             
         MVC   EZIHINST,SPACES                                                  
*                                                                               
         CLC   DIVREV(L'EZIHINST),EZIHINST                                      
         BE    *+14                                                             
         MVC   DIVREV(L'EZIHINST),EZIHINST                                      
         OI    DIVREVH+6,X'80'                                                  
*                                                                               
         MVC   DIVMOS(3),EZIHDMOS                                               
         MVC   DIVMOS+3(2),EZIHDMOS+4                                           
         OI    DIVMOSH+6,X'80'                                                  
*                                                                               
         XC    DIVMOSE,DIVMOSE                                                  
         OI    DIVMOSEH+6,X'80'                                                 
         OC    EZIHBMOS,EZIHBMOS IS MONTH OF SERVICE BLANK                      
         BNZ   *+10                                                             
         MVC   DIVMOSE,=C'*ERROR*'                                              
*                                                                               
         EDIT  (B4,EZIHTSPN),(5,DIVSPTS),COMMAS=YES,ZERO=NOBLANK,      C        
               ALIGN=LEFT                                                       
         OI    DIVSPTSH+6,X'80'                                                 
*                                                                               
         EDIT  (B4,EZITBDUE),(13,DIVORDR),2,COMMAS=YES,ZERO=NOBLANK,   C        
               MINUS=YES,ALIGN=LEFT                                             
         OI    DIVORDRH+6,X'80'                                                 
*                                                                               
         EDIT  (B4,EZITBACT),(13,DIVGRSS),2,COMMAS=YES,ZERO=NOBLANK,   C        
               MINUS=YES,ALIGN=LEFT                                             
         OI    DIVGRSSH+6,X'80'                                                 
*                                                                               
         MVC   SVITBACT,EZITBACT                                                
         MVC   SVITBDUE,EZITBDUE                                                
         MVC   SVIHBMOS,EZIHBMOS SAVE MONTH OF SERVICE                          
*                                                                               
         CLC   DIVSRCE,SVWCSRCE                                                 
         BE    DRH070                                                           
         MVC   DIVSRCE,SVWCSRCE                                                 
         OI    DIVSRCEH+6,X'80'                                                 
*                                                                               
DRH070   MVC   DIVCST,SPACES       CLEAR STATUS                                 
         MVC   DIVACTD,SPACES      AND   DATE FIELD                             
         MVI   DELETESW,C'N'                                                    
*                                                                               
         MVC   SVIHCVST,EZIHCVST SAVE STATUS                                    
*                                                                               
         TM    EZIHCVST,EZIHCDEL TEST DELETED                                   
         BZ    DRH080                                                           
         MVC   DIVCST(7),=C'DELETED'                                            
         MVI   DELETESW,C'Y'                                                    
         B     DRH090                                                           
*                                                                               
DRH080   TM    EZIHCVST,EZIHCVQ TEST CONVERTED                                  
         BNZ   DRH100                                                           
         MVC   DIVCST(2),=C'NO'                                                 
*                                                                               
DRH090   OC    EZIHCVDT,EZIHCVDT BETTER BE CONSISTENT                           
         BZ    *+6                                                              
         DC    H'0'                OOPS! UNCONVERTED MUST HAVE NO DATE          
*                                                                               
         MVI   CONVRTSW,C'N'       SET NOT CONVERTED                            
         B     DRH130                                                           
*                                                                               
DRH100   MVI   CONVRTSW,C'Y'       SET WAS CONVERTED                            
         MVC   DIVCST(3),=C'YES'                                                
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(8,DIVACTD)                             
*                                                                               
         OC    EZIHCVDT,EZIHCVDT BETTER BE CONSISTENT                           
         BNZ   DRH130                                                           
         DC    H'0'                OOPS! CONVERTED MUST HAVE DATE               
*                                                                               
DRH130   OI    DIVCSTH+4,X'20'     VALIDATED                                    
         OI    DIVCSTH+6,X'80'                                                  
         OI    DIVACTDH+6,X'80'                                                 
*                                                                               
DRH250   DS    0H                                                               
*                                                                               
         XC    DIVCSTS,DIVCSTS                                                  
         OI    DIVCSTSH+6,X'80'                                                 
         OI    DIVCSTSH+4,X'20'                                                 
*                                                                               
         MVI   RECONVSW,C'N'                                                    
         TM    EZIHCVST,EZIHRCVQ TEST RECONVERT                                 
         BZ    DRH260                                                           
         MVC   DIVCSTS,=C'RECONVERT'                                            
         MVI   RECONVSW,C'Y'                                                    
*                                                                               
DRH260   CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DRH300                                                           
         MVC   DIVCD(3),=C'IH='                                                 
         EDIT  (B2,SVINVSEQ),(4,DIVCD+3),COMMAS=YES,ALIGN=LEFT                  
         MVC   DIVCD+15(3),=C'IT='                                              
         EDIT  (B2,EZRECSEQ),(3,DIVCD+18),COMMAS=YES,ALIGN=LEFT                 
         OI    DIVCDH+6,X'80'                                                   
*                                                                               
DRH300   L     RD,SVR0             DON'T GO BACK TO PZMOD                       
         B     DRHX                                                             
*                                                                               
DRH360   MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
*                                                                               
DRHX     XIT1                                                                   
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
NUMLINS  EQU   14                                                               
*                                                                               
         TITLE 'T43007 - INVOICE HEADER MAINT - WORKING STORAGE'                
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
* PPEZFWORKD                                                                    
       ++INCLUDE PPEZFWORKD                                                     
* PPEZFCNVWD                                                                    
       ++INCLUDE PPEZFCNVWD                                                     
SYSD     DSECT                                                                  
         ORG   SYSAPPLW                                                         
CLIENTSW DS    CL1                 C'Y' CHANGE IN CLIENT OVERRIDE               
PRDSW    DS    CL1                 C'M' A DETAIL IS MISSING A PRODUCT           
         EJECT                                                                  
       ++INCLUDE PPGENPZ                                                        
* PZBLOCK                                                                       
         PRINT OFF                                                              
       ++INCLUDE PZBLOCK                                                        
         PRINT ON                                                               
* PPEZFFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPEZFFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
* PPEZFF8D                                                                      
       ++INCLUDE PPEZFF8D                                                       
         ORG   CONHEADH-64+X'2000'                                              
HELPSAVE DS    XL512                                                            
IOA4     DS    XL4096                                                           
*                                                                               
         EJECT                                                                  
* PZGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PZGLOBEQUS                                                     
         PRINT ON                                                               
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
* DMWRKRD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFD                                                        
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFK                                                        
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PRGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
* PRHELPCB                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRHELPCB                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096PPEZF07   09/23/97'                                      
         END                                                                    
