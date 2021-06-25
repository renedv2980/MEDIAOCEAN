*          DATA SET PRSFM33    AT LEVEL 207 AS OF 05/11/16                      
*PHASE T41C33A                                                                  
*INCLUDE SRCHCALL                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE NUMED                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE PPBROWSE                                                               
*        TITLE 'T41C33 INSERTION ORDER SETUP RECORDS'                           
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS'                           
***********************************************************************         
*                                                                     *         
*         CHANGE LOG                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
* SMYE  03/11    ADD P TO Y/N OPTIONS FOR EIO/ESR BY SINGLE ESTIMATE            
*                                                                               
* SMYE  07/10    LIMIT NUMBER OF ACTIVITY RECORDS (SEE ACTLMT)                  
*                                                                               
* SMYE  02/07    ADD FIELDS FOR EIO/ESR BY SINGLE ESTIMATE                      
*                                                                               
* SMYE  07/05    ADD ESR FIELDS AND PASSIVE KEYS FOR ESR/EIO                    
*                                                                               
* BOBY  05/05    REVISION FOR GENERAL RELEASE                                   
*                                                                               
* SMYE  06/04    BIG BANG                                                       
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - INIT'                    
***********************************************************************         
*                                                                     *         
*        INITIALISATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41C33   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C33,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         L     R7,=A(SUBROUTS)     SET UP ADDRESSABILITY TO SUBROUTINES         
         A     R7,RELO             RELOCATE ADDRESS                             
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         USING GETTXTD,GETTXTCB    ESTABLISH GETTXT CONTROL BLOCK               
*                                                                               
         MVI   IPSTAT,0            INIT INPUT STATISTICS                        
         MVI   SAVMSGNO,0          INIT MESSAGE NUMBER SAVEAREA                 
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENT                          
         OI    GENSTAT4,NODELLST   NO DELETE FROM LIST                          
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - ANAL'                    
***********************************************************************         
*                                                                     *         
*        ANALIZE CALLING MODE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ANAL     DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,XRECADD        PASSIVE KEY WORK                             
         BNE   *+12                                                             
         BRAS  RE,XR                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,XRECPUT        PASSIVE KEY WORK                             
         BNE   *+12                                                             
         BRAS  RE,XR                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   *+12                                                             
         BRAS  RE,PR                                                            
         B     ANALX                                                            
*                                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   *+12                                                             
         BRAS  RE,RDEL                                                          
         B     ANALX                                                            
*                                                                               
         CLI   MODE,RECREST        RESTORE RECORD                               
         BNE   *+12                                                             
         BRAS  RE,RR                                                            
         B     ANALX                                                            
*                                                                               
         B     ANALX                                                            
*                                                                               
ANALSFLX DS    0H                                                               
*                                                                               
ANALX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VK'                      
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY                                                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC                                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      DIFFERENT IF LIST                            
         BE    VKL                                                              
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BE    VKL                                                              
*                                                                               
         XC    HDRSV,HDRSV         INIT HEADER SAVEAREA                         
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VKMED'                   
***********************************************************************         
*                                                                     *         
*        VALIDATE MED                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKMED    DS    0H                                                               
*                                                                               
         MVI   USEIONUM,2          VALIDATE USING AIO2                          
         XC    HDRSV,HDRSV         INIT HEADER SAVEAREA                         
*                                                                               
         MVC   SCRMEDN,SPACES      INIT                                         
         MVC   MEDNM,SPACES        INIT                                         
         OI    SCRMEDNH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         LA    R2,SCRMEDH         VALIDATE MEDIA                                
         GOTOR VALIMED                                                          
*                                                                               
         MVI   ERROR,0             RESET ERROR CODE                             
*                                                                               
         MVC   SCRMEDN(L'MEDNM),MEDNM  DISPLAY MEDIA NAME                       
*                                                                               
*        READ CURRENT MEDIA LEVEL RECORD                                        
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY                                                           
         USING SCHRECD,R4          ESTABLISH INSERTION ORDER SETUP KEY          
*                                                                               
         MVC   SCHKAGY,AGENCY      AGENCY                                       
         MVC   SCHKMED,QMED        MEDIA                                        
         MVI   SCHKRCD,SCHKRCDQ    RECORD TYPE                                  
*                                                                               
         GOTOR HIGH                READ FOR MEDIA RECORD                        
*                                                                               
         CLC   SCHKKEY,KEYSAVE     SKIP IF RECORD NOT FOUND                     
         BNE   VKMEDX                                                           
*                                                                               
         MVC   AIO,AIO1            USE IOA1 FOR RECORD                          
*                                                                               
         GOTOR GETREC              READ IN MEDIA RECORD                         
*                                                                               
         MVI   ELCODE,SCHHDRQ      FIND HEADER ELEMENT                          
         L     R6,AIO              POINT TO RECORD                              
         BRAS  RE,GETEL            FIND HEADER ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
         USING SCHHDRD,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         SR    RF,RF                                                            
         IC    RF,SCHHDRLN         GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   HDRSV(0),SCHHDR     SAVE MEDIA HEADER ELEMENT                    
*                                                                               
VKMEDX   DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VKCLT'                   
***********************************************************************         
*                                                                     *         
*        VALIDATE CLIENT                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKCLT    DS    0H                                                               
*                                                                               
         LA    R2,SCRCLTH          CLIENT                                       
         MVC   CLTNM,SPACES                                                     
         XC    QCLT,QCLT           INIT TO ALL CLIENTS                          
*                                                                               
         CLI   5(R2),0             NO ENTRY EQUIVALENT TO ALL CLIENTS           
         BE    *+10                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VKCLT20                                                          
*                                                                               
         GOTOR VALICLT             VALIDATE CLIENT ENTRY                        
*                                                                               
         MVI   ERROR,0             RESET ERROR CODE                             
*                                                                               
         OC    HDRSV,HDRSV         MUST HAVE MEDIA ENTRY                        
         BZ    VKCLTMDE                                                         
*                                                                               
VKCLT20  DS    0H                                                               
*                                                                               
         MVC   SCRCLTN(L'CLTNM),CLTNM  DISPLAY CLT NAME                         
         OI    SCRCLTNH+6,X'80'    RE-DISPLAY THE FIELD                         
*                                                                               
VKCLTX   DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VKKEY'                   
***********************************************************************         
*                                                                     *         
*        BUILD STARTING KEY                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKKEY    DS    0H                                                               
*                                                                               
         MVI   USEIONUM,1          USE IOAREA1 FOR VALIDATION                   
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING SCHRECD,R4          ESTABLISH INSERTION ORDER SETUP KEY          
*                                                                               
         MVC   SCHKAGY,AGENCY      AGENCY                                       
         MVC   SCHKMED,QMED        MEDIA                                        
         MVI   SCHKRCD,SCHKRCDQ    RECORD TYPE                                  
         MVC   SCHKCLT,QCLT        CLIENT                                       
*                                                                               
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
*                                                                               
VKKEYX   DS    0H                                                               
*                                                                               
         B     VKX                                                              
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VKL'                     
***********************************************************************         
*                                                                     *         
*        VALIDATE LIST KEY                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKL      DS    0H                                                               
*                                                                               
         MVI   USEIONUM,2          USE IOAREA 2 FOR VALIDATION                  
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VKLMED'                  
***********************************************************************         
*                                                                     *         
*        VALIDATE MEDIA                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLMED   DS    0H                                                               
*                                                                               
         MVC   SCLMEDN,SPACES      INIT                                         
         MVC   MEDNM,SPACES                                                     
         OI    SCLMEDNH+6,X'80'    FORCE RE-TRANSMISSION                        
*                                                                               
         LA    R2,SCLMEDH          MEDIA                                        
*                                                                               
         GOTOR VALIMED                                                          
*                                                                               
         MVI   ERROR,0             RESET ERROR CODE                             
*                                                                               
         MVC   SCLMEDN(L'MEDNM),MEDNM   DISPLAY MEDIA NAME                      
*                                                                               
VKLMEDX  DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VKLCLT'                  
***********************************************************************         
*                                                                     *         
*        VALIDATE CLIENT                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLCLT   DS    0H                                                               
*                                                                               
         XC    QCLT,QCLT           DEFAULT TO ANY CLIENT                        
*                                                                               
         MVC   SCLCLTN,SPACES      INIT CLIENT NAME                             
         OI    SCLCLTNH+6,X'80'    FORCE RE-TRANSMISSION OF NAME                
*                                                                               
         LA    R2,SCLCLTH          CLIENT                                       
*                                                                               
         CLI   5(R2),0             NO ENTRY EQUIVALENT TO ANY CLIENT            
         BE    VKLCLTX                ALL DONE                                  
*                                                                               
         CLC   =C'ALL',8(R2)       'ALL' EQUIVALENT TO ANY CLIENT               
         BE    VKLCLTX                                                          
*                                                                               
         GOTOR VALICLT             VALIDATE CLIENT                              
*                                                                               
         MVI   ERROR,0             RESET ERROR CODE                             
*                                                                               
         MVC   SCLCLTN,CLTNM       DISPLAY CLIENT NAME                          
*                                                                               
VKLCLTX  DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VKLEIO'                  
***********************************************************************         
*                                                                     *         
*        VALIDATE EIO FILTER                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLEIO   DS    0H                                                               
*                                                                               
         CLI   SCLEIO,C'Y'         MUST BE Y                                    
         BE    *+8                                                              
         CLI   SCLEIO,C'N'         N                                            
         BE    *+8                                                              
         CLI   SCLEIO,C' '         SPACE                                        
         BE    *+8                                                              
         CLI   SCLEIO,0            NULLS                                        
         BE    *+8                                                              
         BNE   VKLEIOER                                                         
*                                                                               
VKLEIOX  DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VKLESR'                  
***********************************************************************         
*                                                                     *         
*        VALIDATE ESR FILTER                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLESR   DS    0H                                                               
*                                                                               
         CLI   SCLESR,C'Y'         MUST BE Y                                    
         BE    *+8                                                              
         CLI   SCLESR,C'N'         N                                            
         BE    *+8                                                              
         CLI   SCLESR,C' '         SPACE                                        
         BE    *+8                                                              
         CLI   SCLESR,0            NULLS                                        
         BE    *+8                                                              
         BNE   VKLESRER                                                         
*                                                                               
VKLESRX  DS    0H                                                               
*                                                                               
VKLX     DS    0H                                                               
*                                                                               
VKX      DS    0H                                                               
*                                                                               
         MVI   USEIONUM,1          USE IOAREA 1 FOR VALIDATION                  
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
         XIT1                                                                   
*                                                                               
VKLEIOER DS    0H                  INVALID EIO FILTER                           
*                                                                               
VKLESRER DS    0H                  INVALID ESR FILTER                           
         LHI   RF,INVALID                                                       
         B     VKERR                                                            
*                                                                               
VKCLTMDE DS    0H                  MEDIA LEVEL RECORD MISSING                   
         LHI   RF,PWEEIOMD                                                      
         B     VKERR                                                            
*                                                                               
VKERR    DS    0H                                                               
*                                                                               
         STCM  RF,3,ERROR2CD       SET ERROR EQUATE                             
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         BE    *+14                                                             
         MVC   ERROR2CD+1(1),ERROR      PUT IN NEW STYLE                        
         MVI   ERROR2CD,0                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVC   GTMSGNO,ERROR2CD    MESSAGE NUMBER                               
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DK'                      
***********************************************************************         
*                                                                     *         
*        DISPLAY KEY                                                  *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         LA    R4,KEY         ESTABLISH INSERTION ORDER SETUP KEY               
         USING SCHREC,R4                                                        
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DKMED'                   
***********************************************************************         
*                                                                     *         
*        DISPLAY MEDIA                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DKMED    DS    0H                                                               
*                                                                               
         XC    SCRMED,SCRMED                                                    
         MVC   SCRMED(L'SCHKMED),SCHKMED   MEDIA                                
         MVI   SCRMEDH+5,L'SCHKMED SET INPUT LENGTH                             
         OI    SCRMEDH+6,FOUTTRN   TRANSMIT                                     
*                                                                               
DKMEDX   DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DKCLT'                   
***********************************************************************         
*                                                                     *         
*        DISPLAY CLIENT                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DKCLT    DS    0H                                                               
*                                                                               
         XC    SCRCLT,SCRCLT                                                    
         MVC   SCRCLT(L'SCHKCLT),SCHKCLT      CLIENT                            
         MVI   SCRCLTH+5,L'SCHKCLT SET INPUT LENGTH                             
*                                                                               
         CLI   SCRCLT+2,C' '       IF 2 CHARACTER CLIENT                        
         BH    *+8                                                              
         MVI   SCRCLTH+5,2            ADJUST INPUT LENGTH                       
*                                                                               
         OI    SCRCLTH+6,FOUTTRN   TRANSMIT                                     
*                                                                               
         OC    SCHKCLT,SCHKCLT     IF ALL CLIENTS                               
         BNZ   DKCLTX                                                           
*                                                                               
         MVC   CLTNM,SPACES                                                     
*                                                                               
         MVC   SCRCLT,SPACES                                                    
         MVI   SCRCLTH+5,0         NO INPUT                                     
*                                                                               
DKCLTX   DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DKX'                     
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY AND READ IN RECORD                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
         BRAS  RE,VK               VALIDATE KEY                                 
*                                                                               
         GOTOR HIGH                READ IN RECORD (KEY IN 'KEY')                
*                                                                               
         GOTOR GETREC                                                           
*                                                                               
DKX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VR'                      
***********************************************************************         
*                                                                     *         
*        VALIDATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         XC    SVACHGS,SVACHGS     INIT CHANGE INDICATORS                       
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VRFIRST'                 
***********************************************************************         
*                                                                     *         
*        VALIDATE REQUIRED ENTRY PRESENT                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRFIRST  DS    0H                                                               
*                                                                               
         LA    R2,SCREIOH          POINT TO EIO OPTION FIELD                    
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         CLI   FLDILEN,0           ANY ENTRY ?                                  
         BNE   VRHDR               YES - OK TO EDIT ALL FIELDS                  
*                                                                               
         CLI   SCRESRH+5,0         ENTRY IN ESR OPTION FIELD ?                  
         BNE   VRHDR               YES - OK TO EDIT ALL FIELDS                  
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+8                                                              
         B     VREIOMSE            ERROR - BOTH FIELDS MISSING                  
*                                                                               
         CLI   SCREIOEH+5,0        ENTRY IN EIO BY EST OPTION FIELD ?           
         BNE   VRHDR               YES - OK TO EDIT ALL FIELDS                  
*                                                                               
         CLI   SCRESREH+5,0        ENTRY IN ESR BY EST OPTION FIELD ?           
         BE    VRALLMSE            NO - ERROR - ALL OPT FIELDS MISSING          
*                                                                               
VRFIRSTX DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VRHDR'                   
***********************************************************************         
*                                                                     *         
*        VALIDATE HEADER ELEMENT                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRHDR    DS    0H                                                               
*                                                                               
         XC    SVHDRELM,SVHDRELM   INIT WORK AREA                               
         LA    R6,SVHDRELM                                                      
         USING SCHHDRD,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVI   SCHHDRCD,SCHHDRQ    SET ELEMENT CODE                             
         MVI   SCHHDRLN,SCHHDRLQ   SET BASIC ELEMENT LENGTH                     
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VREIO'                   
***********************************************************************         
*                                                                     *         
*        VALIDATE EIO OPTION                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREIO    DS    0H                                                               
*                                                                               
         LA    R2,SCREIOH          POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         CLI   FLDILEN,0           ENTRY ?                                      
         BNE   VREIOYN             YES                                          
*                                  NO - CHECK DATE FIELD                        
         CLI   SCRACTD,C' '        ANYTHING THERE ?                             
         BNH   VREIOX              NO - OK                                      
         B     VREIODT             ERROR - HAS DATE, MUST HAVE OPTION           
*                                                                               
VREIOYN  DS    0H                                                               
         CLI   FLDDATA,C'N'        OKAY IF 'N' FOR NO                           
         BE    *+8                                                              
         CLI   FLDDATA,C'Y'        OKAY IF 'Y' FOR YES                          
         BNE   VREIONV             ELSE ERROR                                   
*                                                                               
         MVC   SCHEIO,FLDDATA      SAVE OPTION                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE RE-TRANSMISSION OF OPTION              
*                                                                               
         OC    QCLT,QCLT           SPECIFIC CLIENT ?                            
         BZ    VREIOX              NO                                           
*                                                                               
         CLI   HDRSV+SCHEIO-SCHHDR,0     ENTRY IN MEDIA RECORD ?                
         BNH   VREIOENF            NO - MEDIA RECORD MUST BE UPDATED            
*                                                                               
VREIOX   DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VRDATE'                  
***********************************************************************         
*                                                                     *         
*        VALIDATE EIO ACTIVATION DATE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRDATE   DS    0H                                                               
*                                                                               
         LA    R2,SCRACTDH         VALIDATE DATE ENTRY                          
*                                                                               
         CLI   5(R2),0             ANY ENTRY ?                                  
         BNE   VRDTVAL             YES                                          
*                                  NO                                           
         CLI   SCREIOH+5,0         ENTRY IN EIO OPTION ?                        
         BNE   VRACTMSE            YES - DATE REQUIRED                          
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP BELOW ON ADD                            
         BE    VRDATEX                                                          
*                                                                               
         CLI   SVEIODT,0           WAS DATE IN RECORD ?                         
         BNH   VRDATEX             NO - OK                                      
         B     VRACTNOX            ERROR - CANNOT ERASE DATE                    
*                                                                               
VRDTVAL  DS    0H                                                               
*                                                                               
         XC    WORK,WORK           INIT PERVAL AREA                             
*                                  VALIDATE FOR A SINGLE DATE                   
         GOTOR PERVAL,DMCB,(SCRACTDH+5,SCRACTD),(X'40',WORK)                    
*                                                                               
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BE    VRACTNVE            YES                                          
*                                                                               
         LA    R5,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R5          ESTABLISH PRVAL OUTPUT                       
*                                                                               
         MVC   SCHACTVD,PVALBSTA   SAVE BINARY START DATE                       
*                                                                               
         MVC   SCRACTD,PVALCPER    DISPLAY COMPLETE DATE                        
         OI    SCRACTDH+6,X'80'    FORCE RE-TRANMISSION OF FIELD                
         MVI   SCRACTDH+5,8        SET LENGTH OF DATE                           
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP BELOW ON ADD                            
         BE    VRDTCK1                                                          
*                                                                               
         CLC   SCHACTVD,SVEIODT    DATE CHANGED ?                               
         BNE   VRDTCK1             YES - TEST FOR OK "VALUE"                    
*                                                                               
         CLC   SCHEIO,SVEIO        OPTION CHANGED ?                             
         BE    VRDATEX             NO - DONE                                    
*                                  YES - TEST FOR OK "VALUE"                    
VRDTCK1  DS    0H                                                               
*                                                                               
         CLC   SCHACTVD,BTODAY     ACTIVATION MUST BE PRESENT OR FUTURE         
         BL    VRACTERE                                                         
*                                                                               
         OC    QCLT,QCLT           SPECIFIC CLIENT ?                            
         BZ    VRDATEX             NO - MEDIA RECORD (ALL CLIENTS)              
*                                                                               
         CLC   SCHACTVD,SCHACTVD-SCHHDR+HDRSV NOT BEFORE MEDIA DATE             
         BL    VRACTMDE                                                         
*                                                                               
VRDATEX  DS    0H                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VRESR'                   
***********************************************************************         
*                                                                     *         
*        VALIDATE ESR OPTION                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRESR    DS    0H                                                               
*                                                                               
         LA    R2,SCRESRH          POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         CLI   FLDILEN,0           ENTRY ?                                      
         BNE   VRESRYN             YES                                          
*                                  NO - CHECK DATE FIELD                        
         CLI   SCRSRDT,C' '        ANYTHING THERE ?                             
         BNH   VRESRX              NO - OK                                      
         B     VREIODT             ERROR - HAS DATE, MUST HAVE OPTION           
*                                                                               
VRESRYN  DS    0H                                                               
         CLI   FLDDATA,C'N'        OKAY IF 'N' FOR NO                           
         BE    *+8                                                              
         CLI   FLDDATA,C'Y'        OKAY IF 'Y' FOR YES                          
         BNE   VREIONV             ELSE ERROR                                   
*                                                                               
         MVC   SCHESR,FLDDATA      SAVE OPTION                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE RE-TRANSMISSION OF OPTION              
*                                                                               
         OC    QCLT,QCLT           SPECIFIC CLIENT ?                            
         BZ    VRESRX              NO                                           
*                                                                               
         CLI   HDRSV+SCHESR-SCHHDR,0     ENTRY IN MEDIA RECORD ?                
         BNH   VREIOENF            NO - MEDIA RECORD MUST BE UPDATED            
*                                                                               
VRESRX   DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VRSRDT'                  
***********************************************************************         
*                                                                     *         
*        VALIDATE ESR ACTIVATION DATE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRSRDT   DS    0H                                                               
*                                                                               
         LA    R2,SCRSRDTH         VALIDATE DATE ENTRY                          
*                                                                               
         CLI   5(R2),0             ANY ENTRY ?                                  
         BNE   VRSDTVAL            YES                                          
*                                                                               
         CLI   SCRESRH+5,0         ENTRY IN ESR OPTION ?                        
         BNE   VRACTMSE            YES - DATE REQUIRED                          
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP BELOW IF ADD (NO DATE ENTERED)          
         BE    VRSRDTX                                                          
*                                                                               
         CLI   SVESRDT,0           WAS DATE IN RECORD ?                         
         BNH   VRSRDTX             NO - OK                                      
         B     VRACTNOX            ERROR - CANNOT ERASE DATE                    
*                                                                               
VRSDTVAL DS    0H                                                               
*                                                                               
         XC    WORK,WORK           INIT PERVAL AREA                             
*                                  VALIDATE FOR A SINGLE DATE                   
         GOTOR PERVAL,DMCB,(SCRSRDTH+5,SCRSRDT),(X'40',WORK)                    
*                                                                               
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BE    VRACTNVE            YES                                          
*                                                                               
         LA    R5,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R5          ESTABLISH PRVAL OUTPUT                       
*                                                                               
         MVC   SCHSRACT,PVALBSTA   SAVE BINARY START DATE                       
*                                                                               
         MVC   SCRSRDT,PVALCPER    DISPLAY COMPLETE DATE                        
         OI    SCRSRDTH+6,X'80'    FORCE RE-TRANMISSION OF FIELD                
         MVI   SCRSRDTH+5,8        SET LENGTH OF DATE                           
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP BELOW IF ADD                            
         BE    VRDTCK2               SINCE DATE NOT "CHANGED"                   
*                                                                               
         CLC   SCHSRACT,SVESRDT    DATE CHANGED ?                               
         BNE   VRDTCK2             YES - TEST FOR OK "VALUE"                    
*                                                                               
         CLC   SCHESR,SVESR        OPTION CHANGED ?                             
         BE    VRSRDTX             NO - DONE                                    
*                                  YES - TEST FOR OK "VALUE"                    
VRDTCK2  DS    0H                                                               
*                                                                               
         CLC   SCHSRACT,BTODAY     ACTIVATION MUST BE PRESENT OR FUTURE         
         BL    VRACTERE                                                         
*                                                                               
         OC    QCLT,QCLT           SPECIFIC CLIENT ?                            
         BZ    VRSRDTX             NO - MEDIA RECORD (ALL CLIENTS)              
*                                                                               
         CLC   SCHSRACT,SCHSRACT-SCHHDR+HDRSV NOT BEFORE MEDIA DATE             
         BL    VRACTMDE                                                         
*                                                                               
VRSRDTX  DS    0H                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VREIOE'                  
***********************************************************************         
*                                                                     *         
*        VALIDATE EIO BY ESTIMATE OPTION                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREIOE   DS    0H                                                               
*                                                                               
         LA    R2,SCREIOEH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         CLI   FLDILEN,0           ENTRY ?                                      
         BNE   VREIOEYN            YES                                          
*                                  NO - CHECK DATE FIELD                        
         CLI   SCREACD,C' '        ANYTHING THERE ?                             
         BNH   VREIOEX             NO - OK                                      
         B     VREIODTE            ERROR - HAS DATE, MUST HAVE OPTION           
*                                                                               
VREIOEYN DS    0H                                                               
         CLI   FLDDATA,C'N'        OKAY IF 'N' FOR NO                           
         BE    *+8                                                              
         CLI   FLDDATA,C'Y'        OKAY IF 'Y' FOR YES                          
         BE    *+8                                                              
         CLI   FLDDATA,C'P'        OKAY IF 'P' FOR PERIOD                       
         BE    *+8                                                              
         BNE   VREIONVE            ELSE ERROR                                   
*                                                                               
         CLI   SCHEIO,C' '         ANYTHING IN USE EIO OPT ?                    
         BNH   VREIOEXO            NO - CANNOT ENTER EIO BY EST                 
*                                                                               
         MVC   SCHEIOE,FLDDATA     SAVE EIO BY EST OPTION                       
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE RE-TRANSMISSION OF OPTION              
*                                                                               
*                                                                               
         OC    QCLT,QCLT           SPECIFIC CLIENT ?                            
         BZ    VREIOEX             NO                                           
*                                                                               
         CLI   HDRSV+SCHEIOE-SCHHDR,0    ENTRY IN MEDIA RECORD ?                
         BNH   VREIOENF            NO - MEDIA RECORD MUST BE UPDATED            
*                                                                               
VREIOEX  DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VRDATEE'                 
***********************************************************************         
*                                                                     *         
*        VALIDATE EIO BY ESTIMATE ACTIVATION DATE                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRDATEE  DS    0H                                                               
*                                                                               
         LA    R2,SCREACDH         VALIDATE DATE ENTRY                          
*                                                                               
         CLI   5(R2),0             ANY ENTRY ?                                  
         BNE   VRDTVALE            YES                                          
*                                  NO                                           
         CLI   SCREIOEH+5,0        ENTRY IN EIO OPTION ?                        
         BNE   VRACTMSE            YES - DATE REQUIRED                          
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP BELOW ON ADD                            
         BE    VRDATEEX                                                         
*                                                                               
         CLI   SVEIODTE,0          WAS DATE IN RECORD ?                         
         BNH   VRDATEEX            NO - OK                                      
         B     VRACTNOX            ERROR - CANNOT ERASE DATE                    
*                                                                               
VRDTVALE DS    0H                  MMM/YY INPUT ONLY                            
*                                                                               
         XC    WORK,WORK           INIT DATVAL AREA                             
*                                  VALIDATE FOR A SINGLE DATE (MMM/YY)          
         GOTOR DATVAL,DMCB,(2,SCREACD),(0,WORK)                                 
*                                                                               
         OC    DMCB(4),DMCB        ANY ERRORS?                                  
         BZ    VREDTNVE            YES                                          
*                                                                               
*           CONVERT RETURNED YYMMDD DATE AND SAVE BINARY DATE IN RECORD         
         GOTOR DATCON,DMCB,(0,WORK),(3,SCHEACD)                                 
*           CONVERT BINARY DATE IN RECORD AND RE-DISPLAY                        
         XC    SCREACD,SCREACD     CLEAR                                        
         GOTOR DATCON,DMCB,(3,SCHEACD),(9,SCREACD)                              
*                                                                               
         OI    SCREACDH+6,X'80'    FORCE RE-TRANMISSION OF FIELD                
         MVI   SCREACDH+5,6        SET LENGTH OF DATE                           
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP BELOW ON ADD                            
         BE    VRDTCK1E                                                         
*                                                                               
         CLC   SCHEACD,SVEIODTE    DATE CHANGED ?                               
         BNE   VRDTCK1E            YES - TEST FOR OK "VALUE"                    
*                                                                               
         CLC   SCHEIOE,SVEIOE      OPTION CHANGED ?                             
         BE    VRDATEEX            NO - DONE                                    
*                                  YES - TEST FOR OK "VALUE"                    
VRDTCK1E DS    0H                                                               
*                                                                               
*????*   CLC   SCHEACD,BTODAY      ACTIVATION MUST BE PRESENT OR FUTURE         
*????*   BL    VRACTERE                                                         
*                                                                               
         OC    QCLT,QCLT           SPECIFIC CLIENT ?                            
         BZ    VRDATEEX            NO - MEDIA RECORD (ALL CLIENTS)              
*                                                                               
         CLC   SCHEACD,SCHEACD-SCHHDR+HDRSV   NOT BEFORE MEDIA DATE             
         BL    VRACTMDE                                                         
*                                                                               
VRDATEEX DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VRESRE'                  
***********************************************************************         
*                                                                     *         
*        VALIDATE ESR BY ESTIMATE OPTION                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRESRE   DS    0H                                                               
*                                                                               
         LA    R2,SCRESREH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         CLI   FLDILEN,0           ENTRY ?                                      
         BNE   VRESRYNE            YES                                          
*                                  NO - CHECK DATE FIELD                        
         CLI   SCRESRD,C' '        ANYTHING THERE ?                             
         BNH   VRESREX             NO - OK                                      
         B     VREIODTE            ERROR - HAS DATE, MUST HAVE OPTION           
*                                                                               
VRESRYNE  DS    0H                                                              
         CLI   FLDDATA,C'N'        OKAY IF 'N' FOR NO                           
         BE    *+8                                                              
         CLI   FLDDATA,C'Y'        OKAY IF 'Y' FOR YES                          
         BE    *+8                                                              
         CLI   FLDDATA,C'P'        OKAY IF 'P' FOR PERIOD                       
         BE    *+8                                                              
         BNE   VREIONVE            ELSE ERROR                                   
*                                                                               
         CLI   SCHESR,C' '         ANYTHING IN USE ESR OPT ?                    
         BNH   VRESREXO            NO - CANNOT ENTER ESR BY EST                 
*                                                                               
         MVC   SCHESRE,FLDDATA      SAVE OPTION                                 
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE RE-TRANSMISSION OF OPTION              
*                                                                               
         OC    QCLT,QCLT           SPECIFIC CLIENT ?                            
         BZ    VRESREX             NO                                           
*                                                                               
         CLI   HDRSV+SCHESRE-SCHHDR,0     ENTRY IN MEDIA RECORD ?               
         BNH   VREIOENF            NO - MEDIA RECORD MUST BE UPDATED            
*                                                                               
VRESREX  DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VRSRDTE'                 
***********************************************************************         
*                                                                     *         
*        VALIDATE ESR BY ESTIMATE ACTIVATION DATE                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRSRDTE  DS    0H                                                               
*                                                                               
         LA    R2,SCRESRDH         VALIDATE DATE ENTRY                          
*                                                                               
         CLI   5(R2),0             ANY ENTRY ?                                  
         BNE   VRSDVALE            YES                                          
*                                                                               
         CLI   SCRESREH+5,0        ENTRY IN ESR BY EST OPTION ?                 
         BNE   VRACTMSE            YES - DATE REQUIRED                          
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP BELOW IF ADD (NO DATE ENTERED)          
         BE    VRSRDTEX                                                         
*                                                                               
         CLI   SVESRDTE,0          WAS DATE IN RECORD ?                         
         BNH   VRSRDTEX            NO - OK                                      
         B     VRACTNOX            ERROR - CANNOT ERASE DATE                    
*                                                                               
VRSDVALE DS    0H                  MMM/YY INPUT                                 
*                                                                               
         XC    WORK,WORK           INIT DATVAL AREA                             
*                                  VALIDATE FOR A SINGLE DATE (MMM/YY)          
         GOTOR DATVAL,DMCB,(2,SCRESRD),(0,WORK)                                 
*                                                                               
         OC    DMCB(4),DMCB        ANY ERRORS?                                  
         BZ    VREDTNVE            YES                                          
*                                                                               
*           CONVERT RETURNED YYMMDD DATE AND SAVE BINARY DATE IN RECORD         
         GOTOR DATCON,DMCB,(0,WORK),(3,SCHESRD)                                 
*           CONVERT BINARY DATE IN RECORD AND RE-DISPLAY                        
         XC    SCRESRD,SCRESRD     CLEAR                                        
         GOTOR DATCON,DMCB,(3,SCHESRD),(9,SCRESRD)                              
*                                                                               
         OI    SCRESRDH+6,X'80'    FORCE RE-TRANMISSION OF FIELD                
         MVI   SCRESRDH+5,6        SET LENGTH OF DATE                           
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP BELOW IF ADD                            
         BE    VRDTCK2E              SINCE DATE NOT "CHANGED"                   
*                                                                               
         CLC   SCHESRD,SVESRDTE    DATE CHANGED ?                               
         BNE   VRDTCK2E            YES - TEST FOR OK "VALUE"                    
*                                                                               
         CLC   SCHESRE,SVESRE      OPTION CHANGED ?                             
         BE    VRSRDTEX            NO - DONE                                    
*                                  YES - TEST FOR OK "VALUE"                    
VRDTCK2E DS    0H                                                               
*                                                                               
*????*   CLC   SCHESRD,BTODAY      ACTIVATION MUST BE PRESENT OR FUTURE         
*????*   BL    VRACTERE                                                         
*                                                                               
         OC    QCLT,QCLT           SPECIFIC CLIENT ?                            
         BZ    VRSRDTEX            NO - MEDIA RECORD (ALL CLIENTS)              
*                                                                               
         CLC   SCHESRD,SCHESRD-SCHHDR+HDRSV     NOT BEFORE MEDIA DATE           
         BL    VRACTMDE                                                         
*                                                                               
VRSRDTEX DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VRUPADD'                 
***********************************************************************         
*                                                                     *         
*        ADD NEW RECORD TO THE FILE                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRUPADD  DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF NOT ACTION ADD                       
         BNE   VRUPADDN                                                         
*                                                                               
*        ADD HEADER ELEMENT TO RECORD TO MAKE SURE SOMETHING IS THERE           
*                                                                               
         LA    R6,SVHDRELM         POINT TO SAVED HEADER ELEMENT                
         USING SCHHDRD,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         L     R4,AIO1             GET RECORD ADDRESS                           
         ST    R4,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R3,SCHFIRST-SCHREC(R4)    POINT TO FIRST ELEMENT IN REC          
         GOTOR VRECUP,DMCB,,(R6),(C'R',(R3)),0  ADD ELEMENT                     
*                                                                               
*        SET ACTIVITY INDICATOR                                                 
*                                                                               
         XC    SVACHGS,SVACHGS     CLEAR CHANGE INDICATORS                      
         OI    SVACH1,SCHAADD      SET RECORD ADDED                             
*                                                                               
VRUPADDX DS    0H                                                               
*                                                                               
         B     VRUPACT             GO ADD ACTIVITY ELEMENT                      
*                                                                               
VRUPADDN DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - VRUPCHG'                 
***********************************************************************         
*                                                                     *         
*        CHANGE RECORD                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRUPCHG  DS    0H                                                               
*                                                                               
*****    CLI   ACTNUM,ACTCHA       SKIP IF NOT ACTION CHANGE                    
*****    BNE   VRUPCHGN                                                         
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
*                                                                               
*                                  CHECK FOR CHANGES FOR ACTIVITY ELEM          
*                                                                               
         MVI   ELCODE,SCHHDRQ      LOOK FOR HEADER ELM                          
         BRAS  RE,GETEL            FIND IN RECORD                               
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
*        R6 ==> RECORD HEADER ELEMENT                                           
*                                                                               
         XC    SVACHGS,SVACHGS     CLEAR CHANGE INDICATORS                      
*                                                                               
         CLC   0(SCHHDRLQ,R6),SVHDRELM                                          
         BE    VRUPCHGX            NO CHANGES                                   
*                                                                               
*        CHECK IF EIO AND ESR STATUSES AND DATES HAVE CHANGED                   
*                                                                               
         CLC   SCHACTVD,SCHACTVD-SCHHDR+SVHDRELM                                
         BE    *+8                                                              
         OI    SVACH1,SCHAACTQ     EIO ACTIVATION DATE CHANGED                  
*                                                                               
         CLC   SCHEIO,SCHEIO-SCHHDR+SVHDRELM                                    
         BE    *+8                                                              
         OI    SVACH1,SCHAEIOQ     EIO STATUS CHANGED                           
*                                                                               
         CLC   SCHSRACT,SCHSRACT-SCHHDR+SVHDRELM                                
         BE    *+8                                                              
         OI    SVACH2,SCHASRDQ     ESR ACTIVATION DATE CHANGED                  
*                                                                               
         CLC   SCHESR,SCHESR-SCHHDR+SVHDRELM                                    
         BE    *+8                                                              
         OI    SVACH2,SCHAESRQ     ESR STATUS CHANGED                           
*                                                                               
*                                                                               
*    CHECK IF EIO AND ESR STATUSES AND DATES BY ESTIMATE HAVE CHANGED           
*                                                                               
         CLC   SCHEACD,SCHEACD-SCHHDR+SVHDRELM                                  
         BE    *+8                                                              
         OI    SVACH2,SCHAEIEQ     EIO BY EST ACTIVATION DATE CHANGED           
*                                                                               
         CLC   SCHEIOE,SCHEIOE-SCHHDR+SVHDRELM                                  
         BE    *+8                                                              
         OI    SVACH2,SCHAEISQ     EIO BY EST STATUS CHANGED                    
*                                                                               
         CLC   SCHESRD,SCHESRD-SCHHDR+SVHDRELM                                  
         BE    *+8                                                              
         OI    SVACH2,SCHASREQ     ESR BY EST ACTIVATION DATE CHANGED           
*                                                                               
         CLC   SCHESRE,SCHESRE-SCHHDR+SVHDRELM                                  
         BE    *+8                                                              
         OI    SVACH2,SCHASRSQ     ESR BY EST STATUS CHANGED                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        SEE IF EMAIL NOTIFICATION NEEDED                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         CLC   =C'SJ',KEY          TEST AGENCY ?                                
         BE    VRUPEIOX            YES - DO NOT SEND NOTIFICATION               
*                                                                               
         SR    R5,R5               USE AS CHANGE INDICATOR                      
*                                                                               
         CLC   SCHEIO,SCHEIO-SCHHDR+SVHDRELM      EIO STATUS CHANGED ?          
         BE    VRUPESR             NO - CHECK FOR ESR CHANGE                    
*                                                                               
         CLI   SCHEIO-SCHHDR+SVHDRELM,C'Y'  OKAY IF TURNING ON                  
         BE    VRUPESR             CHECK FOR ESR CHANGE                         
*                                                                               
*        TURNING OFF EIO                                                        
*                                                                               
         CLC   SCHACTVD,BTODAY     OKAY IF OLD ACTIVATION IN FUTURE             
         BH    VRUPESR             CHECK FOR ESR CHANGE                         
         LA    R5,1(R5)            SEND EMAIL NOTE OF THIS CHANGE               
*                                                                               
VRUPESR  DS    0H                                                               
*                                                                               
         CLC   SCHESR,SCHESR-SCHHDR+SVHDRELM      ESR STATUS CHANGED ?          
         BE    VREMLOUT            NO                                           
*                                                                               
         CLI   SCHESR-SCHHDR+SVHDRELM,C'Y'  OKAY IF TURNING ON                  
         BE    VREMLOUT                                                         
*                                                                               
*        TURNING OFF ESR                                                        
*                                                                               
         CLC   SCHSRACT,BTODAY     OKAY IF OLD ACTIVATION IN FUTURE             
         BH    VREMLOUT                                                         
         LA    R5,2(R5)            SEND EMAIL NOTE OF THIS CHANGE               
*                                                                               
*        ALERT PRODUCT TEAM VIA E-MAIL IF NECESSARY                             
*                                                                               
VREMLOUT DS    0H                                                               
*                                                                               
         LTR   R5,R5               ANYTHING TO REPORT ?                         
         BZ    VRUPEIOK            NO                                           
*                                                                               
*        FORMAT SUBJECT LINE                                                    
*                                                                               
         MVC   EMLSUBJ,=CL70'MQ - CHANGE IN EIO AND/OR ESR STATUS'              
*                                                                               
*        FORMAT E-MAIL MESSAGE                                                  
*                                                                               
         XC    EMLLIN1,EMLLIN1                                                  
         CHI   R5,3                BOTH EIO AND ESR CHANGED ?                   
         BNE   VREMLEIO            NO                                           
         MVC   EMLLIN1(17),=C'BOTH EIO AND ESR '                                
         MVC   EMLLIN1+17(28),=C'ACCESS BEING TURNED OFF FOR:'                  
         B     VREMLEND                                                         
*                                                                               
VREMLEIO DS    0H                                                               
         MVC   EMLLIN1+4(28),=C'ACCESS BEING TURNED OFF FOR:'                   
         CHI   R5,1                EIO CHANGED ?                                
         BNE   VREMLESR            NO - MUST BE ESR                             
         MVC   EMLLIN1(4),=C'EIO '                                              
         B     VREMLEND                                                         
*                                                                               
VREMLESR DS    0H                                                               
         MVC   EMLLIN1(4),=C'ESR '                                              
*                                                                               
VREMLEND DS    0H                                                               
*                                                                               
*        FORMAT REST OF EML MESSAGE                                             
*                                                                               
         MVC   EMLLIN3,=CL80'AGENCY:'                                           
         MVC   EMLLIN3+8(2),KEY                                                 
*                                                                               
         MVC   EMLLIN4,=CL80'MEDIA :'                                           
         MVC   EMLLIN4+8(1),QMED                                                
         MVC   EMLLIN4+15(10),MEDNM                                             
*                                                                               
         MVC   EMLLIN5,=CL80'CLIENT:'                                           
         MVC   EMLLIN5+8(3),QCLT                                                
         MVC   EMLLIN5+15(20),CLTNM                                             
*                                                                               
         MVI   EMLLIN6,X'FF'       END OF MESSAGE                               
*                                                                               
         LA    R1,SMTPC            ESTABLISH JESMAIL PARAMETER BLOCK            
         USING SMTPD,R1                                                         
*                                                                               
         XC    SMTPC,SMTPC         INIT PARAMETER BLOCK                         
*                                                                               
         LA    RF,TOEMLST          SET TO ADDRESS                               
         ST    RF,SMTPTO                                                        
         LA    RF,EMLSUBJ                                                       
         ST    RF,SMTPSUB          SET SUBJECT ADDRESS                          
         LA    RF,EMLMSG           E-MAIL ADDRESS                               
         ST    RF,SMTPDATA                                                      
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
*                                                                               
         L     RF,CJESMAIL        JESMAIL  ADDRESS                              
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTOR (RF),(R1)        SEND E-MAIL                                     
*                                                                               
VRUPEIOK DS    0H                                                               
*                                                                               
VRUPEIOX DS    0H                                                               
*                                                                               
         L     RF,AIO1             GET RECORD ADDRESS                           
         ST    RF,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         GOTOR VRECUP,DMCB,,(R6),(R6),0  DELETE HEADER ELEMENT                  
*                                                                               
         L     R4,AIO1             GET RECORD ADDRESS                           
         ST    R4,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R3,SCHFIRST-SCHREC(R4)    POINT TO FIRST ELEMENT IN REC          
         LA    R6,SVHDRELM         POINT TO SAVED HEADER ELEMENT                
*                                                                               
         GOTOR VRECUP,DMCB,,(R6),(R3),0  ADD CHANGED ELEMENT                    
*                                                                               
         B     VRUPACT             UPDATE ACTIVITY ELEMENT                      
*                                                                               
VRUPCHGX DS    0H                                                               
*                                                                               
         B     VRHDRX                                                           
*                                                                               
VRUPCHGN DS    0H                                                               
*                                                                               
         B     VRHDRX              UNKNOWN ACTION                               
*                                                                               
VRUPACT  DS    0H                  ADD NEW ACTIVITY ELEMENT                     
*                                                                               
         BRAS  RE,ACTPUT           ADD ACTIVITY ELEM                            
*                                                                               
VRHDRX   DS    0H                                                               
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
VRACTMSE DS    0H                  ACTIVATION DATE MISSING                      
         LHI   RF,PWEACTMS                                                      
         B     VRERR                                                            
*                                                                               
VRACTNVE DS    0H                  ACTIVATION DATE NOT VALID                    
         LHI   RF,PWEACTNV                                                      
         B     VRERR                                                            
*                                                                               
VRACTERE DS    0H                  ACTIVATION DATE NOT IN FUTURE                
         LHI   RF,PWEACTER                                                      
         B     VRERR                                                            
*                                                                               
VRACTMDE DS    0H                  ACTIVATION DATE BEFORE MEDIA DATE            
         LHI   RF,PWEACTMD                                                      
         B     VRERR                                                            
*                                                                               
VRACTNOX DS    0H                  ACTIVATION DATE CANNOT BE REMOVED            
         LHI   RF,PWEACTNX                                                      
         B     VRERR                                                            
*                                                                               
VREIOENF DS    0H                  EIO/ESR MEDIA RECORD OPTION MISSING          
         LHI   RF,PWEEIONF                                                      
         B     VRERR                                                            
*                                                                               
VREIOMSE DS    0H                  EIO/ESR OPTION MISSING                       
         LHI   RF,PWEEIOMS                                                      
         B     VRERR                                                            
*                                                                               
VRALLMSE DS    0H                  ALL FOUR OPTIONS MISSING                     
         LHI   RF,PWEOPTMS          (AT LEAST 1 OF 4 REQUIRED)                  
         B     VRERR                                                            
*                                                                               
VREIONV  DS    0H                  EIO/ESR OPTION INVALID                       
         LHI   RF,PWEEIONV                                                      
         B     VRERR                                                            
*                                                                               
VREIOEXO DS    0H                  ENTRY IN EIO BY ESTIMATE REQUIRES            
         LHI   RF,PWEEIOXO           ENTRY IN USE EIO OPTION                    
         B     VRERR                                                            
*                                                                               
VRESREXO DS    0H                  ENTRY IN ESR BY ESTIMATE REQUIRES            
         LHI   RF,PWEESRXO           ENTRY IN USE ESR OPTION                    
         B     VRERR                                                            
*                                                                               
VREIODT  DS    0H                  EIO/ESR OPTION NEEDED WITH DATE              
         LHI   RF,PWEEIODT                                                      
         B     VRERR                                                            
*                                                                               
VREIONVE DS    0H                  EIO/ESR OPTION BY EST INVALID                
         LHI   RF,PWEEIENV                                                      
         B     VRERR                                                            
*                                                                               
VREIODTE DS    0H                  EIO/ESR OPT BY EST NEEDED WITH DATE          
         LHI   RF,PWEEIEDT                                                      
         B     VRERR                                                            
*                                                                               
VREDTNVE DS    0H                  EIO/ESR BY EST DATE NOT MMM/YY               
         LHI   RF,PWEEDTNV                                                      
         B     VRERR                                                            
*                                                                               
VRHDRNV  DS    0H                  INVALID DOMAIN NAME                          
         LHI   RF,PWEEBSNV         SET ERROR CODE                               
         B     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
*                                                                               
         STCM  RF,3,ERROR2CD       SET ERROR EQUATE                             
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         BE    *+14                                                             
         MVC   ERROR2CD+1(1),ERROR      PUT IN NEW STYLE                        
         MVI   ERROR2CD,0                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVC   GTMSGNO,ERROR2CD    MESSAGE NUMBER                               
*                                                                               
VRERRXX  DS    0H                                                               
         GOTOR ERREX                                                            
*                                                                               
*                                                                               
TOEMLST  DS    0C                  TO EMAIL LIST                                
         DC    CL60'kwang@mediaocean.com'                                       
         DC    CL60'pwillis@mediaocean.com'                                     
         DC    X'FF'               END OF LIST                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DR'                      
***********************************************************************         
*                                                                     *         
*        DISPLAY  RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRHDR'                   
***********************************************************************         
*                                                                     *         
*        DISPLAY  RECORD HEADER INFO                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRHDR    DS    0H                                                               
*                                                                               
         L     R6,AIO              POINT TO RECORD FOR DISPLAY                  
         USING SCHHDRD,R6          ESTABLISH AS HEADER ELEMENT                  
*                                                                               
         MVI   ELCODE,SCHHDRQ      LOOK FOR HEADER ELM                          
         BRAS  RE,GETEL            FIND IN RECORD                               
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRDATE'                  
***********************************************************************         
*                                                                     *         
*        DISPLAY EIO ACTIVATION DATE                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRDATE   DS    0H                  EIO ACTIVATION DATE                          
*                                                                               
         LA    R2,SCRACTDH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRACTD,SCRACTD     INIT OUTPUT                                  
*                                  DISPLAY ACTIVATION DATE                      
         GOTOR DATCON,DMCB,(3,SCHACTVD),(5,FLDDATA),0                           
*                                                                               
         MVI   FLDILEN,8           SET FIELD LENGTH                             
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         MVC   SVEIODT,SCHACTVD    SAVE DATE FOR VR TESTING                     
*                                                                               
DRDATEX  DS    0H                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRACTD'                  
***********************************************************************         
*                                                                     *         
*        DISPLAY ESR ACTIVATION DATE                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRACTD   DS    0H                  ESR ACTIVATION DATE                          
*                                                                               
         LA    R2,SCRSRDTH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRSRDT,SCRSRDT     INIT OUTPUT                                  
*                                  DISPLAY ACTIVATION DATE                      
         GOTOR DATCON,DMCB,(3,SCHSRACT),(5,FLDDATA),0                           
*                                                                               
         MVI   FLDILEN,8           SET FIELD LENGTH                             
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         MVC   SVESRDT,SCHSRACT    SAVE DATE FOR VR TESTING                     
*                                                                               
DRACTDX  DS    0H                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DREIO'                   
***********************************************************************         
*                                                                     *         
*        DISPLAY  EIO OPTION                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DREIO    DS    0H                  EIO OPTION                                   
*                                                                               
         LA    R2,SCREIOH          POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCREIO,SCREIO       INIT OUTPUT                                  
*                                                                               
         MVC   SCREIO(L'SCHEIO),SCHEIO DISPLAY EIO OPTION                       
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         MVC   SVEIO,SCHEIO        SAVE OPTION FOR VR TESTING                   
*                                                                               
DREIOX   DS    0H                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRESR'                   
***********************************************************************         
*                                                                     *         
*        DISPLAY  ESR OPTION                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRESR    DS    0H                  ESR OPTION                                   
*                                                                               
         LA    R2,SCRESRH          POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRESR,SCRESR       INIT OUTPUT                                  
*                                                                               
         MVC   SCRESR(L'SCHESR),SCHESR    DISPLAY ESR OPTION                    
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         MVC   SVESR,SCHESR        SAVE OPTION FOR VR TESTING                   
*                                                                               
DRESRX   DS    0H                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRDATEE'                 
***********************************************************************         
*                                                                     *         
*        DISPLAY EIO BY ESTIMATE ACTIVATION DATE                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRDATEE  DS    0H                  EIO BY ESTIMATE ACTIVATION DATE              
*                                                                               
         LA    R2,SCREACDH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCREACD,SCREACD     INIT OUTPUT                                  
*                                  DISPLAY ACTIVATION DATE                      
         GOTOR DATCON,DMCB,(3,SCHEACD),(9,FLDDATA),0                            
*                                                                               
         MVI   FLDILEN,6           SET FIELD LENGTH                             
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         MVC   SVEIODTE,SCHEACD    SAVE DATE FOR VR TESTING                     
*                                                                               
DRDATEEX DS    0H                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRACTDE'                 
***********************************************************************         
*                                                                     *         
*        DISPLAY ESR BY ESTIMATE ACTIVATION DATE                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRACTDE  DS    0H                  ESR BY ESTIMATE ACTIVATION DATE              
*                                                                               
         LA    R2,SCRESRDH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRESRD,SCRESRD     INIT OUTPUT                                  
*                                  DISPLAY ACTIVATION DATE                      
         GOTOR DATCON,DMCB,(3,SCHESRD),(9,FLDDATA),0                            
*                                                                               
         MVI   FLDILEN,6           SET FIELD LENGTH                             
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         MVC   SVESRDTE,SCHESRD    SAVE DATE FOR VR TESTING                     
*                                                                               
DRACTDEX DS    0H                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DREIOE'                  
***********************************************************************         
*                                                                     *         
*        DISPLAY  EIO BY ESTIMATE OPTION                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DREIOE   DS    0H                  EIO BY ESTIMATE OPTION                       
*                                                                               
         LA    R2,SCREIOEH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCREIOE,SCREIOE     INIT OUTPUT                                  
*                                                                               
         MVC   SCREIOE(L'SCHEIOE),SCHEIOE   DISPLAY EIO BY EST OPTION           
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         MVC   SVEIOE,SCHEIOE      SAVE OPTION FOR VR TESTING                   
*                                                                               
DREIOEX  DS    0H                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRESRE'                  
***********************************************************************         
*                                                                     *         
*        DISPLAY ESR BY ESTIMATE OPTION                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRESRE   DS    0H                  ESR BY EST OPTION                            
*                                                                               
         LA    R2,SCRESREH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRESRE,SCRESRE     INIT OUTPUT                                  
*                                                                               
         MVC   SCRESRE(L'SCHESRE),SCHESRE    DISPLAY ESR BY EST OPTION          
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         MVC   SVESRE,SCHESRE      SAVE OPTION FOR VR TESTING                   
*                                                                               
DRESREX  DS    0H                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRTACD'                  
***********************************************************************         
*                                                                     *         
*        DISPLAY  ACCESS TIME OUT                                     *         
*                                                                     *         
*              NOT USED AT THE MOMENT                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRTACD   DS    0H                  ACCESS TIME OUT - DAYS                       
*                                                                               
         B     DRTACX                                                           
*                                                                               
         LA    R2,SCRTACDH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRTACD,SCRTACD     INIT OUTPUT                                  
         CLI   SCHACCTM+0,0        ANYTHING THERE ?                             
         BNH   DRTACDX             NO                                           
*                                                                               
         EDIT  (B1,SCHACCTM+0),(3,FLDDATA),ALIGN=LEFT                           
*                                                                               
DRTACDX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRTACH   DS    0H                  ACCESS TIME OUT - HOURS                      
*                                                                               
         LA    R2,SCRTACHH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRTACH,SCRTACH     INIT OUTPUT                                  
         CLI   SCHACCTM+1,0        ANYTHING THERE ?                             
         BNH   DRTACHX             NO                                           
*                                                                               
         EDIT  (B1,SCHACCTM+1),(3,FLDDATA),ALIGN=LEFT                           
*                                                                               
DRTACHX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRTACM   DS    0H                  ACCESS TIME OUT - MINUTES                    
*                                                                               
         LA    R2,SCRTACMH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRTACM,SCRTACM     INIT OUTPUT                                  
         CLI   SCHACCTM+2,0        ANYTHING THERE ?                             
         BNH   DRTACMX             NO                                           
*                                                                               
         EDIT  (B1,SCHACCTM+2),(3,FLDDATA),ALIGN=LEFT                           
*                                                                               
DRTACMX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRTACX   DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRTAOD'                  
***********************************************************************         
*                                                                     *         
*        DISPLAY  ACTION TIME OUT                                     *         
*                                                                     *         
*              NOT USED AT THE MOMENT                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRTAOD   DS    0H                  ACTION TIME OUT - DAYS                       
*                                                                               
         B     DRTAOX                                                           
*                                                                               
         LA    R2,SCRTAODH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRTAOD,SCRTAOD     INIT OUTPUT                                  
         CLI   SCHACTTM+0,0        ANYTHING THERE ?                             
         BNH   DRTAODX             NO                                           
*                                                                               
         EDIT  (B1,SCHACTTM+0),(3,FLDDATA),ALIGN=LEFT                           
*                                                                               
DRTAODX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRTAOH   DS    0H                  ACTION TIME OUT - HOURS                      
*                                                                               
         LA    R2,SCRTAOHH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRTAOH,SCRTAOH     INIT OUTPUT                                  
         CLI   SCHACTTM+1,0        ANYTHING THERE ?                             
         BNH   DRTAOHX             NO                                           
*                                                                               
         EDIT  (B1,SCHACTTM+1),(3,FLDDATA),ALIGN=LEFT                           
*                                                                               
DRTAOHX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRTAOM   DS    0H                  ACTION TIME OUT - MINUTES                    
*                                                                               
         LA    R2,SCRTAOMH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRTAOM,SCRTAOM     INIT OUTPUT                                  
         CLI   SCHACTTM+2,0        ANYTHING THERE ?                             
         BNH   DRTAOMX             NO                                           
*                                                                               
         EDIT  (B1,SCHACTTM+2),(3,FLDDATA),ALIGN=LEFT                           
*                                                                               
DRTAOMX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRTAOX   DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRIOP'                   
***********************************************************************         
*                                                                     *         
*        DISPLAY  INSERTION ORDER PERIOD                              *         
*                                                                     *         
*              NOT USED AT THE MOMENT                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRIOP    DS    0H                  INSERTION ORDER PERIOD                       
*                                                                               
         B     DRIOPX                                                           
*                                                                               
         LA    R2,SCRIOPH          POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         MVI   SCRIOP,C' '         INIT OUTPUT                                  
         CLI   SCHPER,0            ANYTHING THERE ?                             
         BNH   DRIOP1              NO                                           
*                                                                               
         MVC   FLDDATA(L'SCRIOP),SCHPER                                         
*                                                                               
DRIOP1   DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRIOPX   DS    0H                                                               
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DRIOD'                   
***********************************************************************         
*                                                                     *         
*        DISPLAY  IO# DEPENDENCY                                      *         
*                                                                     *         
*              NOT USED AT THE MOMENT                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRIODM   DS    0H                  IO# DEPENDS ON - AGENCY (MEDIA)              
*                                                                               
         B     DRIODX                                                           
*                                                                               
         LA    R2,SCRIODMH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         MVI   FLDDATA,C'N'        DEFAULT                                      
         TM    SCHIO#TP,SCH#AGYQ   AGENCY?                                      
         BNO   DRIODMX             NO                                           
*                                                                               
         MVI   FLDDATA,C'Y'                                                     
*                                                                               
DRIODMX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRIODC   DS    0H                  IO# DEPENDS ON - CLIENT                      
*                                                                               
         LA    R2,SCRIODCH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         MVI   FLDDATA,C'N'        DEFAULT                                      
         TM    SCHIO#TP,SCH#CLTQ   CLIENT?                                      
         BNO   DRIODCX             NO                                           
*                                                                               
         MVI   FLDDATA,C'Y'                                                     
*                                                                               
DRIODCX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRIODP   DS    0H                  IO# DEPENDS ON - PRODUCT                     
*                                                                               
         LA    R2,SCRIODPH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         MVI   FLDDATA,C'N'        DEFAULT                                      
         TM    SCHIO#TP,SCH#PRDQ   AGENCY?                                      
         BNO   DRIODPX             NO                                           
*                                                                               
         MVI   FLDDATA,C'Y'                                                     
*                                                                               
DRIODPX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRIODB   DS    0H                  IO# DEPENDS ON - PUB                         
*                                                                               
         LA    R2,SCRIODBH         POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         MVI   FLDDATA,C'N'        DEFAULT                                      
         TM    SCHIO#TP,SCH#PUBQ   PUB?                                         
         BNO   DRIODBX             NO                                           
*                                                                               
         MVI   FLDDATA,C'Y'                                                     
*                                                                               
DRIODBX  DS    0H                                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DRIODX   DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - RDEL'                    
***********************************************************************         
*                                                                     *         
*        DELETE   RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
RDEL     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
*        ADD OR UPDATE ACTIVITY ELEMENT                                         
*                                                                               
         OI    SVACH1,SCHADEL      SET RECORD DELETED (IN SAVE AREA)            
*                                  NOW . . .                                    
         BRAS  RE,ACTPUT           DO ACTIVITY ELEMENT                          
*                                                                               
RDELX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - RR'                      
***********************************************************************         
*                                                                     *         
*        RESTORE  RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
RR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
*        ADD OR UPDATE ACTIVITY ELEMENT                                         
*                                                                               
         OI    SVACH1,SCHARES      SET RECORD RESTORED(IN SAVE AREA)            
*                                  NOW . . .                                    
         BRAS  RE,ACTPUT           DO ACTIVITY ELEMENT                          
*                                                                               
RRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - XR'                      
***********************************************************************         
*                                                                     *         
*        PASSIVE KEY WORK                                             *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
XR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         L     R6,AIO              POINT TO RECORD                              
         LA    R6,33(R6)           HEADER ELEMENT                               
         USING SCHHDRD,R6                                                       
*                                                                               
         MVC   PASELCOD,=X'6202'   EIO PASSIVE POINTER ID                       
         MVC   EIOESRCD,SCHEIO      AND EIO INDICATOR                           
         BRAS  RE,PASSIV           PASSIVE KEYS I/O WORK                        
*                                                                               
         MVC   PASELCOD,=X'6203'   ESR PASSIVE POINTER ID                       
         MVC   EIOESRCD,SCHESR      AND ESR INDICATOR                           
         BRAS  RE,PASSIV           PASSIVE KEYS I/O WORK                        
*                                                                               
XRX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C41 ESR VENDOR CONTACT LIST RECORDS - PR'                    
***********************************************************************         
*                                                                     *         
*        PRINT REPORT                                                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
PR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         CLI   SCRMED,C'O'                                                      
         BNE   *+12                                                             
         MVI   RCSUBPRG,1          OUTDOOR                                      
         B     *+8                                                              
         MVI   RCSUBPRG,0          ALL OTHER MEDIA                              
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         BRAS  RE,LR               USE LIST REC LOGIC                           
*                                                                               
PRX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C41 ESR VENDOR CONTACT LIST RECORDS - HOOK'                  
***********************************************************************         
*                                                                     *         
*        HEADLINE ROUTINES                                            *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
HOOK     NTR1  BASE=*,LABEL=*      HEADLINE ROUTINES                            
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         MVC   HEAD4(5),=C'MEDIA'                                               
         MVC   HEAD4+7(1),QMED                                                  
         MVC   HEAD4+9(10),MEDNM                                                
*                                                                               
HOOKX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - LR'                      
***********************************************************************         
*                                                                     *         
*        LIST RECRDS                                                  *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
LR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         OI    GLSTSTAT,RETEXTRA   RETURN AFTER LAST ON SCREEN                  
*                                                                               
         LA    R4,KEY              ESTABLISH RECORD KEY                         
         USING SCHREC,R4                                                        
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR1STX              KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
*        BUILD STARTING KEY                                                     
*                                                                               
         MVC   SCHKAGY,AGENCY      AGENCY                                       
         MVC   SCHKMED,QMED        MEDIA CODE                                   
         MVI   SCHKRCD,SCHKRCDQ    TYPE                                         
         MVC   SCHKCLT,QCLT           CLIENT                                    
*                                                                               
         GOTOR HIGH                READ FIRST RECORD                            
*                                                                               
LR1STX   DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         TM    SCHDCNTL,SCHCDELQ   SKIP DELETED RECORDS                         
         BO    LRSEQ                                                            
*                                                                               
         CLC   SCHKKEY(SCHKCLT-SCHKKEY),KEYSAVE  DONE AT END OF AG/M/RC         
         BNE   LRDONE                                                           
*                                                                               
         OC    SCHKPRD,SCHKPRD     SKIP IF NOT FOR ALL PRODUCTS                 
         BNZ   LRSEQ                                                            
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
*                                                                               
*        FILTER ON CLIENT                                                       
*                                                                               
         OC    QCLT,QCLT           SKIP IF CLIENT NOT GIVEN                     
         BZ    LRCLTX                                                           
*                                                                               
         CLC   SCHKCLT,QCLT                                                     
         BL    LRSEQ               START AT CLIENT                              
*                                                                               
LRCLTX   DS    0H                                                               
*                                                                               
         GOTOR GETREC              GET SETUP RECORD                             
*                                                                               
         MVC   MYDSKADD,DMDSKADD   SAVE D/A FOR LIST                            
*                                                                               
         L     R6,AIO              POINT TO FOUND RECORD                        
*                                                                               
*        FIND HEADER ELEMENT                                                    
*                                                                               
         L     R6,AIO              POINT TO START OF RECORD                     
         MVI   ELCODE,SCHHDRQ      LOOK FOR HEADER ELM                          
         BRAS  RE,GETEL            FIND IN RECORD                               
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                                                               
         USING SCHHDRD,R6                                                       
*                                                                               
*        FILTER ON EIO STATUS                                                   
*                                                                               
LREIOFL  DS    0H                                                               
*                                                                               
         CLI   SCLEIO,C' '         SKIP IF NO FILTER                            
         BNH   LREIOFLX                                                         
*                                                                               
         CLC   SCHEIO,SCLEIO       EIO OPTION MUST MATCH FILTER                 
         BNE   LRSEQ                                                            
*                                                                               
LREIOFLX DS    0H                                                               
*                                                                               
*        FILTER ON ESR STATUS                                                   
*                                                                               
LRESRFL  DS    0H                                                               
*                                                                               
         CLI   SCLESR,C' '         SKIP IF NO FILTER                            
         BNH   LRESRFLX                                                         
*                                                                               
         CLC   SCHESR,SCLESR       ESR OPTION MUST MATCH FILTER                 
         BNE   LRSEQ                                                            
*                                                                               
LRESRFLX DS    0H                                                               
*                                                                               
         USING LISTD,R5            ESTABLISH LIST DISPLAY                       
*                                                                               
         LA    R5,P1               USE P LINES                                  
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    *+8                                                              
         LA    R5,LISTAR           OR LIST AREA                                 
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
*        DISPLAY CLIENT                                                         
*                                                                               
         MVC   LCLT,SCHKCLT        CLIENT                                       
*                                                                               
         OC    SCHKCLT,SCHKCLT                                                  
         BNZ   LRCLT10                                                          
*                                                                               
         MVC   LCLT,=C'   '                                                     
         MVC   LCLTNM,=CL20'ALL CLIENTS'                                        
*                                                                               
         B     LRCLT20                                                          
*                                                                               
LRCLT10  DS    0H                                                               
*                                                                               
         GOTOR MYVCLT,DMCB,LCLT    GET CLIENT NAME                              
*                                                                               
         MVC   LCLTNM,CLTNM        DISPLAY NAME                                 
*                                                                               
LRCLT20  DS    0H                                                               
*                                                                               
*        DISPLAY EIO/ESR OPTIONS                                                
*                                                                               
         MVC   LEIO,SCHEIO                                                      
         MVC   LESR,SCHESR                                                      
         MVC   LEIOE,SCHEIOE                                                    
         MVC   LESRE,SCHESRE                                                    
*                                                                               
*        DISPLAY ACTIVATION DATES                                               
*                                                                               
LRLRDAT  DS    0H                  EIO ACTIVATION DATE                          
*                                                                               
         OC    SCHACTVD,SCHACTVD   "SPECIFIC" ACTIVATION DATE ?                 
         BZ    LRLRDAT2            NO                                           
*                                  DISPLAY DATE                                 
         GOTOR DATCON,DMCB,(3,SCHACTVD),(17,LACTD),0                            
*                                                                               
LRLRDAT2 DS    0H                  ESR ACTIVATION DATE                          
*                                                                               
         OC    SCHSRACT,SCHSRACT   "SPECIFIC" ACTIVATION DATE ?                 
         BZ    LRLREDAT            NO                                           
*                                  DISPLAY DATE                                 
         GOTOR DATCON,DMCB,(3,SCHSRACT),(17,LSRDT),0                            
*                                                                               
*                                                                               
LRLREDAT DS    0H                  EIO BY EST ACTIVATION DATE                   
*                                                                               
         OC    SCHEACD,SCHEACD     "SPECIFIC" ACTIVATION DATE ?                 
         BZ    LRLREDT2            NO                                           
*                                  DISPLAY DATE                                 
         GOTOR DATCON,DMCB,(3,SCHEACD),(9,LACTDE),0                             
*                                                                               
LRLREDT2 DS    0H                  ESR BY EST ACTIVATION DATE                   
*                                                                               
         OC    SCHESRD,SCHESRD     "SPECIFIC" ACTIVATION DATE ?                 
         BZ    LRLRATVX            NO                                           
*                                  DISPLAY DATE                                 
         GOTOR DATCON,DMCB,(3,SCHESRD),(9,LSRDTE),0                             
*                                                                               
LRLRATVX DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
LRLRCONT DS    0H                  LIST NEXT RECORD                             
*                                                                               
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   LRLRCON1                                                         
*                                                                               
         GOTOR SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
         B     LRCONT                                                           
*                                                                               
LRLRCON1 DS    0H                                                               
*                                                                               
         MVC   DMDSKADD,MYDSKADD   RESTORE D/A                                  
*                                                                               
         GOTOR LISTMON             CALL LISTMON                                 
*                                                                               
LRCONT   DS    0H                                                               
*                                                                               
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTOR HIGH                                                             
*                                                                               
LRSEQ    DS    0H                                                               
*                                                                               
         LA    R4,KEY              MUST RESET R4 TO KEY                         
         USING SCHREC,R4                                                        
*                                                                               
         GOTOR SEQ                                                              
*                                                                               
         B     LRLOOP                                                           
*                                                                               
LRDONE   DS    0H                                                               
*                                                                               
         XC    KEY,KEY             TELL GENCON ALL DONE                         
*                                                                               
LRX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - MYVPUB'                  
***********************************************************************         
*                                                                     *         
*        GET PUB NAME                                                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
MYVPUB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   PUBNM,SPACES                                                     
         MVC   PUBZNM,SPACES                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),MYPUB    MOVE PUB/ZONE/EDTN                           
         CLC   MYPUB+4(2),=X'FFFF'    ALL ZONES/EDTS                            
         BNE   *+10                                                             
         XC    PUBKPUB+4(2),PUBKPUB+4   READ "BASE" PUB                         
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTOR HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VPNO                                                             
*                                                                               
         MVC   AIO,AIO2            USE AIO2                                     
*                                                                               
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTOR GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
         L     R6,AIO2             POINT TO FOUND RECORD                        
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUBNAMEL,R6                                                      
         MVC   PUBNM,PUBNAME                                                    
         MVC   PUBZNM(3),=C'ALL'                                                
         CLC   MYPUB+4(2),=X'FFFF'   ALL ZONES/EDTS                             
         BE    *+10                                                             
         MVC   PUBZNM,PUBZNAME                                                  
*                                                                               
VPNO     DS    0H                                                               
*                                                                               
         MVC   FILENAME,=CL8'PUBDIR'                                            
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTOR HIGH                                                             
         XC    FILENAME,FILENAME                                                
         B     MYVPUBX                                                          
*                                                                               
MYVPUBX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - MYVCLT'                  
***********************************************************************         
*                                                                     *         
*        GET CLIENT NAME                                              *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
MYVCLT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         L     R2,0(R1)            POINT TO CLIENT CODE                         
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   CLTNM,SPACES                                                     
*                                                                               
         XC    KEY,KEY             ESTABLISH PCLTKEY                            
         LA    R4,KEY                                                           
         USING PCLTRECD,R4                                                      
*                                                                               
         MVC   PCLTKMED,QMED                                                    
         MVC   PCLTKAGY,AGENCY                                                  
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),0(R2)      CLIENT CODE                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PRTDIR'                                            
         GOTOR HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(10),KEYSAVE                                                  
         BNE   VCNO                                                             
*                                                                               
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME,=CL8'PRTFILE'                                           
         GOTOR GETREC                                                           
         XC    FILENAME,FILENAME                                                
         MVC   CLTNM,PCLTNAME                                                   
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTOR HIGH                                                             
         XIT1                                                                   
*                                                                               
VCNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTOR HIGH                                                             
         LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM33 - INSERTION ORDER SETUP RECORD - ACTPUT'                
***********************************************************************         
*                                                                     *         
*        ROUTINE TO ADD ACTIVITY DATA TO RECORD                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
ACTPUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         BRAS  RE,ACTLMT           SEE IF MUST DELETE EXCESS ELEMENT(S)         
*                                                                               
*        ADD ACTIVITY ELEMENT                                                   
*                                                                               
         XC    SVACTELM,SVACTELM   INIT WORK AREA                               
         LA    R3,SVACTELM                                                      
         USING SCHACTHD,R3         ESTABLISH ACTIVITY ELM                       
*                                                                               
         MVI   SCHACDE,SCHAIDQ     SET ELEMENT CODE                             
         MVI   SCHALEN,SCHACTLQ    SET ELEMENT LENGTH                           
*                                                                               
         GOTOR DATCON,DUB,(5,0),(3,SCHADTE) SET TODAY'S DATE                    
*                                                                               
         BRAS  RE,PIDGET           FIND PID                                     
*                                                                               
         MVC   SCHAPID,SVPID       SET CHANGER'S PID                            
*NOP*    MVC   SCHASCID,SVSCID     SET CHANGER'S SECURITY PID                   
*                                                                               
         MVC   SCHACHGS,SVACHGS    SET ACTIVITY INDICATORS                      
*                                                                               
         L     R6,AIO              POINT TO RECORD FOR DISPLAY                  
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
*        CHECK OLD ACTIVITY FOR DUPLICATES                                      
*                                                                               
         MVI   ELCODE,SCHAIDQ      LOOK FOR ACTIVITY ELM                        
         BRAS  RE,GETEL            FIND IN RECORD                               
*                                                                               
ACTPUTLP DS    0H                                                               
*                                                                               
         BNE   ACTPUTDN            SKIP IF NONE FOUND                           
*                                                                               
         CLC   SCHAPID,SCHAPID-SCHAELM(R6)  IF SAME PERSON                      
         BNE   ACTPUTCN                                                         
*NOP*    CLC   SCHASCID,SCHASCID-SCHAELM(R6) IF SAME SCID                       
*NOP*    BNE   ACTPUTCN                                                         
         CLC   SCHADTE,SCHADTE-SCHAELM(R6)  AND DATE                            
         BNE   ACTPUTCN                                                         
*                                                                               
         OC    SCHACHGS-SCHAELM(L'SCHACHGS,R6),SVACHGS UPDATE CHANGES           
*                                                                               
         B     ACTPUTDX               SKIP ADD OF ELM                           
*                                                                               
ACTPUTCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL                                                        
*                                                                               
         B     ACTPUTLP                                                         
*                                                                               
ACTPUTDN DS    0H                                                               
*                                                                               
         GOTOR VRECUP,DMCB,,(R3),(C'R',(R6)),0  ADD ELEMENT                     
*                                                                               
         CLI   DMCB+8,0            ONLY ERROR CAN BE NO ROOM IN REC             
         BE    ACTPUTER            NO ERROR TOLERATED                           
*                                                                               
ACTPUTDX DS    0H                                                               
*                                                                               
ACTPUTX  DS    0H                                                               
         MVC   ERROR,SAVMSGNO      RESTORE MESSAGE NUMBER                       
         XIT1                                                                   
*                                                                               
ACTPUTER MVI   ERROR,RECFULL       NO ROOM IN RECORD OR TABLE                   
         LA    R2,SCRACTDH                                                      
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM33 - INSERTION ORDER SETUP RECORD - ACTLMT'                
***********************************************************************         
*                                                                     *         
*       ROUTINE TO LIMIT NUMBER OF ACTIVITY ELEMENTS TO TEN           *         
*       (NINE MOST RECENT PLUS THE FIRST ACTIVITY ELEMENT)            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
ACTLMT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         L     R6,AIO              POINT TO RECORD                              
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
*        COUNT NUMBER OF EXISTING ACTIVITY ELEMENTS                             
*                                                                               
         LA    R4,1                USE R4 FOR ELEMENT COUNTER                   
*                                                                               
         MVI   ELCODE,SCHAIDQ      LOOK FOR ACTIVITY ELEM                       
         BRAS  RE,GETEL            FIND IN RECORD                               
         BNE   ACTLMTX             NOTHING FOUND                                
*                                                                               
ACTLMTC  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   ACTLMTCX            NO MORE ACTIVITY ELEMS                       
         AHI   R4,1                                                             
         B     ACTLMTC             LOOK FOR MORE                                
*                                                                               
ACTLMTCX DS    0H                                                               
         CHI   R4,10               TEN OR MORE ACTIVITY ELEMS ?                 
         BL    ACTLMTX             NO - OK - NO DELETION NEEDED                 
*                                                                               
*        DELETE ELEMENTS - LEAVE FIRST ONE AND LAST TWO                         
*                                                                               
         AHI   R4,-9               SET R4 TO NUMBER OF ELEMS TO DELETE          
         L     R6,AIO              POINT TO RECORD                              
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         XC    DMCB+8(4),DMCB+8    SET FOR DELETE                               
*                                                                               
         MVI   ELCODE,SCHAIDQ      LOOK FOR ACTIVITY ELEM                       
         BRAS  RE,GETEL            FIND IN RECORD                               
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
ACTLMTD  DS    0H                                                               
         BRAS  RE,NEXTEL           GO TO SECOND ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
ACTLMTDD DS    0H                                                               
         GOTO1 VRECUP,DMCB,,(R6),0        DELETE ELEMENT                        
*                                                                               
         BCT   R4,ACTLMTDD         DELETE ANOTHER                               
*                                                                               
ACTLMTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM33 - INSERTION ORDER SETUP RECORD - PASSIV'                
***********************************************************************         
*                                                                     *         
*        ROUTINE TO HANDLE ALL PASSIVE KEY ACTIVITY                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
PASSIV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
*        ESTABLISH "CURRENT" KEY                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SCHKKEY),ORIGKEY                                           
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ FOR RECORD KEY                          
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         CLC   KEY(L'SCHKKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                                                               
*        LOOK FOR EXISTING PASSIVE KEY                                          
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS SETUP PASSIVE               
         USING SCH1KEYD,R4                                                      
         LA    R3,SVKEY            ESTABLISH SVKEY AS "CURRENT" KEY             
         USING SCHRECD,R3                                                       
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
*                                                                               
         MVC   SCH1AGY,SCHKAGY     SET AGENCY                                   
         MVC   SCH1MED,SCHKMED     SET MEDIA                                    
         MVC   SCH1RCD,PASELCOD    SET RECORD CODE (2-BYTE)                     
         MVC   SCH1CLT,SCHKCLT     SET CLIENT CODE                              
         MVC   SCH1PRD,SCHKPRD     SET PRODUCT CODE                             
         MVC   SCH1EST,SCHKEST     SET ESTIMATE CODE                            
         MVC   SCH1PUB,SCHKPUB     SET PUB NUMBER                               
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ FOR PASSIVE                             
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         CLC   KEY(L'SCH1KEY),KEYSAVE                                           
         BNE   PSVADD              NOT FOUND - SEE IF ADD NEEDED                
*                                  PASSIVE KEY EXISTS                           
         TM    SCHDCNTL,SCHCDELQ   RECORD DELETED ?                             
         BO    PSVDEL              YES - DELETE PASSIVE                         
*                                                                               
         CLI   EIOESRCD,C'Y'       USE EIO/ESR ?                                
         BNE   PSVDEL              NO - DELETE PASSIVE                          
         TM    SCH1CNTL,SCH1DELQ   DELETED ?                                    
         BNO   PSVDONE             NO - DONE                                    
*****    BO    PSVRES              YES - RESTORE                                
*                                                                               
PSVRES   DS    0H                                                               
         NI    SCH1CNTL,X'FF'-SCH1DELQ     RESTORE                              
         B     PSVWRT                                                           
*                                                                               
PSVDEL   DS    0H                                                               
         TM    SCH1CNTL,SCH1DELQ   DELETED ?                                    
         BO    PSVDONE             YES - DONE                                   
         OI    SCH1CNTL,SCH1DELQ   FLAG DELETED                                 
*                                                                               
PSVWRT   DS    0H                                                               
         GOTOR WRITE               RE-WRITE THE PASSIVE                         
         B     PSVDONE             DONE                                         
*                                                                               
PSVADD   DS    0H                                                               
         CLI   EIOESRCD,C'Y'       USE EIO/ESR ?                                
         BNE   PSVDONE             NO - NO PASSIVE NEEDED                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'SCH1KEY),KEYSAVE    RESET KEY                              
         MVC   SCH1DISK,SCHDDISK   SET D/A                                      
         GOTOR ADD                 ADD THE PASSIVE                              
*                                                                               
PSVDONE  DS    0H                                                               
*                                                                               
PASSIVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM33 - SFM INSERTION ORDER SETUP - PIDGET'                   
***********************************************************************         
*                                                                     *         
*   PIDGET - THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK            *         
*         WHICH ARE "PERSONAL ID"                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
PIDGET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         XC    SVPID,SVPID         PASSWORD ID NUMBER CLEARED                   
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
*                                                                               
         GOTOR CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
         DROP  RF                                                               
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         MVC   SVSECAGY,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
*                                                                               
         TM    FATFLAG,X'08'       CHECK IF SECET CODE IS THERE                 
         BZ    *+10                                                             
         MVC   SVPID,FAPASSWD      SAVE PASSWORD ID NUMBER                      
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         TITLE 'PRSFM33 - VALIDATE CONTACT E-ADDRESS - SUBROUTS'                
***********************************************************************         
* COMMONLY ADDRESSABLE ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBROUTS DS    0D                                                               
***********************************************************************         
*                                                                     *         
* HANDLE FIELD IN ERROR - R1 -POINTS TO FIELD                         *         
*         HIGHLIGHT FIELD                                             *         
*         ERROR MESSAGE IS IN ERROR                                   *         
*         IF SAVMSGNO IS NOT FVFOK THEN THIS IS NOT FIRST ERROR       *         
*            ROUTINE RESTORES ERROR TO SAVMSGNO                       *         
*         ELSE                                                        *         
*            ROUTINE SETS CURSOR TO THIS FIELD                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         USING FLDHDRD,R1          ESTABLISH HEADER                             
ERRFLD   OI    IPSTAT,LUWVERRQ     INDICATE VALIDATION ERROR                    
         OI    FLDATB,FATBHIGH     HIGHLIGHT FIELD                              
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         NI    FLDIIND,X'FF'-FINPVAL TURN OFF VALID INDICATOR                   
         CLI   SAVMSGNO,0          IF NOT FIRST ERROR                           
         JE    *+14                                                             
         MVC   ERROR,SAVMSGNO      RESTORE PRIOR MESSAGE                        
         J     ERRFLDX                                                          
*                                                                               
         ST    R1,ACURFORC         PUT CURSOR HERE                              
*                                                                               
ERRFLDX  DS    0H                                                               
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER LITTLE ROUTINES                                               *         
***********************************************************************         
         SPACE 2                                                                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         JNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - DSPFLD'                  
***********************************************************************         
*                                                                     *         
*        DISPLAY DATA IN  FLD IN FIELD POINTED TO BY R2               *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
DSPFLD   NTR1  BASE=*,LABEL=*      BUMP TO NEXT SCREEN FIELD                    
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         SHI   RF,8                HEADER LENGTH                                
*                                                                               
         TM    FLDATB,X'02'        IF THERE IS EXTENDED HEADER                  
         JNO   *+8                                                              
         SHI   RF,8                   TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),FLD      MOVE DATA TO OUTPUT                          
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
NEEDDATA EQU   83                  AT LEAST ONE ENTRY REQUIRED                  
DUPEDATA EQU   179                 DUPLICATE DATA                               
RECFULL  EQU   180                 RECORD FULL                                  
*                                                                               
         EJECT                                                                  
*                                                                               
PUBZNM   DS    CL20                                                             
MYPUB    DS    XL6                                                              
MYDSKADD DS    XL4                                                              
*                                                                               
         EJECT                                                                  
         PRINT GEN                                                              
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         TITLE 'T41C33 INSERTION ORDER SETUP RECORDS - MYPUBVAL'                
***********************************************************************         
*                                                                     *         
*        VALIDATE PUB FIELD                                           *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
MYPUBVAL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         LR    R4,R2               COPY INPUT FIELD POINTER                     
         XC    BPUB(6),BPUB                                                     
         MVI   ALLZE,C'N'          INITIALIZE                                   
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BNE   VKPUB5                                                           
         CLI   ACTNUM,ACTREP       SEE IF REPORTING                             
         BE    VKPX                                                             
         B     VKPUB5F             PUB NOT ENTERED - TREAT AS ALL               
*                                                                               
VKPUB5   DS    0H                                                               
         CLI   5(R2),3                                                          
         BNE   VKPUB7                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VKPUB7                                                           
VKPUB5F  MVC   BPUB(6),=6X'FF'                                                  
         MVC   PUBNM,=CL20'ALL PUBS'                                            
         MVC   PUBZNM,=CL20' '                                                  
         B     VKPX                                                             
*                                                                               
VKPUB7   CLI   8(R2),C'='          PUB NAME SEARCH                              
         BNE   VKPUB10                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,QMED                                                    
         GOTOR =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         B     VKPUB30                                                          
         DROP  R3                                                               
*                                                                               
VKPUB10  DS    0H                                                               
         LR    R2,R4                                                            
         MVI   ERROR,INVALID                                                    
         XC    SCANBLK,SCANBLK                                                  
         GOTOR SCANNER,DMCB,(R2),(2,SCANBLK)                                    
         CLI   DMCB+4,0                                                         
         BE    VKPUBERR                                                         
         LA    R1,SCANBLK                                                       
         CLC   =C'ALL',44(R1)                                                   
         BNE   VKPUB30                                                          
         MVI   ALLZE,C'Y'          WANT ONLY ACROSS ALL ZONES AND ED            
         ZIC   R3,0(R1)                                                         
         STC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R1)                                                   
*                                                                               
VKPUB30  LR    R2,R4                                                            
         MVC   AIO,AIO2            USE IOAREA2                                  
         GOTOR VALIPUB                                                          
         MVC   AIO,AIO1            RESET TO IOAREA1                             
         CLI   ALLZE,C'Y'                                                       
         BNE   VKPX                                                             
         MVC   BPUB+4(2),=X'FFFF'  ALL ZONES/EDTNS                              
         ZIC   R1,5(R2)                                                         
         AH    R1,=H'4'            PUT BACK THE ORIGINAL LEN                    
         STC   R1,5(R2)                                                         
*                                                                               
VKPX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
VKPUBERR DS    0H                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H1,1,REQUESTOR                                                   
         PSPEC H1,56,C'SET UP RECORD REPORT'                                    
         PSPEC H2,56,C'--------------------'                                    
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,95,AGYADD                                                     
         PSPEC H4,95,RUN                                                        
         PSPEC H5,95,REPORT                                                     
         PSPEC H5,115,PAGE                                                      
         PSPEC H6,1,C'CLT  CLIENT                EIO ACT DATE'                  
         PSPEC H7,1,C'---  ------                --- --------'                  
         PSPEC H6,47,C'ESR  ACT DATE'                                           
         PSPEC H7,47,C'---  --------'                                           
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMAAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMABD                                                       
*                                                                               
         DS    XL64                AREA FOR BOOK= NOTICE                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                                                                
ORIGKEY  DS    XL(L'KEY)                                                        
MYKEY    DS    XL(L'KEY)                                                        
ALLZE    DS    CL1                                                              
SCANBLK  DS    CL70                                                             
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHANGED                 
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
QPID     DS    XL2                 USER PID                                     
*                                                                               
HDRSV    DS    XL256               MEDIA HEADER SAVEAREA                        
*                                                                               
EIOESRCD DS    XL1                 EIO/ESR CODE (Y,N,BLANK) FOR PASSIV          
PASELCOD DS    XL2                 PASSIVE KEYS RECORD CODE FOR PASSIV          
SVEIODT  DS    XL3                 EIO ACTIVATION DATE FROM RECORD              
SVESRDT  DS    XL3                 ESR ACTIVATION DATE FROM RECORD              
SVEIO    DS    XL1                 EIO OPTION (SCHEIO) FROM RECORD              
SVESR    DS    XL1                 ESR OPTION (SCHESR) FROM RECORD              
SVEIODTE DS    XL3                 EIO ACT DATE BY ESTIMATE FROM RECORD         
SVESRDTE DS    XL3                 ESR ACT DATE BY ESTIMATE FROM RECORD         
SVEIOE   DS    XL1                 EIO OPT (SCHEIOE) BY EST FROM RECORD         
SVESRE   DS    XL1                 ESR OPT (SCHESRE) BY EST FROM RECORD         
*                                                                               
SVDIR    DS    X                   LINUP DIRECTION SAVEAREA                     
SVELTKEY DS    XL2                 KEY SAVEAREA                                 
*                                                                               
SCHKSAVE DS    XL(L'SCHKKEY)       KEY SAVEAREA                                 
SVHDRELM DS    XL(SCHHDRLQ)        HEADER ELM SAVEAREA                          
SVACTELM DS    XL(SCHACTLQ)        ACTIVITY ELM SAVEAREA                        
SVPID    DS    XL2                 CHANGER'S PID                                
SVSCID   DS    XL8                 CHANGER'S SECURITY PID                       
SVSECAGY DS    XL(L'FATAGYSC)      SECURITY AGENCY                              
ALINCUR  DS    A                   A(CURSOR)                                    
*                                                                               
SVACHGS  DS    0XL6                ACTIVITY BYTES                               
SVACH1   DS    XL1                 ACTIVTY INDICATOR                            
SVACH2   DS    XL1                 ACTIVTY INDICATOR                            
SVACH3   DS    XL1                 ACTIVTY INDICATOR                            
SVACH4   DS    XL1                 ACTIVTY INDICATOR                            
SVACH5   DS    XL1                 ACTIVTY INDICATOR                            
SVACH6   DS    XL1                 ACTIVTY INDICATOR                            
         DS    0F                                                               
*                                                                               
*        JESMAIL PARAMTER BLOCK                                                 
*                                                                               
         DS    0F                                                               
SMTPC    DS    XL(SMTPDQ)          PARAMTER BLOCK FOR JES MAIL                  
*                                                                               
*        E-MAIL FIELDS                                                          
*                                                                               
EMLFLDS  DS    0D                                                               
EMLTOADR DS    CL60                TO: E-MAIL ADDRESS                           
EMLTOEND DS    XL1                 X'FF' END OF LIST                            
EMLSUBJ  DS    CL70                SUBJECT                                      
EMLMSG   DS    10CL80              MESSAGE                                      
         ORG   EMLMSG                                                           
EMLLIN1  DS    CL80                  LINE 1                                     
EMLLIN2  DS    CL80                  LINE 2                                     
EMLLIN3  DS    CL80                  LINE 3                                     
EMLLIN4  DS    CL80                  LINE 4                                     
EMLLIN5  DS    CL80                  LINE 5                                     
EMLLIN6  DS    CL80                  LINE 6                                     
EMLLIN7  DS    CL80                  LINE 7                                     
EMLLIN8  DS    CL80                  LINE 8                                     
EMLLIN9  DS    CL80                  LINE 9                                     
EMLLIN10 DS    CL80                  LINE 10                                    
         DS    XL1                 POSSIBLE EOL MARKER                          
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
LCLT     DS    CL3                 CLIENT                                       
         DS    CL1                                                              
LCLTNM   DS    CL20                CLIENT NAME                                  
         DS    CL1                                                              
LEIO     DS    CL1                 EIO STATUS                                   
         DS    CL2                                                              
LACTD    DS    CL8                 ACTIVATION DATE                              
         DS    CL3                                                              
LESR     DS    CL1                 ESR STATUS                                   
         DS    CL2                                                              
LSRDT    DS    CL8                 ACTIVATION DATE                              
         DS    CL3                                                              
LEIOE    DS    CL1                 EIO BY EST STATUS                            
         DS    CL2                                                              
LACTDE   DS    CL6                 ACTIVATION DATE                              
         DS    CL3                                                              
LESRE    DS    CL1                 ESR BY EST STATUS                            
         DS    CL2                                                              
LSRDTE   DS    CL6                 ACTIVATION DATE                              
         EJECT                                                                  
       ++INCLUDE PPGENSCH                                                       
         EJECT                                                                  
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
** FAJESMAILD                                                                   
         PRINT OFF                                                              
       ++INCLUDE FAJESMAILD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*DDFLDHDR                                                                       
*DDPERVAL                                                                       
*PPSRCHPARM                                                                     
*FAGETTXTD                                                                      
*DDLINUPD                                                                       
*FAFACTS                                                                        
*FATIOB                                                                         
*PRWRIEQUS                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
       ++INCLUDE DDLINUPD                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE PRWRIEQUS                                                      
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'207PRSFM33   05/11/16'                                      
         END                                                                    
