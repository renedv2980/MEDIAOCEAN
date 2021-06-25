*          DATA SET PRSFM31    AT LEVEL 169 AS OF 03/16/09                      
*PHASE T41C31A                                                                  
*INCLUDE SRCHCALL                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE NUMED                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE PPBROWSE                                                               
*        TITLE 'T41C31 VENDOR CONTACT LIST RECORDS'                             
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS'                             
***********************************************************************         
*                                                                     *         
*         CHANGE LOG                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
* SMYE  08/06    LIMIT NUMBER OF ACTIVITY RECORDS (SEE ACTLMT)                  
*                                                                               
* SMYE  07/05    ADD SUPPRESS COST FIELD (CCVLSUP) AND MODIFY LIST              
*                  FUNCTION DISPLAY/FILTERING AND DISPLAY KEY FUNCTION          
*                                                                               
* BOBY  04/04    BIG BANG                                                       
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - INIT'                      
***********************************************************************         
*                                                                     *         
*        INITIALISATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T41C31   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C31,RR=R3                                                   
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
         MVI   IPSTAT,0            INIT INPUT STATISTICS                        
         MVI   SAVMSGNO,0          INIT MESSAGE NUMBER SAVEAREA                 
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENT                          
*                                                                               
         OI    GENSTAT3,MULTFILS   DEALING WITH MULTIPLE FILES                  
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
*                                                                               
         MVC   SYSDIR,=CL8'PUBDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PUBFILE' SET FILE                                    
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - ANAL'                      
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
         CLI   MODE,XRECADD        RE-DISPLAY RECORD                            
         BNE   *+12                                                             
         NOP   DR                                                               
         B     ANALX                                                            
*                                                                               
         CLI   MODE,XRECPUT        RE-DISPLAY RECORD                            
         BNE   *+12                                                             
         NOP   DR                                                               
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
         CLI   MODE,VALRA          AFTER RECORD/ACTION VAL                      
         BE    *+8                                                              
         CLI   MODE,SETFILE        SET FILE NAME                                
         BNE   ANALSFLX                                                         
*                                                                               
         MVC   SYSDIR,=CL8'PUBDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PUBFILE' SET FILE                                    
         MVC   AIO,AIO1            USE AIO1                                     
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
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - VK'                        
***********************************************************************         
*                                                                     *         
*        VALIDATE KEY                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
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
         MVC   SYSDIR,=CL8'PRTDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PRTFILE' SET FILE                                    
         MVI   USEIONUM,2          USE IOAREA 2 FOR VALIDATION                  
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
VKMED    DS    0H                                                               
*                                                                               
         MVC   SCRMEDN,SPACES      INIT                                         
         MVC   MEDNM,SPACES        INIT                                         
         OI    SCRMEDNH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         LA    R2,SCRMEDH         VALIDATE MEDIA                                
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   SCRMEDN(L'MEDNM),MEDNM  DISPLAY MEDIA NAME                       
*                                                                               
VKMEDX   DS    0H                                                               
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
VKPUB    DS    0H                                                               
*                                                                               
         LA    R2,SCRPUBH          VALIDATE PUB ENTRY                           
         BRAS  RE,MYPUBVAL                                                      
*                                                                               
         MVC   SCRPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    SCRPUBNH+6,X'80'                                                 
         MVC   SCRPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    SCRPUBZH+6,X'80'                                                 
*                                                                               
VKPUBX   DS    0H                                                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
VKCLT    DS    0H                                                               
*                                                                               
         LA    R2,SCRCLTH          CLIENT                                       
         MVC   CLTNM,SPACES                                                     
         MVC   QCLT,=X'FFFFFF'     SET TO DEFAULT                               
*                                                                               
         CLI   5(R2),0             NO ENTRY EQUIVALENT TO ALL CLIENTS           
         BE    VKCLT20                                                          
         CLC   =C'ALL',8(R2)                                                    
         BE    VKCLT20                                                          
*                                                                               
         GOTO1 VALICLT             VALIDATE CLIENT ENTRY                        
*                                                                               
VKCLT20  MVC   SCRCLTN(L'CLTNM),CLTNM  DISPLAY CLT NAME                         
         OI    SCRCLTNH+6,X'80'                                                 
*                                                                               
         MVC   QPRD,=X'FFFFFF'     SET TO DEFAULT                               
*                                                                               
VKCLTX   DS    0H                                                               
*                                                                               
*        VALIDATE PRODUCT                                                       
*                                                                               
VKPRD    DS    0H                                                               
*                                                                               
         LA    R2,SCRPRDH          PRODUCT                                      
         MVC   PRDNM,SPACES                                                     
         MVC   QPRD,=X'FFFFFF'     SET TO DEFAULT                               
*                                                                               
         CLI   5(R2),0             NO ENTRY EQUIVALENT TO ALL PRODUCTS          
         BE    VKPRD20                                                          
         CLC   =C'ALL',8(R2)                                                    
         BE    VKPRD20                                                          
*                                                                               
         CLC   QCLT,=X'FFFFFF'     CLIENT REQUIRED                              
         BNE   VKPRD10                                                          
*                                                                               
         LA    R2,SCRCLTH          POINT TO CLIENT FIELD                        
         B     VKCLTER                                                          
*                                                                               
VKPRD10  DS    0H                                                               
*                                                                               
         GOTO1 VALIPRD             VALIDATE PRODUCT ENTRY                       
*                                                                               
VKPRD20  MVC   SCRPRDN(L'PRDNM),PRDNM  DISPLAY PRD NAME                         
         OI    SCRPRDNH+6,X'80'    RE-TRANSMIT                                  
*                                                                               
VKPRDX   DS    0H                                                               
*                                                                               
*        BUILD STARTING KEY                                                     
*                                                                               
VKKEY    DS    0H                                                               
*                                                                               
         MVC   SYSDIR,=CL8'PUBDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PUBFILE' SET FILE                                    
*                                                                               
         MVI   USEIONUM,1          USE IOAREA 1 FOR VALIDATION                  
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING CCVRECD,R4          ESTABLISH VENDOR CONTACT LIST KEY            
*                                                                               
         MVC   CCVKMED,QMED        MEDIA                                        
         MVC   CCVKPUB,BPUB                                                     
         MVC   CCVKAGY,AGENCY      AGENCY                                       
         MVI   CCVKRCD,CCVKRCDQ    RECORD TYPE                                  
         MVC   CCVKCLT,QCLT        CLIENT                                       
         MVC   CCVKPRD,QPRD        PRODUCT                                      
*                                                                               
* * * * * * * * BELOW NOT IN USE A/O 03/13/09 * * * * * * * * * * * * *         
******   MVC   CCVKPID,QPID        USER ID                                      
* * * * * * * * ABOVE NOT IN USE A/O 03/13/09 * * * * * * * * * * * * *         
*                                                                               
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
*                                                                               
         CLC   TWAKEYSV(L'CCVKEY),KEY    CHECK FOR NEWKEY                       
         BE    *+8                                                              
         MVI   NEWKEY,C'Y'                                                      
*                                                                               
         B     VKX                                                              
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - VKL'                       
***********************************************************************         
*                                                                     *         
*        VALIDATE LIST KEY                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKL      DS    0H                                                               
*                                                                               
         MVC   SYSDIR,=CL8'PRTDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PRTFILE' SET FILE                                    
         MVI   USEIONUM,2          USE IOAREA 2 FOR VALIDATION                  
*                                                                               
*        VALIDATE MEDIA                                                         
*                                                                               
VKLMED   DS    0H                                                               
*                                                                               
         MVC   SCLMEDN,SPACES      INIT                                         
         MVC   MEDNM,SPACES                                                     
         OI    SCLMEDNH+6,X'80'    FORCE RE-TRANSMISSION                        
*                                                                               
         LA    R2,SCLMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   SCLMEDN(L'MEDNM),MEDNM   DISPLAY MEDIA NAME                      
*                                                                               
VKLMEDX  DS    0H                                                               
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
VKLPUB   DS    0H                                                               
*                                                                               
*        INIT PUB FIELDS                                                        
*                                                                               
         LA    R2,SCLPUBH          PUB                                          
         XC    BPUB,BPUB                                                        
*                                                                               
         XC    SCLPUBN,SCLPUBN                                                  
         OI    SCLPUBNH+6,X'80'                                                 
*                                                                               
         XC    SCLPUBZ,SCLPUBZ                                                  
         OI    SCLPUBZH+6,X'80'                                                 
*                                                                               
         CLI   5(R2),0             NO ENTRY OKAY                                
         BE    VKLPUBX                                                          
*                                                                               
         BRAS  RE,MYPUBVAL                                                      
*                                                                               
         MVC   SCLPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    SCLPUBNH+6,X'80'                                                 
*                                                                               
         MVC   SCLPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    SCLPUBZH+6,X'80'                                                 
*                                                                               
VKLPUBX  DS    0H                                                               
*                                                                               
*        VALIDATE CLIENT                                                        
*                                                                               
VKLCLT   DS    0H                                                               
*                                                                               
         XC    QCLT,QCLT           FOR LISTING CLEAR QCLT                       
         XC    SCLCLTN,SCLCLTN                                                  
         OI    SCLCLTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SCLCLTH          CLIENT                                       
*                                                                               
         CLI   5(R2),0             NO ENTRY - NO FILTERING                      
         BE    VKLCLTX             DONE WITH CLIENT                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VKLCLT30                                                         
*                                                                               
         MVC   QCLT,=X'FFFFFF'     SET FOR CLIENT 'ALL'                         
*                                                                               
         B     VKLCLTX                                                          
*                                                                               
VKLCLT30 GOTO1 VALICLT             VALIDATE CLIENT                              
*                                                                               
         MVC   SCLCLTN,CLTNM                                                    
         OI    SCLCLTNH+6,X'80'                                                 
*                                                                               
VKLCLTX  DS    0H                                                               
*                                                                               
*        VALIDATE PRODUCT                                                       
*                                                                               
VKLPRD   DS    0H                                                               
*                                                                               
         XC    QPRD,QPRD           FOR LISTING CLEAR QPRD                       
         XC    SCLPRDN,SCLPRDN                                                  
         OI    SCLPRDNH+6,X'80'                                                 
*                                                                               
         CLC   QCLT,=X'FFFFFF'     CLIENT ENTERED AS "ALL" ?                    
         BNE   VKLPRD10            NO                                           
         CLC   SCLPRD(3),=C'ALL'   PRODUCT ENTERED AS "ALL" ?                   
         BE    VKLPRD20            YES - FINISH                                 
         MVC   SCLPRD(3),=C'ALL'   IF CLIENT IS "ALL" PRODUCT MUST BE           
         OI    SCLPRDH+6,X'80'       "ALL" SO SET PRODUCT FIELD TO              
         B     VKLPRD20              "ALL" AND FINISH                           
*                                                                               
VKLPRD10 DS    0H                  CLIENT NOT ENTERED AS "ALL"                  
         LA    R2,SCLPRDH          POINT TO PRODUCT FIELD                       
         CLI   5(R2),0             NO ENTRY EQUIVALENT TO ALL PRODUCTS          
         BE    VKLPRDX             SO DONE                                      
*                                                                               
         CLC   SCLPRD(3),=C'ALL'   PRODUCT ENTERED AS "ALL" ?                   
         BE    VKLPRD20            YES - FINISH                                 
*                                  PRODUCT ENTERED AND NOT "ALL"                
         OC    QCLT,QCLT           CLIENT ENTERED ?                             
         BNZ   VKLPRD30            YES - GO VALIDATE PRODUCT                    
*                                                                               
         LA    R2,SCLCLTH          POINT TO CLIENT FIELD                        
         B     VKCLTER             CLIENT REQUIRED FOR SPECIFIC PRODUCT         
*                                                                               
VKLPRD20 DS    0H                  CLIENT OR PRODUCT ENTERED AS "ALL"           
         MVC   QPRD,=X'FFFFFF'     SET FOR PRODUCT 'ALL'                        
         B     VKLPRDX                                                          
*                                                                               
VKLPRD30 GOTO1 VALIPRD             VALIDATE PRODUCT                             
*                                                                               
         MVC   SCLPRDN,PRDNM                                                    
         OI    SCLPRDNH+6,X'80'                                                 
*                                                                               
VKLPRDX  DS    0H                                                               
*                                                                               
         CLI   SCLMED,C'O'                                                      
         BNE   *+12                                                             
         MVI   RCSUBPRG,1          OUTDOOR                                      
         B     *+8                                                              
         MVI   RCSUBPRG,0          ALL OTHER MEDIA                              
*                                                                               
         MVC   SYSDIR,=CL8'PUBDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PUBFILE' SET FILE                                    
         MVI   USEIONUM,1          USE IOAREA 1 FOR VALIDATION                  
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
VKX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
NEEDCLT  EQU   85             SPECIFIC CLIENT ENTRY REQUIRED (SECURITY)         
*                                                                               
VKCLTER  DS    0H                                                               
         MVI   ERROR,NEEDCLT       SECURITY - CLIENT REQUIRED                   
         B     VKERR                                                            
*                                                                               
VKERR    DS    0H                                                               
         GOTOR ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - MYPUBVAL'                  
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
         B     VKPUBERR            IF NOT THE MUST HAVE PUB                     
*                                                                               
VKPUB5   DS    0H                                                               
         CLI   5(R2),3                                                          
         BNE   VKPUB7                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VKPUB7                                                           
         MVC   BPUB(6),=6X'FF'                                                  
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
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         B     VKPUB30                                                          
         DROP  R3                                                               
*                                                                               
VKPUB10  DS    0H                                                               
         LR    R2,R4                                                            
         MVI   ERROR,INVALID                                                    
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,SCANBLK)                                    
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
         GOTO1 VALIPUB                                                          
         MVC   AIO,AIO1            USE IOAREA2                                  
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
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - DK'                        
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
         L     R6,AIO              ESTABLISH VENDOR CONTACT LIST RECORD         
         USING CCVREC,R6                                                        
*                                                                               
*        DISPLAY MEDIA                                                          
*                                                                               
*                                                                               
DKMED    DS    0H                                                               
*                                                                               
         XC    SCRMED,SCRMED                                                    
         MVC   SCRMED(L'CCVKMED),CCVKMED   MEDIA                                
         MVI   SCRMEDH+5,L'CCVKMED SET INPUT LENGTH                             
         OI    SCRMEDH+6,FOUTTRN   TRANSMIT                                     
*                                                                               
DKMEDX   DS    0H                                                               
*                                                                               
*        DISPLAY PUB                                                            
*                                                                               
DKPUB    DS    0H                                                               
*                                                                               
         MVI   NEWKEY,C'Y'         FORCE RE-DISPLAY FROM START OF PUB           
*                                                                               
         XC    SCRPUB,SCRPUB                                                    
         OI    SCRPUBH+6,FOUTTRN   TRANSMIT                                     
*                                                                               
         CLI   CCVKPUB,X'FF'       "ALL" PUBS ?                                 
         BNE   DKPUBED             NO                                           
         MVC   SCRPUB(3),=C'ALL'                                                
         MVI   SCRPUBH+5,3         SET INPUT LENGTH                             
         B     DKPUBX                                                           
*                                                                               
DKPUBED  DS    0H                                                               
         GOTO1 =V(PUBEDIT),DMCB,CCVKPUB,(C'S',SCRPUB),RR=RELO                   
*                                                                               
         LA    R0,L'SCRPUB         MAX LENGTH OF PUB NUMBER                     
         LA    RF,SCRPUB+L'SCRPUB-1 LAST OF PUB FIELD                           
*                                                                               
         CLI   0(RF),C' '          FIND LAST OF PUB ID                          
         BH    *+14                                                             
         SHI   RF,1                BACK UP A BYTE                               
         BCT   R0,*-12                                                          
         DC    H'0'                NO PUB NUMBER                                
*                                                                               
         STC   R0,SCRPUBH+5        SET INPUT LENGTH                             
*                                                                               
DKPUBX   DS    0H                                                               
*                                                                               
*        DISPLAY CLIENT                                                         
*                                                                               
DKCLT    DS    0H                                                               
*                                                                               
         XC    SCRCLT,SCRCLT                                                    
         MVC   SCRCLT(L'CCVKCLT),CCVKCLT      CLIENT                            
         MVI   SCRCLTH+5,L'CCVKCLT SET INPUT LENGTH                             
*                                                                               
         CLI   SCRCLT+2,C' '       IF 2 CHARACTER CLIENT                        
         BH    *+8                                                              
         MVI   SCRCLTH+5,2            ADJUST INPUT LENGTH                       
*                                                                               
         OI    SCRCLTH+6,FOUTTRN   TRANSMIT                                     
*                                                                               
         CLC   CCVKCLT,=X'FFFFFF'  IF ALL CLIENTS                               
         BNE   DKCLTX                                                           
*                                                                               
         MVC   CLTNM,SPACES                                                     
         MVC   SCRCLT(3),=C'ALL'                                                
         MVI   SCRCLTH+5,3         LENGTH                                       
*                                                                               
DKCLTX   DS    0H                                                               
*                                                                               
*        DISPLAY PRODUCT                                                        
*                                                                               
DKPRD    DS    0H                                                               
*                                                                               
         XC    SCRPRD,SCRPRD                                                    
         MVC   SCRPRD(L'CCVKPRD),CCVKPRD      PRODUCT                           
         MVI   SCRPRDH+5,L'CCVKPRD SET INPUT LENGTH                             
*                                                                               
         CLI   SCRPRD+2,C' '       IF 2 CHARACTER PRODUCT                       
         BH    *+8                                                              
         MVI   SCRPRDH+5,2            ADJUST INPUT LENGTH                       
*                                                                               
         OI    SCRPRDH+6,FOUTTRN   TRANSMIT                                     
*                                                                               
         CLC   CCVKPRD,=X'FFFFFF'  IF ALL PRODUCTS                              
         BNE   DKPRDX                                                           
*                                                                               
         MVC   PRDNM,SPACES                                                     
         MVC   SCRPRD(3),=C'ALL'                                                
         MVI   SCRPRDH+5,3         LENGTH                                       
*                                                                               
DKPRDX   DS    0H                                                               
*                                                                               
         BRAS  RE,VK               VALIDATE KEY                                 
*                                                                               
DKX      DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - VR'                        
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
         MVC   SYSDIR,=CL8'PUBDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PUBFILE' SET FILE                                    
*                                                                               
         XC    SVACHGS,SVACHGS     INIT CHANGE INDICATORS                       
*                                                                               
***********************************************************************         
*                                                                     *         
*        VALIDATE BASE E-MAIL ADDRESS                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREBAS   DS    0H                                                               
*                                                                               
         XC    SVEBASE,SVEBASE     INIT WORK AREA                               
         LA    R3,SVEBASE                                                       
         USING CCVEBASD,R3         ESTABLISH BASE ADDRESS ELEMENT               
*                                                                               
         MVI   CCVEBCDE,CCVEBIDQ   SET ELEMENT CODE                             
         MVI   CCVEBLEN,CCVEBLQ    SET BASIC ELEMENT LENGTH                     
*                                                                               
         LA    R2,SCRBASH          POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         CLI   FLDILEN,0           IF NO INPUT IN BASE FIELD                    
         BE    VREBASOK               DONE                                      
*                                                                               
         TM    FLDIIND,FINPTHIS    IF FIELD INPUT THIS TIME                     
         BNO   *+8                                                              
         OI    SVACH1,CCVAEBCH        EBASE CHANGED                             
*                                                                               
*        ONLY ONE @ ALLOWED AND ONLY IN FIRST POSITION                          
*                                                                               
         LA    R0,C'@'             SET SEARCH CHARACTER                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,FLDILEN          INPUT LENGTH                                 
*                                                                               
         CHI   RE,1                SKIP IF 1-BYTE                               
         BE    VREBAS10                                                         
*                                                                               
         LA    RE,FLDDATA(RE)      END OF INPUT                                 
*                                                                               
         LA    RF,FLDDATA+1        BYPASS FIRST POSITION                        
*                                                                               
         SRST  RE,RF               SEARCH FOR @ SIGN                            
         BL    VREBASNV              FOUND - INVALID DOMAIN NAME                
*                                                                               
*        NO SPACES ALLOWED IN DOMAIN NAME                                       
*                                                                               
         LA    R0,C' '             SET SEARCH CHARACTER                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,FLDILEN          INPUT LENGTH                                 
*                                                                               
         LA    RE,FLDDATA(RE)      END OF INPUT                                 
*                                                                               
         LA    RF,FLDDATA+1        BYPASS FIRST POSITION                        
*                                                                               
         SRST  RE,RF               SEARCH FOR INTERNAL SPACE                    
         BL    VREBASNV              FOUND - INVALID DOMAIN NAME                
*                                                                               
VREBAS10 DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDILEN          INPUT LENGTH                                 
         LA    R1,FLDDATA          START OF DOMAIN NAME                         
*                                                                               
         CLI   0(R1),C'@'          DROP LEADING @ SIGN                          
         BNE   *+16                                                             
         AHI   R1,1                BUMP INPUT POINTER                           
         SHI   RF,1                DECREMENT LENGTH                             
         BZ    VREBASNV            @ ONLY DOMAIN NAME                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CCVEBASE(0),0(R1)   SAVE DOMAIN NAME                             
*                                                                               
         LR    R0,RF               SAVE EXECUTE LENGTH                          
*                                                                               
         AHI   RF,CCVEBLQ+1        CALCULATE TRUE ELM LENGTH                    
         STC   RF,CCVEBLEN         SET ELEMENT LENGTH                           
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LR    RF,R0               RESTORE EXECUTE LENGTH                       
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),CCVEBASE RE-DIPLAY INPUT                              
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
VREBASOK DS    0H                                                               
*                                                                               
*        ON ACTION ADD                                                          
*        ADD E-BASE ELEMENT TO RECORD TO MAKE SURE SOMETHING IS THERE           
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF NOT ACTION ADD                       
         BNE   VREBAS30                                                         
*                                                                               
         LA    R3,SVEBASE          POINT TO SAVED E-BASE ELEMENT                
         USING CCVEBASD,R3         ESTABLISH E-BASE ELEMENT                     
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R6,CCVFIRST-CCVREC(R6)    POINT TO FIRST ELEMENT IN REC          
         GOTO1 VRECUP,DMCB,,(R3),(C'R',(R6))  ADD ELEMENT                       
*                                                                               
VREBAS30 DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
VREBASX  DS    0H                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        VALIDATE DETAIL INPUT VIA LINUP                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LA    RE,LSVTAB                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE SAVE COPY OF LINUP SAVE TABLE           
*                                  TO RESTORE AFTER VALIDATION ERRORS           
         BRAS  RE,LINSET           LINUP INTERFACE                              
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
VREBASNV DS    0H                  INVALID DOMAIN NAME                          
         LHI   RF,PWEEBSNV         SET ERROR CODE                               
*                                                                               
VRERR    DS    0H                                                               
         STCM  RF,3,ERROR2CD       SET ERROR EQUATE                             
*                                                                               
VRERRX   DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2cd IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         BE    *+10                                                             
         MVC   ERROR2CD+1(1),ERROR      PUT IN NEW STYLE                        
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVC   GTMSGNO,ERROR2CD    MESSAGE NUMBER                               
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - DR'                        
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
         MVC   SYSDIR,=CL8'PUBDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PUBFILE' SET FILE                                    
*                                                                               
***********************************************************************         
*                                                                     *         
*        DISPLAY  BASE E-MAIL ADDRESS                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DREBAS   DS    0H                                                               
*                                                                               
         L     R6,AIO              POINT TO RECORD FOR DISPLAY                  
         USING CCVEBASD,R6         ESTABLISH AS E-MAIL BASE ELEMENT             
*                                                                               
         LA    R2,SCRBASH          POINT TO FIELD ON SCREEN                     
         USING FLDHDRD,R2          ESTABLISH BASIC FIELD                        
*                                                                               
         XC    SCRBAS,SCRBAS       INIT OUTPUT                                  
*                                                                               
         MVI   ELCODE,CCVEBIDQ     LOOK FOR E-BASE ELM                          
         BRAS  RE,GETEL            FIND IN RECORD                               
         BNE   DREBASOK            SKIP IF NONE FOUND                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CCVEBLEN         GET ELEMENT LENGTH                           
         SHI   RF,CCVEBLQ          SUBTRACT BASIC ELM LENGTH                    
         BNP   DREBASOK                                                         
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),CCVEBASE    DISPLAY E-MAIL BASE                       
*                                                                               
DREBASOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
DREBASX  DS    0H                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  DISPLAY DETAILS VIA LINUP                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         BRAS  RE,LINSET           INTERFACE WITH LINUP                         
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
*                                                                               
         MVI   GTMSGNO1,3          MESSAGE NUMBER (1 BYTE)                      
         MVI   GTMSYS,X'FF'        SYSTEM ZERO (GENERAL) MESSAGES               
*                                                                               
         DROP  R1                                                               
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - RDEL'                      
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
         MVC   SYSDIR,=CL8'PUBDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PUBFILE' SET FILE                                    
*                                                                               
*        ADD ACTIVITY ELEMENT                                                   
*                                                                               
         XC    SVACTELM,SVACTELM   INIT WORK AREA                               
         LA    R3,SVACTELM                                                      
         USING CCVACTHD,R3         ESTABLISH ACTIVITY ELM                       
*                                                                               
         MVI   CCVACDE,CCVAIDQ     SET ELEMENT CODE                             
         MVI   CCVALEN,CCVACTLQ    SET ELEMENT LENGTH                           
*                                                                               
         GOTO1 DATCON,DUB,(5,0),(3,CCVADTE) SET TODAY'S DATE                    
*                                                                               
         MVC   CCVAPID,SVPID       SET CHANGER'S PID                            
         MVC   CCVASCID,SVSCID     SET CHANGER'S SECURITY PID                   
*                                                                               
         OI    SVACH1,CCVADEL      SET RECORD DELETED                           
*                                                                               
         BRAS  RE,ACTPUT           ADD ACTIVITY ELEMENT                         
*                                                                               
RDELX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - RR'                        
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
         MVC   SYSDIR,=CL8'PUBDIR'  SET DIRECTORY                               
         MVC   SYSFIL,=CL8'PUBFILE' SET FILE                                    
*                                                                               
*        ADD ACTIVITY ELEMENT                                                   
*                                                                               
         OI    SVACH1,CCVARES      SET RECORD RESTORED                          
*                                                                               
         BRAS  RE,ACTPUT           ADD ACTIVITY ELEMENT                         
*                                                                               
RRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - LR'                        
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
         LA    R6,KEY              ESTABLISH RECORD KEY                         
         USING CCVREC,R6                                                        
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR1STX              KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
*        BUILD STARTING KEY                                                     
*                                                                               
         MVC   CCVKMED,QMED        MEDIA CODE                                   
*                                                                               
         CLI   SCLPUBH+5,0         IF PUB ENTERED                               
         BE    *+10                                                             
         MVC   CCVKPUB,BPUB                     PUB                             
*                                                                               
         MVC   CCVKAGY,AGENCY      AGENCY                                       
         MVI   CCVKRCD,CCVKRCDQ    TYPE                                         
*                                                                               
         MVC   CCVKCLT,QCLT           CLIENT                                    
*                                                                               
         MVC   CCVKPRD,QPRD           PRODUCT                                   
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD                            
*                                                                               
LR1STX   DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1            USE AIO1                                     
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
*                                                                               
         CLC   CCVKEY(CCVKPUB-CCVKEY),KEYSAVE  DONE AT END OF MEDIA             
         BNE   LRDONE                                                           
*                                                                               
         TM    CCVDCNTL,CCVCDELQ   SKIP DELETED RECORDS                         
         BO    LRCONT2                                                          
*                                                                               
*        FILTER ON PUB                                                          
*                                                                               
LRPUB    DS    0H                                                               
*                                                                               
         OC    BPUB,BPUB          IF PUB GIVEN                                  
         BZ    LRPUBX                                                           
*                                                                               
         CLI   BPUB,X'FF'          "ALL" PUBS ENTERED ?                         
         BE    LRPUBW              YES                                          
*                                                                               
         OC    BPUB+4(2),BPUB+4    ANY ZONE/EDTN ?                              
         BNZ   LRPUBW              YES                                          
*                                                                               
         CLC   CCVKPBCD,BPUB       FILTER ON BASE PUB ONLY                      
         BE    LRPUBX                                                           
*                                                                               
LRPUBW   DS    0H                                                               
*                                                                               
         CLC   CCVKPUB,BPUB        FILTER ON WHOLE PUB                          
         BNE   LRDONE                                                           
*                                                                               
LRPUBX   DS    0H                                                               
*                                                                               
         CLC   CCVKAGY,AGENCY      FILTER ON AGENCY                             
         BNE   LRCONT2                                                          
*                                                                               
         CLI   CCVKRCD,CCVKRCDQ    FILTER ON RECORD CODE                        
         BNE   LRCONT2                                                          
*                                                                               
         OC    QCLT,QCLT           CLIENT GIVEN ?                               
         BZ    LRCLTX              NO                                           
         CLC   CCVKCLT,QCLT                                                     
         BNE   LRCONT2             FILTER ON CLIENT                             
*                                                                               
LRCLTX   DS    0H                                                               
         OC    QPRD,QPRD           PRODUCT GIVEN ?                              
         BZ    LRPRDX              NO                                           
         CLC   CCVKPRD,QPRD                                                     
         BNE   LRCONT2             FILTER ON PRODUCT                            
*                                                                               
LRPRDX   DS    0H                                                               
*                                                                               
         GOTO1 GETREC              GET CONTACT LIST RECORD                      
*                                                                               
         MVC   MYDSKADD,DMDSKADD   SAVE D/A FOR LIST                            
*                                                                               
         L     R6,AIO              POINT TO FOUND RECORD                        
*                                                                               
         USING LISTD,R5            ESTABLISH LIST DISPLAY                       
*                                                                               
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
*                                                                               
*        DISPLAY PUB AND PUB NAME                                               
*                                                                               
         CLI   CCVKPUB,X'FF'                                                    
         BNE   LRPRDX4                                                          
         MVC   LPUB(3),=C'ALL'                                                  
         MVC   LPUBN(8),=C'ALL PUBS'                                            
         B     LRPRDXX                                                          
*                                                                               
LRPRDX4  DS    0H                                                               
         GOTO1 =V(PUBEDIT),DMCB,CCVKPUB,(C'S',LPUB),RR=RELO                     
*                                                                               
         MVC   MYPUB,CCVKPUB                                                    
*                                                                               
         BRAS  RE,MYVPUB                                                        
*                                                                               
         MVC   LPUBN,PUBNM         DISPLAY PUB NAME                             
*                                                                               
LRPRDXX  DS    0H                                                               
*                                                                               
*        DISPLAY CLIENT                                                         
*                                                                               
         MVC   LCLT,CCVKCLT        CLIENT                                       
*                                                                               
         CLC   CCVKCLT,=X'FFFFFF'                                               
         BNE   *+10                                                             
         MVC   LCLT,=C'ALL'                                                     
*                                                                               
*        DISPLAY PRODUCT                                                        
*                                                                               
         MVC   LPRD,CCVKPRD        PRODUCT                                      
*                                                                               
         CLC   CCVKPRD,=X'FFFFFF'                                               
         BNE   *+10                                                             
         MVC   LPRD,=C'ALL'                                                     
*                                                                               
         CLI   MODE,PRINTREP       IF NOT PRINTING REPORT                       
         BNE   LRLRCONT               GO TO NEXT RECORD                         
*                                                                               
         MVC   P1,SPACES                                                        
         LA    R4,P1               USE P LINES                                  
         USING PLINED,R4           ESTABLIS PRINTED LINE                        
*                                                                               
*        PRINT KEY FIELDS                                                       
*                                                                               
         MVC   PPUB,LPUB           PUB CODE                                     
         MVC   PPUBN,LPUBN         PUB NAME                                     
         MVC   PCLT,LCLT           CLIENT CODE                                  
         MVC   PPRD,LPRD           PRODUCT CODE                                 
*                                                                               
*        FIND DOMAIN NAME                                                       
*                                                                               
         XC    SAVEBASE,SAVEBASE   INIT SAVEAREA                                
         XC    SAVEBASL,SAVEBASL   INIT EBASE LENGTH                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CCVEBIDQ     LOOK FOR E-BASE ELM                          
         BRAS  RE,GETEL            FIND IN RECORD                               
         BNE   LREBASOK            SKIP IF NONE FOUND                           
*                                                                               
         USING CCVEBASD,R6         ESTABLISH DOMAIN ELEMENT                     
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CCVEBLEN         GET ELEMENT LENGTH                           
         SHI   RF,CCVEBLQ          SUBTRACT BASIC ELM LENGTH                    
         BNP   LREBASOK                                                         
         STC   RF,SAVEBASL         SAVE DOMAIN NAME LENGTH                      
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SAVEBASE(0),CCVEBASE    DISPLAY E-MAIL BASE                      
*                                                                               
LREBASOK DS    0H                                                               
*                                                                               
*        PRINT ALL CONTACTS IN RECORD                                           
*                                                                               
         USING CCVLSTD,R6                                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CCVLIDQ      LOOK FOR LIST ELMS                           
         BRAS  RE,GETEL            FIND FIRST IN RECORD                         
         BE    LRPRLOOP            ELEMENT FOUND                                
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE- WILL FORCE SPACING               
*                                     LINE                                      
         B     LRPRDONE                                                         
*                                                                               
LRPRLOOP DS    0H                                                               
*                                                                               
         BNE   LRPRDONE            END OF ELEMENTS                              
*                                                                               
*        PRINT CONTACT NAME                                                     
*                                                                               
         MVC   PCNAME,CCVLNAME     PRINT CONTACT NAME                           
*                                                                               
*        PRINT CONTACT ADDRESS                                                  
*                                                                               
LRADR    DS    0H                                                               
*                                                                               
*        FAX ADDRESSES ARE EXACTLY 10 DIGITS                                    
*        DISPLAY AS NNN-NNN-NNNN                                                
*                                                                               
LRFAX    DS    0H                                                               
*                                                                               
         CLI   CCVLTYPE,0          SKIP IF NOT FAX NUMBER                       
         BE    *+8                                                              
         CLI   CCVLTYPE,C'F'       SKIP IF NOT FAX NUMBER                       
         BNE   LRFAXN                                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CCVLLEN        GET FAX# LENGTH                              
         SHI   RF,CCVLSTLQ         DECREMENT BY BASIC LENGTH                    
*                                                                               
         LA    R1,CCVLADDR         START OF FAX #                               
*                                                                               
         CLI   0(R1),1             IF NUMBER STARTS WITH 1                      
         BNE   *+12                                                             
         LA    R1,1(R1)               BUMP POINTER                              
         SHI   RF,1                   DECREMENT LENGTH BY 1                     
*                                                                               
         CHI   RF,10               IF FAX # HAS LENGTH 10                       
         BNE   LRFAXN                 FORMAT AS XXX-XXX-XXXX                    
*                                                                               
         MVC   PCADDR(3),0(R1)     AREA CODE                                    
         MVI   PCADDR+3,C'-'                                                    
         MVC   PCADDR+4(3),3(R1)   EXCHANGE                                     
         MVI   PCADDR+7,C'-'                                                    
         MVC   PCADDR+8(4),6(R1)   NUMBER                                       
*                                                                               
         B     LRADRX                                                           
*                                                                               
LRFAXN   DS    0H                                                               
*                                                                               
*        PRINT E-MAIL/FAX NUMBER                                                
*                                                                               
         LA    R3,L'PCADDR         MAKE LENGTH OF PRINT AREA                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CCVLLEN          GET ELEMENT LENGTH                           
         SHI   RF,CCVLSTLQ         DECREMENT BY BASIC LENGTH                    
*                                                                               
         CR    RF,R3               MAKE SURE THERE IS ROOM                      
         BNH   *+6                                                              
         LR    RF,R3                                                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PCADDR(0),CCVLADDR  DISPLAY ADDRESS                              
*                                                                               
         CLI   CCVLTYPE,C'E'       SKIP IF NOT AN E-MAIL                        
         BNE   LRADRX                                                           
*                                                                               
         CLI   SAVEBASL,0          SKIP IF NO DOMAIN NAME                       
         BE    LRADRX                                                           
*                                                                               
         SR    R3,RF               CALCULATE REMAINING LENGTH                   
         SHI   R3,1                                                             
         BNP   LRADRX                 NO MORE ROOM                              
*                                                                               
         LA    R1,PCADDR           START OF EMAIL ADDRESS                       
         LA    R1,1(RF,R1)         POINT TO NEXT AVAILABLE SPACE                
*                                                                               
         LA    R0,C'@'             SET TO SEARCH FOR @                          
         LA    RE,PCADDR           START OF E-MAIL ADDRESS                      
*                                                                               
         SRST  R1,RE               SEARCH ADDRESS FOR @                         
         BL    LRADRX              FOUND SO SKIP ADDING DOMAIN NAME             
*                                                                               
         CLI   SAVEBASE,C'@'       SKIP IF BASE HAS @                           
         BE    *+20                                                             
         MVI   0(R1),C'@'             PUT IN @                                  
         LA    R1,1(R1)               BUMP OUTPUT POINTER                       
         SHI   R3,1                DECREMENT REMAING LENGTH                     
         BNP   LRADRX                 NO MORE ROOM                              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,SAVEBASL       GET DOMAIN NAME LENGTH                       
*                                                                               
         CR    RF,R3               MAKE SURE THERE IS ROOM                      
         BNH   *+6                                                              
         LR    RF,R3                                                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SAVEBASE    ADD ON DOMAIN NAME                           
*                                                                               
LRADRX   DS    0H                                                               
*                                                                               
*              PRINT FYI                                                        
*                                                                               
LRFYI    DS    0H                                                               
*                                                                               
         CLI   CCVLTYP2,C'Y'       IF FYI VERSION                               
         BNE   *+10                                                             
         MVC   PFYI,=C'FYI'           PRINT 'FYI'LL)                            
*                                                                               
LRFYIX   DS    0H                                                               
*                                                                               
*        PRINT SUPPRESS COST                                                    
*                                                                               
LRSUP    DS    0H                                                               
*                                                                               
         MVI   PWOCOSTS,C'Y'       DEFAUT TO PRINT COSTS                        
*                                                                               
         CLI   CCVLSUP,C'Y'        IF W/O COSTS VERSION                         
         BNE   *+10                                                             
         MVC   PWOCOSTS,=C'N'         PRINT 'N'                                 
*                                                                               
LRSUPX  DS    0H                                                                
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT  LINE                                  
*                                                                               
LRPRCONT DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT IN LIST                            
*                                                                               
         B     LRPRLOOP                                                         
*                                                                               
LRPRDONE DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   SKIP A LINE                                  
*                                                                               
         B     LRCONT              GO TO SEQ READ                               
*                                                                               
LRLRCONT DS    0H                  LIST NEXT RECORD                             
*                                                                               
         MVC   DMDSKADD,MYDSKADD   RESTORE D/A                                  
*                                                                               
         GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
LRCONT   DS    0H                                                               
*                                                                               
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
*                                                                               
LRCONT2  DS    0H                                                               
*                                                                               
         LA    R6,KEY              MUST RESET R6 TO KEY                         
*                                                                               
         GOTO1 SEQ                                                              
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
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - MYVPUB'                    
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
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VPNO                                                             
*                                                                               
         MVC   AIO,AIO2            USE AIO2                                     
*                                                                               
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
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
         GOTO1 HIGH                                                             
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
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - MYVCLT'                    
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
         MVC   KEY+4(3),SCRCLT                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PRTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(10),KEYSAVE                                                  
         BNE   VCNO                                                             
*                                                                               
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME,=CL8'PRTFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         MVC   CLTNM,PCLTNAME                                                   
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         XIT1                                                                   
*                                                                               
VCNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - PR'                        
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
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - HOOK'                      
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
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - LINSET'                    
**********************************************************************          
*   LINUP INTERFACE                                                  *          
*     - BUILD LUBLK, CREATE ELEMENT TABLE, CALL LINUP                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         DS    0D                                                               
LINSET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         XC    SVELTKEY,SVELTKEY   INIT WORKAREA                                
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
         MVI   LUNFLDS,NFLDS           FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,SCRNAM1H         A(FIRST FIELD)                               
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
*                                                                               
LS04     DS    0H                                                               
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
*                                                                               
         ZIC   R0,LUNFLDS          GET NUMBER OF FIELDS ON LINE                 
*                                                                               
         BAS   RE,BUMPU            BUMP TO START OF NEXT LINE                   
         BCT   R0,*-4                BY BUMPING THRU ALL FLDS ON LINE           
*                                                                               
         BCT   R4,LS04             ADD NEXT LINE TO LIST                        
*                                                                               
         MVI   LUSCROLL,LUHALFQ    SCROLL FACTOR OF A HALF IS DEFAULT           
*                                                                               
         CLI   SCRSCRL,C'P'        CHECK FOR PAGE SCROLL                        
         BE    *+8                                                              
         CLI   SCRSCRL,X'97'         LOWERCASE 'P'                              
         BNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   SCRSCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         BE    *+8                                                              
         CLI   SCRSCRL,X'88'            LOWERCASE 'H'                           
         BNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    SCRSCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         BNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,SCRSCRLH+5     FIELD INPUT LENGTH                           
         BZ    LS051               NO ENTRY                                     
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SCRSCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,LUSCROLL         PASS SCROLL AMOUNT                           
*                                                                               
LS051    DS    0H                                                               
*                                                                               
         CLI   PFAID,19            CHECK FOR UP KEY                             
         BE    *+8                                                              
         CLI   PFAID,7             CHECK FOR UP KEY                             
         BNE   *+12                                                             
         MVI   LUPFKEY,LUPFUPQ                                                  
         B     LSPFKX                                                           
*                                                                               
*                                                                               
         CLI   PFAID,20            CHECK FOR DOWN KEY                           
         BE    *+8                                                              
         CLI   PFAID,8             CHECK FOR DOWN KEY                           
         BNE   *+12                                                             
         MVI   LUPFKEY,LUPFDNQ                                                  
         B     LSPFKX                                                           
*                                                                               
*        CLI   LUPFKEY,0           IF VALID PFKEY HIT                           
*        BE    *+8                                                              
*        OI    GENSTAT2,RETEQSEL   RE-DISPLAY SAME SCREEN                       
*                                                                               
LSPFKX   DS    0H                                                               
*                                                                               
         CLI   MODE,VALREC         SET LINUP MODE                               
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPVALQ   VALIDATE                                     
*                                                                               
         CLI   MODE,DISPREC        SET LINUP MODE - DISPREC                     
         BE    *+8                                                              
         CLI   MODE,XRECADD        RE-DISPLAY AFTER ADD                         
         BE    *+8                                                              
         CLI   MODE,XRECPUT        RE-DISPLAY AFTER CHANGE                      
         BE    *+8                                                              
         CLI   MODE,XRECDEL        RE-DISPLAY AFTER DELETE                      
         BE    *+8                                                              
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    *+8                                                              
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    *+8                                                              
         CLI   MODE,XRECREST       RE-DISPLAY AFTER RESTORE                     
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPDSPQ   DISPLAY                                      
*                                                                               
         MVI   LUCNTL,LUBACKQ      WINDOW SUPPORTS BACKWARD SCROLLING           
*                                                                               
         TM    IPSTAT,LUWCURSQ     CHECK IF CURSOR FOUND YET                    
         BNO   *+8                 NO - NO CURSOR SELECT                        
         OI    LUCNTL,LUCURSQ      YES - MAKE CURSOR SENSITIVE                  
*                                                                               
         LA    RF,LINHOOK          PROCESSING ROUTINE                           
         ST    RF,LUHOOK                                                        
*                                                                               
         MVC   LUSVLEN,=AL2(LSVTABL) SAVED BYTES PER LINE                       
*                                                                               
         LA    RF,LSVTAB           LINUP SAVE AREA                              
         ST    RF,LUSVTAB                                                       
*                                                                               
*              BUILD TABLE OF ELEMENTS                                          
*                                                                               
         BRAS  RE,LSBLDTAB            BUILD ELEM TABLE                          
*                                                                               
         CLI   PFAID,4             INSERTING LINE                               
         BE    *+8                                                              
         CLI   PFAID,16                                                         
         BE    *+8                                                              
         CLI   PFAID,6             DELETING  LINE                               
         BE    *+8                                                              
         CLI   PFAID,18                                                         
         BNE   *+8                                                              
         BRAS  RE,PFKEYS                                                        
*                                                                               
*                                     FIRST LINE UP CALL                        
*                                     ------------------                        
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD     LINUP                                     
*                                                                               
         OC    IPSTAT,LUWSTAT      OR IN WINDOW INPUT STATUS                    
*                                                                               
         CLI   LUAPMODE,LUAPVALQ      TEST VALIDATING                           
         BNE   LSMOR                                                            
*                                                                               
         TM    IPSTAT,LUWVERRQ     CONTINUE IF NO ERRORS                        
         BNO   LS22                                                             
*                                  ELSE                                         
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LA    RE,LSVTAB                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE LINUP SAVE TABLE                     
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LSX                 AND DON'T UPDATE ELEMS                       
*                                                                               
LS22     DS    0H                                                               
*                                                                               
         TM    SCRBASH+4,FINPTHIS  IF EBASE INPUT THIS TIME                     
         BO    LS23                UPDATE RECORD                                
*                                  OR                                           
         CLI   DELFLAG,C'D'        IF ELEMENT TO BE DELETED                     
         BE    LS23                UPDATE RECORD                                
*                                  OR                                           
         TM    LUWSTAT,LUSNPVQ     UPDATE RECORD IF THERE WAS AT LEAST          
         BNO   *+8                 ONE NON-PREVIOUSLY VALIDATED FIELD           
         TM    LUWSTAT,LUSDATQ     AND DATA ENTERED IN SOME FIELD               
         BNO   LSWRTTB1                                                         
*                                                                               
LS23     DS    0H                                                               
*                                                                               
         BRAS  RE,LSWRTTAB         WRITES CHANGES TO RECORD                     
*                                                                               
         MVI   DELFLAG,0           CLEAR                                        
*                                                                               
         B     LSWRTTBX                                                         
*                                                                               
LSWRTTB1 DS    0H                                                               
*                                                                               
         OI    GENSTAT2,USMYOK+USGETTXT+STLCOK  USE OUR MSG AND GTTXT           
*                                                                               
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
*                                                                               
         MVI   GTMSGNO1,3          MESSAGE NUMBER (1 BYTE)                      
         MVI   GTMSYS,X'FF'        SYSTEM ZERO (GENERAL) MESSAGES               
*                                                                               
         DROP  R1                                                               
*                                                                               
LSWRTTBX DS    0H                                                               
*                                                                               
         TM    IPSTAT,LUSNPVQ      IF ALL PREVIOUSLY VALIDATED                  
         BO    LSNCHA                                                           
         CLI   ACTNUM,ACTCHA       AND ACTION IS CHANGE                         
         BNE   LSNCHA              THEN WANT TO RE-DISPLAY IN CASE OF           
*                                    NEED TO SCROLL                             
         TM    LUSTAT,LUCLEARQ     SKIP IF CLEAR COMMAND ISSUED                 
         BO    LSNCHA                                                           
*                                    NEED TO SCROLL                             
         MVI   LUAPMODE,LUAPDSPQ   SET FOR DISPLAY                              
         MVI   MODE,DISPREC        SET FOR DISPLAY RECORD                       
         MVI   LUWSTAT,0           RESET WINDOW STAT                            
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD SCROLL IF NEEDED                              
*                                                                               
LSNCHA   DS    0X                                                               
*                                                                               
LSMOR    DS    0X                  SET 'MORE' FIELDS                            
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOW                 CLEARED - LEAVE UPPER AS IS                
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   ONLY IF IN DISPLAY MODE                      
         BNE   LSMORX                                                           
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R2,SCRMOR1H         POINT TO FIRST MORE FIELD                    
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST TABLE ENTRY                   
         USING ELTABD,R4           ESTABLISH ENTRY                              
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT START AT START            
         BNH   *+10                OF TABLE THEN SET UP INDICATOR               
         MVC   FLD(2),=C'<<'                                                    
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSLOW    DS    0H                                                               
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R2,SCRMORLH         POINT TO LAST MORE FIELD                     
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOWOUT              CLEARED                                    
*                                                                               
         ZIC   RF,LUNLINS          GET NUMBER OF LINES ON SCREEN                
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MH    RF,=Y(LSVTABL)      GET INDEX                                    
         AR    R3,RF               POINT TO LAST ELEMENT IN TABLE               
         L     R4,ELTLAST          POINT TO LAST ELEMENT IN TABLE               
*                                                                               
         OC    LSVKEY,LSVKEY       NULLS INDICATE END OF TABLE ALREADY          
         BZ    LSLOWOUT            ON SCREEN                                    
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT END AT END                
         BNL   *+10                OF TABLE THEN SET DOWN INDICATOR             
         MVC   FLD(2),=C'>>'                                                    
*                                                                               
LSLOWOUT DS    0H                                                               
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
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
NLINS    EQU   ((SCRNAMLH-SCRNAM1H)/(SCRNAM2H-SCRNAM1H))+1                      
NFLDS    EQU   5            5 FIELDS PER LINE (07/05 SCRSUP ADDED)              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS - LINHOOK'                   
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP PROCESSING HOOK                                  *          
*                                                                    *          
**********************************************************************          
         DS    0D                                                               
LINHOOK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         CLI   LUMODE,LUVALQ       VALIDATE                                     
         BE    LHVAL                                                            
         CLI   LUMODE,LUDSPQ       DISPLAY                                      
         BE    LHDIS                                                            
         CLI   LUMODE,LUMOREQ      MORE TO DISPLAY                              
         BE    LHMORE                                                           
         DC    H'0'                INVALID MODE                                 
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP VALIDATION HOOK ROUTINE                          *          
**********************************************************************          
         SPACE 2                                                                
LHVAL    DS    0H                                                               
*                                                                               
*              IF FIRST INPUT FIELD HAS '++' THEN USER WANTS TO CLEAR           
*              OUT WINDOW FROM THIS POINT ON                                    
*                                                                               
         L     R2,LUACLIN          TEST FOR SCREEN CLEAR REQUEST                
*                                                                               
         CLC   8(2,R2),=C'++'      CHECK FOR CLEARING INDICATOR                 
         BNE   LHV04                                                            
*                                                                               
         NI    LULSTAT,X'FF'-LUSNPVQ  TURN OFF NOT VALIDATED INDICATOR          
         OI    LUSTAT,LUCLEARQ     TELL LINUP TO CLEAR FROM HERE ON             
*                                                                               
         OC    ACURFORC,ACURFORC   SET CURSOR IF IT IS NOT SET ALREADY          
         BNZ   *+8                                                              
         ST    R2,ACURFORC         FORCE CURSOR TO THIS FIELD                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LHVALX                                                           
*                                                                               
LHV04    DS    0H                                                               
*                                                                               
         BAS   RE,LHVALLIN         YES, VALIDATE LINE AND ADD TO TABLE          
         BNE   LHVALX              IF ERROR DONT DELETE OLD AND DONT            
*                                  CLEAR SAVE TABLE ENTRY                       
         EJECT                                                                  
LHV06    DS    0H                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED TABLE ENTRY           
         USING LSVTABD,R2          ESTABLISH ENTRY                              
*                                                                               
         OC    LSVKEY,LSVKEY       WAS ANYTHING THERE BEFORE                    
         BZ    LHV10               NO                                           
*                                  YES, MARK OLD ENTRY DELETED                  
*                                  NOTE- ADD+DEL=CHANGE, THIS ENTRY             
*                                        MAY BE SAME AS ABOVE                   
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R2)),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,RF           ESTABLISH ELEMENT                            
*                                                                               
         OI    ELTCTL,ELTDELQ      SET ENTRY DELETED                            
         DROP  RF                                                               
*                                                                               
LHV10    DS    0H                  SET NEW SAVE TABLE ENTRY                     
         XC    LSVKEY,LSVKEY       INIT SAVE TABLE ENTRY                        
         ICM   RF,15,ELTENT        GET ADDRESS OF ELEMENT                       
         BZ    *+10                PUT IN TABLE IF FOUND                        
         MVC   LSVKEY,ELTKEY-ELTABD(RF)                                         
*                                                                               
LHVALX   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP DISPLAY HOOK ROUTINE                             *          
**********************************************************************          
         SPACE 2                                                                
LHDIS    DS    0H                                                               
         MVI   LUSTAT,0            INIT STATUS                                  
         L     R4,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,R4                                                       
         XC    0(LSVTABL,R4),0(R4)  CLEAR IT                                    
*                                                                               
         BAS   RE,LHSRCH           FIND ELEM TO DISPLAY                         
         BAS   RE,LHDISLIN         BUILD SCREEN LINE                            
*                                                                               
LHDISX   DS    0H                                                               
         B     LHOOKX                                                           
         EJECT                                                                  
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
         TITLE 'PRSFM31 - BUILD TABLE OF ELEMENTS ON FILE - LSBLDTAB'           
***********************************************************************         
*                                                                     *         
* ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSBLDTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         XC    BSPPRMS(BSPPRML),BSPPRMS INIT BINSRCH PARAMETERS                 
*                                                                               
         L     R1,AIO2             STORE TABLE IN I/O2 & I/O3                   
         ST    R1,BSPATAB          PASS TABLE ADDRESS                           
         LA    R1,ELTABL           PASS ENTRY LENGTH                            
         ST    R1,BSPLENR                                                       
         LA    R1,ELTKEYL          PASS KEY LENGTH                              
         ST    R1,BSPLENK                                                       
         MVI   BSPKEYD,0           PASS KEY DISPLACEMENT                        
         LA    R1,ELTMAX           PASS MAXIMUM COUNT                           
         ST    R1,BSPMAX                                                        
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CCVLIDQ      LIST ENTRY ELEMENT                           
         USING CCVLSTD,R6          ESTABLISH LIST ELEMENT                       
*                                                                               
         SR    R0,R0               INIT COUNTER                                 
*                                                                               
         BRAS  RE,GETEL            FIND FIRST LIST ELEMENT                      
*                                                                               
LSBTLOOP DS    0H                                                               
*                                                                               
         BNE   LSBTDONE            NO ELEMENTS LEFT                             
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CCVLLEN        ELEMENT LENGTH                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELTELEM(0),CCVLELM  SAVE LIST ELEMENT                            
*                                                                               
         AHI   R0,1                BUMP ELEMENT COUNTER                         
         STC   R0,ELTSORT          SET ELEMENT COUNTER AS SORT KEY              
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',WRKELTAB),RR=RELO                  
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FITS INTO TABLE              
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
LSBTCONT BRAS  RE,NEXTEL           FIND NEXT LIST ELEMENT                       
*                                                                               
         B     LSBTLOOP                                                         
*                                                                               
LSBTDONE DS    0H                                                               
*                                                                               
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+6                                                              
         BCTR  R1,0                MINUS ONE                                    
         MHI   R1,ELTABL           TIMES ENTRY LENGTH                           
*                                                                               
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
         TITLE 'PRSFM31 - SEARCH TABLE FOR ELEMENTS - LSSRCH'                   
***********************************************************************         
*                                                                     *         
* ROUTINE TO SEARCH TABLE FOR ELEMENT                                 *         
* AND SET ADDRESS IN ELTENT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHSRCH   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         XC    ELTENT,ELTENT       INIT ELEMENT ADDRESS RETURN                  
         NI    LUSTAT,255-LUEOLQ   SET OFF END OF LIST INDICATOR                
*                                                                               
         OC    BSPNOR,BSPNOR       IF NO ENTRIES                                
         BNZ   *+12                                                             
         OI    LUSTAT,LUEOLQ          SET END OF LIST INDICATOR                 
         B     LHSRCHX                RETURN EMPTY-HANDED                       
*                                                                               
         L     R4,BSPATAB          DEFAULT TO FIRST ENTRY IN TABLE              
         USING ELTABD,R4                                                        
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
         USING LSVTABD,R3                                                       
*                                                                               
         MVC   HALF,LSVKEY         COPY KEY                                     
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                                                               
         OC    HALF,HALF           NO PREVIOUS ENTRY MEANS FIRST TIME           
         BNZ   LHSRCH02            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         BNE   LHSRCH11               AFTER AN UP SCROLL                        
*                                                                               
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         B     LHSRCHX                                                          
*                                                                               
LHSRCH02 DS    0H                                                               
*                                                                               
         OC    HALF,HALF           NO PREVIOUS ENTRY MEANS FIRST TIME           
         BZ    LHSRCH11            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLC   HALF,HIVALS         IF PREVIOUS IS X'FF'S                        
         BNE   LHSRCH05                                                         
*                                                                               
         L     RE,BSPNOR              USE LAST ENTRY IN TABLE                   
         BCTR  RE,0                   DECREMENT FOR INDEXING                    
         L     RF,BSPLENR             RECORD LENGTH                             
         MR    RE,RE                  INDEX TO LAST ENTRY                       
         LA    R4,0(RF,R4)            A(LAST TABLE ENTRY)                       
         B     LHSRCH10                                                         
*                                                                               
LHSRCH05 DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',HALF),RR=RELO                     
*                                                                               
         L     R4,BSPAREC          GET TABLE ENTRY ADDRESS                      
*                                                                               
         CLI   BSPCNTL,BSPNF       DEFAULT TO FIRST OF TABLE IF                 
         BNE   LHSRCH10            PREVIOUS ENTRY WAS NOT FOUND                 
*                                                                               
         L     R4,BSPATAB          NO, POINT TO FIRST-(END OF TABLE)            
         B     LHSRCH11            DONE (NO MOVEMENT)                           
         EJECT                                                                  
LHSRCH10 DS    0H                                                               
         CLI   LUDIR,C'-'          CHECK FOR BACKWARD SCROLLING                 
         BE    LHSRCH16                                                         
*                                                                               
         CLI   LUDIR,C'='          SKIP BUMPING ENTRY IF RE-DISPLAYING          
         BE    *+8                                                              
         LA    R4,ELTABL(R4)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
LHSRCH11 DS    0H                                                               
*                                                                               
         C     R4,ELTLAST          DONE IF NOT AT END OF TABLE                  
         BL    LHSRCH30                                                         
         BE    LHSRCH12            AT END OF TABLE TELL LINUP                   
*                                  PAST END - ONLY IF SCROLLING DOWN            
*                                  AND PRIOR SCREEN ENDED WITH LAST             
*                                  ELEMENT IN TABLE - USE DEFAULT               
*                                  OR UP SCROLLING AND TABLE HAS ONLY           
*                                  ONE ELEMENT - STOP WITH NO DISPLAY           
         L     R4,BSPATAB          POINT TO FIRST ENTRY IN TABLE                
         CLC   BSPNOR,=F'1'        DONE IF MORE THAN ONE ENTRY IN TABLE         
         BH    LHSRCH30                                                         
*                                  EXIT WITHOUT FINDING ELEMENT                 
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCHX                                                          
*                                                                               
LHSRCH12 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCH30                                                         
         EJECT                                                                  
LHSRCH16 DS    0H                  GOING BACKWARDS                              
         C     R4,BSPATAB          IF AT START                                  
         BNH   LHSRCH18            DONT GO FURTHER                              
         SHI   R4,ELTABL           BACK UP AN ENTRY                             
LHSRCH17 DS    0H                                                               
         C     R4,BSPATAB          IF AT START                                  
         BH    *+8                                                              
LHSRCH18 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
*                                                                               
LHSRCH30 DS    0H                                                               
         TM    ELTCTL,ELTDELQ+ELTADDQ DISPLAY CHANGED ELEMENTS                  
         BO    LHSRCH40                                                         
         TM    ELTCTL,ELTDELQ      BYPASS DELETED ELEMENTS                      
         BNO   LHSRCH40                                                         
         TM    LUSTAT,LUEOLQ       EXIT IF AT END OF LIST                       
         BO    LHSRCHX                                                          
         CLI   LUDIR,C'='          QUIT IF RE-DISPLAY                           
         BE    LHSRCH40                                                         
         B     LHSRCH10            ELSE GO FIND NEXT ELEMENT                    
*                                                                               
LHSRCH40 DS    0H                                                               
         ST    R4,ELTENT           RETURN TABLE ENTRY ADDRESS                   
         L     R3,LUACTAB          POINT TO CURRENT SAVE TABLE ENTRY            
         MVC   LSVKEY,ELTKEY       SAVE APPROPRIATE DATA                        
         MVC   LSVKEYNW,ELTKEYNW   SAVE APPROPRIATE DATA                        
*                                                                               
LHSRCHX  DS    0H                                                               
         MVC   SVDIR,LUDIR         SAVE LAST TIME DIRECTION                     
*                                                                               
         XIT1                                                                   
HIVALS   DC    32X'FF'             HIGH VALUES                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM31 - VALIDATE LINE - LHVALLIN'                             
***********************************************************************         
*                                                                     *         
* ROUTINE TO VALIDATE WINDOW LINE                                     *         
* BUILD ENTRY IN APWORK AND ADD TO ELEM TABLE                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHVALLIN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         XC    ELTENT,ELTENT       CLEAR A(ENTRY)                               
*                                                                               
         TM    LULSTAT,LUSNPVQ     SKIP IF ALL FIELDS PREVIOUSLY                
         BO    *+8                   VALIDATED                                  
         TM    LULSTAT,LUSDATQ     AND NO DATA IN FIELDS ON THIS LINE           
         BZ    LHVLX                                                            
*                                                                               
         XC    WRKELTAB,WRKELTAB   INIT WORK AREA                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
         USING ELTABD,R4           IN WORK AREA                                 
*                                                                               
         LA    R3,ELTELEM          INITIALIZE ELEMENT                           
         USING CCVLSTD,R3          ESTABLISH STANDARD CONTRACT UNIT ELM         
*                                                                               
         SLR   R4,R4               INIT TABLE ELEMENT POINTER                   
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE                 
         USING FLDHDRD,R2          ESTABLISH INPUT FIELD                        
*                                                                               
         TITLE 'PRSFM31 - VALIDATE LINE - LHVNAM'                               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE CONTACT NAME                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVNAM   DS    0H                                                               
*                                                                               
         CLI   FLDILEN,0           IF NO INPUT IN NAME FIELD                    
         BNE   LHVNAM20                                                         
*                                                                               
         TM    LULSTAT,LUSDATQ     AND DATA IN SOME FIELDS ON THIS LINE         
         BO    LHVMISSE               ERROR                                     
*                                                                               
*        NO DATA IN ANY FIELD ON LINE                                           
*        MARK ALL FIELDS ON LINE VALIDATED                                      
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE LVL IND         
         SR    R0,R0                                                            
         IC    R0,LUNFLDS          NUMBER OF FIELDS LEFT ON LINE                
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
         BAS   RE,BUMPU            BUMP TO NEXT FIELD                           
         BCT   R0,*-8                                                           
*                                                                               
         B     LHVLX               GO TO NEXT LINE                              
*                                                                               
LHVNAM20 DS    0H                                                               
*                                                                               
         MVC   DUB,FLDDATA         MOVE DATA TO WORK AREA                       
         OC    DUB,SPACES          FORCE UPPERCASE                              
*                                                                               
         CLC   =C'DELETE',DUB      DELETE THIS ENTRY                            
         BE    LHVNAMOK            IGNORE ANY OTHER INPUT                       
*                                                                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
*                                                                               
         MVI   CCVLCDE,CCVLIDQ     SET ELEMENT CODE                             
         MVI   CCVLLEN,CCVLSTLQ    SET BASIC ELEMENT LENGTH                     
         MVC   CCVLNAME,FLDDATA    SAVE CONTACT NAME                            
*                                                                               
         TM    FLDIIND,FINPTHIS    IF FIELD INPUT THIS TIME                     
         BNO   *+8                                                              
         OI    SVACH1,CCVALSCH        LIST CHANGED                              
*                                                                               
LHVNAMOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
LHVNAMX  DS    0H                                                               
*                                                                               
         TITLE 'PRSFM31 - VALIDATE CONTACT TYPE - LHVTYP'                       
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE ADDRESS TYPE                             *         
*              C'F' - FAX NUMBER                                      *         
*              C'E' - E-MAIL - DEFAULT VALUE                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVTYP   DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU            BUMP TO TYPE FIELD                           
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHVTYPOK                                                         
*                                                                               
         MVI   CCVLTYPE,C'E'       ASSUME THIS IS AN E-MAIL ADDRESS             
*                                                                               
         CLI   FLDILEN,0           OKAY IF NO ENTRY                             
         BE    LHVTYP10                                                         
*                                                                               
         CLI   FLDDATA,C'F'        ENTRY MUST BE C'F' OR                        
         BE    *+8                                                              
         CLI   FLDDATA,C'E'        C'E'                                         
         BNE   LHVTYPNV                                                         
*                                                                               
         MVC   CCVLTYPE,FLDDATA    SAVE ADDRESS TYPE                            
*                                                                               
LHVTYP10 DS    0H                                                               
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         CLI   CCVLTYPE,C'E'       EXPAND CODE LETTER                           
         BNE   *+14                                                             
         MVC   FLDDATA(L'LHVTYPEM),LHVTYPEM                                     
         B     LHVTYPOK                                                         
*                                                                               
         CLI   CCVLTYPE,C'F'       EXPAND CODE LETTER                           
         BNE   *+10                                                             
         MVC   FLDDATA(L'LHVTYPFX),LHVTYPFX                                     
*                                                                               
LHVTYPOK DS    0H                                                               
*                                                                               
         TM    FLDIIND,FINPTHIS    IF FIELD INPUT THIS TIME                     
         BNO   *+8                                                              
         OI    SVACH1,CCVALSCH        LIST CHANGED                              
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
         B     LHVTYPX                                                          
*                                                                               
LHVTYPEM DC    CL8'E-mail'         E-MAIL LOWERCASE                             
LHVTYPFX DC    CL8'Fax'            FAX    LOWERCASE                             
*                                                                               
LHVTYPX  DS    0H                                                               
*                                                                               
         TITLE 'PRSFM31 - VALIDATE FYI FIELD - LHVFYI'                          
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE FYI FIELD                                *         
*              C'Y' - SET CCVLTYP2 TO Y                               *         
*  NO ENTRY OR C'N' - CLEAR CCVLTYP2                                  *         
*   ANY OTHER ENTRY - INVALID                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVFYI   DS    0H                                                               
*                                                                               
         MVI   CCVLTYP2,0          CLEAR FYI FIELD                              
*                                                                               
         BAS   RE,BUMPU            BUMP TO FYI FIELD                            
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHVFYIOK                                                         
*                                                                               
         CLI   FLDILEN,0           OKAY IF NO ENTRY                             
         BE    LHVFYI10                                                         
*                                                                               
         CLI   FLDDATA,C'Y'        ENTRY MUST BE C'Y' OR                        
         BE    *+8                                                              
         CLI   FLDDATA,C'N'        C'N'               OR                        
         BE    *+8                                                              
         BNE   LHVTYPNV                                                         
*                                                                               
         MVI   CCVLTYP2,C'Y'       SET FYI FIELD                                
         CLI   FLDDATA,C'N'        C'N' ?                                       
         BNE   LHVFYIOK                                                         
         MVI   CCVLTYP2,0          CLEAR FYI FIELD                              
*                                                                               
LHVFYI10 DS    0H                                                               
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
LHVFYIOK DS    0H                                                               
*                                                                               
         TM    FLDIIND,FINPTHIS    IF FIELD INPUT THIS TIME                     
         BNO   *+8                                                              
         OI    SVACH1,CCVALSCH        LIST CHANGED                              
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
*******  B     LHVFYIX                                                          
*                                                                               
LHVFYIX  DS    0H                                                               
*                                                                               
         TITLE 'PRSFM31 - VALIDATE SUPPRESS COST FIELD - LHVSUP'                
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE SUPPRESS COST FIELD                      *         
*              C'Y' - SET CCVLSUP TO Y                                *         
*  NO ENTRY OR C'N' - CLEAR CCVLSUP                                   *         
*   ANY OTHER ENTRY - INVALID                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVSUP   DS    0H                                                               
*                                                                               
         MVI   CCVLSUP,0           CLEAR SUPPRESS COST FIELD                    
*                                                                               
         BAS   RE,BUMPU            BUMP TO SUPPRESS COST FIELD                  
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHVSUPOK                                                         
*                                                                               
         CLI   FLDILEN,0           OKAY IF NO ENTRY                             
         BE    LHVSUP10                                                         
*                                                                               
         CLI   FLDDATA,C'Y'        ENTRY MUST BE C'Y' OR                        
         BE    *+8                                                              
         CLI   FLDDATA,C'N'        C'N'               OR                        
         BE    *+8                                                              
         BNE   LHVTYPNV                                                         
*                                                                               
         MVI   CCVLSUP,C'Y'        SET SUPPRESS COST FIELD                      
         CLI   FLDDATA,C'N'        C'N' ?                                       
         BNE   LHVSUPOK                                                         
         MVI   CCVLSUP,0           CLEAR SUPPRESS COST FIELD                    
*                                                                               
LHVSUP10 DS    0H                                                               
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
LHVSUPOK DS    0H                                                               
*                                                                               
         TM    FLDIIND,FINPTHIS    IF FIELD INPUT THIS TIME                     
         BNO   *+8                                                              
         OI    SVACH1,CCVALSCH        LIST CHANGED                              
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
*******  B     LHVSUPX                                                          
*                                                                               
LHVSUPX  DS    0H                                                               
*                                                                               
         TITLE 'PRSFM31 - VALIDATE CONTACT E-ADDRESS - LHVADDR'                 
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE FAX/E-MAIL ADDRESS                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVADDR  DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU            BUMP TO ADDRESS FLD                          
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHVADDOK                                                         
*                                                                               
         CLI   FLDILEN,0           MUST HAVE AN ENTRY                           
         BE    LHVADDNE                                                         
*                                                                               
*        FAX NUMBER MUST CONTAIN BETWEEN 10 AND 17 DIGITS                       
*                                                                               
LHVFAX   DS    0H                                                               
*                                                                               
         CLI   CCVLTYPE,C'F'       SKIP IF NOT A FAX NUMBER                     
         BNE   LHVFAXN                                                          
*                                                                               
*        EXTRACT ALL NUMERIC DIGITS FROM INPUT                                  
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,FLDILEN        GET LENGTH OF INPUT                          
         XC    WORK,WORK           INIT WORKAREA                                
         LA    RF,FLDDATA          POINT TO INPUT                               
         LA    RE,WORK                                                          
*                                                                               
LHVFAXLP DS    0H                                                               
*                                                                               
         CLI   0(RF),C'0'          DROP NON-NUMERIC CHARACTERS                  
         BL    LHVFAXCN                                                         
         CLI   0(RF),C'9'                                                       
         BH    LHVFAXCN                                                         
*                                                                               
         MVC   0(1,RE),0(RF)       SAVE NUMERIC DIGIT                           
         AHI   RE,1                BUMP TO NEXT SAVE POSITION                   
*                                                                               
LHVFAXCN DS    0H                                                               
*                                                                               
         AHI   RF,1                BUMP TO NEXT INPUT CHARACTER                 
         BCT   R0,LHVFAXLP                                                      
*                                                                               
LHVFAXDN DS    0H                                                               
*                                                                               
         LA    RF,WORK             CALCULATE LENGTH OF FAX NUMBER               
         SR    RE,RF                                                            
*                                                                               
         CHI   RE,10               MUST BE AT LEAST 10 DIGITS                   
         BL    LHVFAXE1                                                         
*                                                                               
         CHI   RE,17               MUST BE LESS THAN 17 DIGITS                  
         BNL   LHVFAXE1                                                         
*                                                                               
         BCTR  RE,0                PREP FOR EXECUTED MOVE                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CCVLADDR(0),WORK    SAVE FAX NUMBER                              
         ST    RE,FULL             SAVE RE                                      
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CCVLLEN          BUMP ELEMENT LENGTH                          
         AR    RF,RE               ADD FAX NUMBER LENGTH                        
         AHI   RF,1                RE WAS RECUCED FOR EXECUTED MOVE             
         STC   RF,CCVLLEN                                                       
*                                                                               
*        RE-DISPLAY FAX NUMBER                                                  
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         L     RE,FULL             RESTORE LENGTH FOR EXECUTED MOVE             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),CCVLADDR                                              
*                                                                               
LHVFAXX  DS    0H                                                               
*                                                                               
         B     LHVADDFD                                                         
*                                                                               
LHVFAXN  DS    0H                                                               
*                                                                               
*        E-MAIL ADDRESS CAN'T START WITH @                                      
*              CAN'T HAVE 2 @'S                                                 
*              CAN'T END IN @                                                   
*                                                                               
         CLI   FLDDATA,C'@'        CAN'T BE A DOMAIN NAME                       
         BE    LHVADDE1                                                         
*                                                                               
         LA    R0,C'@'             SET SEARCH CHARACTER                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,FLDILEN        INPUT LENGTH                                 
         BZ    LHVADDNE            MUST HAVE AN E-MAIL ADDRESS                  
*                                                                               
         LA    RE,FLDDATA(RE)      END OF INPUT                                 
         LR    R1,RE               SAVE END ADDRESS                             
*                                                                               
         LA    RF,FLDDATA          BYPASS FIRST POSITION                        
*                                                                               
         SRST  RE,RF               SEARCH FOR @ SIGN                            
         BL    LHVADD05              FOUND                                      
*                                    NOT FOUND                                  
         CLI   SCRBAS,C' '         DOMAIN NAME EXISTS ?                         
         BNH   LHVADDE4               MUST HAVE A DOMAIN NAME                   
*                                                                               
         B     LHVADD10              OKAY - NO @ BUT HAS DOMAIN NAME            
*                                  FOUND                                        
LHVADD05 DS    0H                                                               
*                                                                               
         LA    RF,1(RE)            START OF NEXT SEARCH                         
         LR    RE,R1               RESET END OF SEARCH                          
*                                  FOUND                                        
         CR    RE,RF               CAN'T END IN @                               
         BE    LHVADDE3                                                         
*                                                                               
         SRST  RE,RF               LOOK FOR SECOND @ SIGN                       
         BL    LHVADDE2            SECOND @ FOUND                               
*                                                                               
*        NO SPACES ALLOWED IN ADDRESS                                           
*                                                                               
LHVADD10 DS    0H                                                               
*                                                                               
         LA    R0,C' '             SET SEARCH CHARACTER                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,FLDILEN          INPUT LENGTH                                 
*                                                                               
         LA    RE,FLDDATA(RE)      END OF INPUT                                 
*                                                                               
         LA    RF,FLDDATA          START OF INPUT                               
*                                                                               
         SRST  RE,RF               SEARCH FOR INTERNAL SPACE                    
         BL    LHVADDE5            FOUND - NO GOOD                              
*                                                                               
*        HAVE FOUND VALID ADDRESS                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDILEN          INPUT LENGTH                                 
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CCVLADDR(0),FLDDATA    SAVE E-MAIL ADDRESS                       
*                                                                               
         AHI   RF,CCVLSTLQ+1       CALCULATE ELEMENT LENGTH                     
         STC   RF,CCVLLEN          NEW ELEMENT LENGTH                           
*                                                                               
LHVADDFD DS    0H                                                               
*                                                                               
         TM    FLDIIND,FINPTHIS    IF FIELD INPUT THIS TIME                     
         BNO   *+8                                                              
         OI    SVACH1,CCVALSCH        LIST CHANGED                              
*                                                                               
LHVADDOK DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
LHVADDX  DS    0H                                                               
*                                                                               
         LTR   R4,R4               DONE IF NOT BUILDING AN ELEMENT              
         BZ    LHVLX                                                            
*                                                                               
         L     RF,LUACTAB          POINT TO CURRENT SAVED ELEMENT               
         USING LSVTABD,RF                                                       
*                                                                               
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+8                                                              
         LA    RF,LSVKEYNW            USE IT                                    
*                                                                               
         OC    LSVKEY,LSVKEY       IF THERE WAS NOTHING LAST TIME               
         BNZ   LHVTLINX                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SVELTKEY+1         BUMP SUB LINE NUMBER                       
         LA    R1,1(R1)               OF LAST USED ELEMENT KEY                  
         STC   R1,SVELTKEY+1                                                    
*                                                                               
         LA    RF,SVELTKEY            POINT TO LAST USED KEY                    
*                                                                               
LHVTLINX DS    0H                                                               
*                                                                               
         MVC   ELTKEY,LSVKEY       SET SORT KEY TO LINE NUMBER                  
*                                                                               
         MVC   SVELTKEY,ELTKEY     SAVE CURRENT KEY                             
*                                                                               
         DROP  RF                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      ADD ELEMENT                                  
*                                                                               
*        ADD NEW ELEMENT TO TABLE                                               
*                                                                               
LHVCMP   DS    0H                                                               
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE CURRENT MESSAGE NUMBER                  
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',ELTABD),RR=RELO                    
*                                                                               
         OC    1(3,R1),1(R1)       TEST ROOM                                    
         BZ    LHVCMPE1                                                         
*                                                                               
         L     R4,BSPAREC          POINT TO FOUND TABLE ELEMENT                 
*                                                                               
         STCM  R4,7,ELTENT+1                                                    
*                                                                               
         CLI   BSPCNTL,BSPNF       TEST IF NO MATCH FOUND                       
         BE    LHVL92              YES - NEW ENTRY FOR TABLE                    
*                                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED ELEMENT               
         USING LSVTABD,R2                                                       
*                                                                               
         CLC   LSVKEY,ELTKEY       ALSO OK IF ENTRY KEY NOT CHANGED             
         BE    *+10                                                             
         CLC   LSVKEYNW,ELTKEY     ALSO OK IF ENTRY KEY NOT CHANGED             
         BE    *+10                                                             
         CLC   LSVKEYNW,ELTKEYNW   ALSO OK IF ENTRY KEY NOT CHANGED             
         BNE   LHVCMPE2                                                         
*                                                                               
LHVL92   DS    0H                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      SET ADDED (NB- ADD+DEL=CHA)                  
         MVC   ELTELEM,CCVLELM     SET NEW ELEM IN TABLE                        
*                                  SET NEW ELTLAST                              
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(ELTABL)                                                    
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
         B     LHVLX                                                            
*                                                                               
LHVLX    DS    0H                                                               
         CLI   ERROR,0             SET CC                                       
         XIT1                                                                   
         EJECT                                                                  
LHVCMPE1 DS    0H                                                               
         MVI   ERROR,RECFULL             TOO MANY DETAIL LINES                  
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE2 DS    0H                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =SPARM          
         MVI   ERROR,DUPEDATA      DUPLICATE                                    
         B     LHVCMPER                                                         
*                                                                               
LHVCMPER DS    0H                                                               
*                                                                               
         NI    4(R2),X'FF'-X'20'   TURN OFF VALIDATED STATUS                    
         GOTO1 ERREX               HANDLE FIELD IN ERROR                        
*                                                                               
         DROP  R4                                                               
         DROP  R3                                                               
         DROP  R2                                                               
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     LHVERR0X                                                         
*                                                                               
LHVTYPNV MVI   ERROR,INVALID       LIST ENTRY TYPE NOT VALID                    
         B     LHVERR0X                                                         
*                                                                               
DUPERR   MVI   ERROR,DUPEDATA      DUPLICATE DATA                               
         B     LHVERR0X                                                         
*                                                                               
LHVFAXE1 LHI   RF,PWEFAXNV         INVALID FAX NUMBER                           
         B     LHVERR1X                                                         
*                                                                               
LHVADDE1 DS    0H                                                               
         LHI   RF,PWEADDE1         E-MAIL ADDRESS STARTS WIRH @                 
         B     LHVERR1X                                                         
*                                                                               
LHVADDNE DS    0H                                                               
         LHI   RF,PWEADDNE         E-MAIL ADDRESS MISSING                       
         B     LHVERR1X                                                         
*                                                                               
LHVADDE2 DS    0H                                                               
         LHI   RF,PWEADDE2         TOO MANY @ SIGNS                             
         B     LHVERR1X                                                         
*                                                                               
LHVADDE3 DS    0H                                                               
         LHI   RF,PWEADDE3         END IN @                                     
         B     LHVERR1X                                                         
*                                                                               
LHVADDE4 DS    0H                                                               
         LHI   RF,PWEADDE4         NO DOMAIN NAME                               
         B     LHVERR1X                                                         
*                                                                               
LHVADDE5 DS    0H                                                               
         LHI   RF,PWEADDE5         SPACES IN E-MAIL ADDRESS                     
         B     LHVERR1X                                                         
*                                                                               
LHVMISSE MVI   ERROR,MISSING                                                    
         B     LHVERR0X                                                         
*                                                                               
NONEERR  MVI   ERROR,NEEDDATA      AT LEAST ONE ENTRY REQUIRED                  
         B     LHVERR0X                                                         
*                                                                               
LHVERR0X DS    0H                                                               
         GOTOR ERREX                                                            
*                                                                               
LHVERR1X DS    0H                                                               
         STCM  RF,3,ERROR2CD       SET ERROR EQUATE                             
*                                                                               
LHVERRX  DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2cd IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         BE    *+10                                                             
         MVC   ERROR2CD+1(1),ERROR      PUT IN NEW STYLE                        
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVC   GTMSGNO,ERROR2CD    MESSAGE NUMBER                               
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM31 - VALIDATE CONTACT E-ADDRESS - LHDISLIN'                
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
LHDISLIN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         L     R2,LUACLIN          A(FIRST FIELD)                               
         USING FLDHDRD,R2          ESTABLISH GENERIC FIELD                      
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         BZ    LHDNAMX             CHECK IF NONE FOUND                          
*                                                                               
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING CCVLSTD,R3                                                       
*                                                                               
*              DISPLAY CONTACT NAME                                             
*                                                                               
LHDNAM   DS    0H                                                               
*                                                                               
         MVC   FLDDATA(L'CCVLNAME),CCVLNAME  DISPLAY CONTACT NAME               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
LHDNAMX  DS    0H                                                               
*                                                                               
*        DISPLAY ADDRESS TYPE                                                   
*                                                                               
LHDTYP   DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU                                                         
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NOT ELEMENT TO DISPLAY               
         BZ    LHDTYPX                                                          
*                                                                               
         MVC   FLDDATA(L'CCVLTYPE),CCVLTYPE  DISPLAY ADDRESS TYPE               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
*                                                                               
         CLI   FLDDATA,C'E'        EXPAND CODE LETTER                           
         BNE   *+14                                                             
         MVC   FLDDATA(L'LHDTYPEM),LHDTYPEM                                     
         B     LHDTYP10                                                         
*                                                                               
         CLI   FLDDATA,C'F'        EXPAND CODE LETTER                           
         BNE   *+10                                                             
         MVC   FLDDATA(L'LHDTYPFX),LHDTYPFX                                     
*                                                                               
LHDTYP10 DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
         B     LHDTYPX                                                          
*                                                                               
LHDTYPEM DC    CL8'E-mail'         E-MAIL LOWERCASE                             
LHDTYPFX DC    CL8'Fax'            FAX    LOWERCASE                             
*                                                                               
LHDTYPX  DS    0H                                                               
*                                                                               
*              DISPLAY FYI                                                      
*                                                                               
LHDFYI   DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU                                                         
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NOT ELEMENT TO DISPLAY               
         BZ    LHDFYIX                                                          
*                                                                               
         MVC   FLDDATA(L'CCVLTYP2),CCVLTYP2   DISPLAY (Y OR NULL)               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
LHDFYIX  DS    0H                                                               
*                                                                               
*              DISPLAY SUPPRESS COST                                            
*                                                                               
LHDSUP   DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU                                                         
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NOT ELEMENT TO DISPLAY               
         BZ    LHDSUPX                                                          
*                                                                               
         MVC   FLDDATA(L'CCVLSUP),CCVLSUP    DISPLAY (Y OR NULL)                
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
LHDSUPX  DS    0H                                                               
*                                                                               
*        DISPLAY ADDRESS                                                        
*                                                                               
LHDADR   DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU                                                         
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NOT ELEMENT TO DISPLAY               
         BZ    LHDADRX                                                          
*                                                                               
*        FAX ADDRESSES ARE EXACTLY 10 DIGITS                                    
*        DISPLAY AS NNN-NNN-NNNN                                                
*     A/O 8/10/04, FAX ADDRESSES ARE TREATED EXACTLY LIKE E-MAIL                
*                                                                               
LHDFAX   DS    0H                                                               
*                                                                               
         B     LHDFAXN             TREAT LIKE E-MAIL                            
*                                                                               
         CLI   CCVLTYPE,C'F'       SKIP IF NOT FAX NUMBER                       
         BNE   LHDFAXN                                                          
*                                                                               
         MVC   FLDDATA(3),CCVLADDR AREA CODE                                    
         MVI   FLDDATA+3,C'-'                                                   
         MVC   FLDDATA+4(3),CCVLADDR+3 EXCHANGE                                 
         MVI   FLDDATA+7,C'-'                                                   
         MVC   FLDDATA+8(4),CCVLADDR+6 NUMBER                                   
*                                                                               
         B     LHDADR99                                                         
*                                                                               
LHDFAXN  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CCVLLEN          GET ELEMENT LENGTH                           
         SHI   RF,CCVLSTLQ         DECREMENT BY BASIC LENGTH                    
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),CCVLADDR  DISPLAY ADDRESS                             
*                                                                               
LHDADR99 DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     INDICATE FIELD VALIDATED                     
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
LHDADRX  DS    0H                                                               
*                                                                               
*                                                                               
LHDLX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM31 - VALIDATE CONTACT E-ADDRESS - LHWRTTAB'                
***********************************************************************         
*                                                                     *         
* ROUTINE TO UPDATE RECORD BASED ON ELEMENT TABLE CHANGES             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSWRTTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENT                          
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE MESSAGE NUMBER                          
*                                                                               
*        FIRST DELETE ALL CURRENT ELEMENTS                                      
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R6,CCVFIRST-CCVREC(R6)    POINT TO FIRST ELEMENT IN REC          
*                                                                               
         XC    DMCB+8(4),DMCB+8    SET FOR DELETE                               
*                                                                               
         SR    R0,R0                                                            
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
         L     RF,VRECUP           POINT TO UPDATING ROUTINE                    
*                                                                               
LSWTDELL DS    0H                                                               
*                                                                               
         USING CCVLSTD,R6          ESTABLISH LIST ELEMENT                       
*                                                                               
         CLI   CCVLCDE,0           DONE IF EOR REACHED                          
         BE    LSWTDELD                                                         
*                                                                               
         CLI   CCVLCDE,CCVEBIDQ    DELETE IF E-BASE ELEMENT                     
         BE    *+8                                                              
         CLI   CCVLCDE,CCVLIDQ     DELETE IF LIST ELEMENT                       
         BNE   LSWTDELC                                                         
*                                                                               
         GOTO1 (RF),(R1),,(R6)     DELETE ELEMENT                               
*                                                                               
         B     LSWTDELX            BECAUSE R6 ==> NEXT ELM NOW                  
*                                                                               
LSWTDELC DS    0H                                                               
*                                                                               
         IC    R0,CCVLLEN          ELEMENT LENGTH                               
         AR    R6,R0               NEXT ELEMENT                                 
*                                                                               
LSWTDELX DS    0H                                                               
*                                                                               
         B     LSWTDELL                                                         
*                                                                               
LSWTDELD DS    0H                  ALL ELEMENTS DELETED                         
*                                                                               
         DROP  R6                                                               
*                                                                               
*        ADD E-BASE ELEMENT TO RECORD                                           
*                                                                               
         LA    R3,SVEBASE          POINT TO SAVED E-BASE ELEMENT                
         USING CCVEBASD,R3         ESTABLISH E-BASE ELEMENT                     
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R6,CCVFIRST-CCVREC(R6)    POINT TO FIRST ELEMENT IN REC          
         GOTO1 (RF),(R1),,(R3),(C'R',(R6))  ADD ELEMENT                         
*                                                                               
         CLI   DMCB+8,0            ONLY ERROR CAN BE NO ROOM IN REC             
         BE    LSWFULER            NO ERROR TOLERATED                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)            ELEMENT LENGTH                               
         AR    R6,R0               NEXT INSERTION POINT                         
*                                                                               
*        ADD ELEMENTS IN TABLE TO RECORD                                        
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         OC    BSPNOR,BSPNOR       SKIP IF NO ENTRIES                           
         BZ    LSWTDONE                                                         
*                                                                               
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
         SR    R2,R2               INIT LINE COUNTER                            
*                                                                               
LSWTLOOP DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTADDQ      ADD ELEMENTS FLAGGED FOR ADD                 
         BO    LSWTADD                                                          
         TM    ELTCTL,ELTDELQ      AND THOSE NOT TO BE DELETED                  
         BO    LSWTNADD                                                         
*                                                                               
LSWTADD  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR WORKAREA                               
*                                                                               
         LA    R3,ELTELEM          POINT TO ELEMENT IN TABLE                    
         USING CCVLSTD,R3          ESTABLISH SPARM ELEMENT                      
*                                                                               
         CLI   CCVLLEN,0           GET ELEMENT LENGTH                           
         BZ    LSWTADDX            SKIP IF NO ENTRY                             
*                                                                               
         XC    ELTKEYNW,ELTKEYNW   INIT AREA                                    
         AHI   R2,1                BUMP ELEMENT COUNTER                         
         STCM  R2,1,ELTKEYNW       SET AS NEW KEY                               
*                                                                               
         GOTO1 (RF),(R1),,(R3),(C'R',(R6))  ADD ELEMENT                         
*                                                                               
         CLI   DMCB+8,0            ONLY ERROR CAN BE NO ROOM IN REC             
         BE    LSWFULER            NO ERROR TOLERATED                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)            ELEMENT LENGTH                               
         AR    R6,R0               NEXT INSERTION POINT                         
*                                                                               
LSWTADDX DS    0H                                                               
*                                                                               
         B     LSWTCONT                                                         
*                                                                               
LSWTNADD DS    0H                                                               
*                                                                               
LSWTCONT DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         C     R4,ELTLAST          LOOP IF NOT LAST ENTRY                       
         BNH   LSWTLOOP                                                         
*                                                                               
LSWTDONE DS    0H                                                               
*                                                                               
         LA    R0,NLINS            NUMBER OF LINES IN TABLE                     
         LA    R6,LSVTAB           LINUP SAVE AREA                              
*                                                                               
LSWLSVLP DS    0H                                                               
*                                                                               
         USING LSVTABD,R6          ESTABLISH SAVED KEYS AREA                    
*                                                                               
         OC    LSVKEY,LSVKEY       SKIP IF NO ENTRY SAVED                       
         BNZ   *+10                                                             
         OC    LSVKEYNW,LSVKEYNW                                                
         BZ    LSWLSVCN                                                         
*                                                                               
         MVC   HALF,LSVKEY                                                      
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                  FIND TABLE ENTRY                             
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',HALF),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,R4           ESTABLISH ELEMENT                            
*                                                                               
         MVC   LSVKEY,ELTKEYNW     COPY NEW KEY                                 
         XC    LSVKEYNW,LSVKEYNW                                                
*                                                                               
LSWLSVCN DS    0H                                                               
*                                                                               
         LA    R6,LSVTABL(R6)      BUMP TO NEXT ITEM IN TABLE                   
         BCT   R0,LSWLSVLP                                                      
*                                                                               
LSWLSVDN DS    0H                                                               
*                                                                               
*        SET TABLE KEYS TO NEW ONES                                             
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         ICM   R0,15,BSPNOR        SKIP IF NO ENTRIES                           
         BZ    LSWTRSDN                                                         
*                                                                               
LSWTRSLP DS    0H                                                               
*                                                                               
         MVC   ELTKEY,ELTKEYNW     RESET TO NEW KEY                             
         XC    ELTKEYNW,ELTKEYNW                                                
*                                                                               
LSWTRSCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         BCT   R0,LSWTRSLP         LOOP IF NOT LAST ENTRY                       
*                                                                               
LSWTRSDN DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   *+8                                                              
         OI    SVACH1,CCVAADD         SET RECORD ADDED                          
*                                                                               
*        ADD ACTIVITY ELEMENT                                                   
*                                                                               
         BRAS  RE,ACTPUT                                                        
*                                                                               
LSWRTX   DS    0H                                                               
         MVC   ERROR,SAVMSGNO      RESTORE MESSAGE NUMBER                       
         XIT1                                                                   
*                                                                               
LSWFULER MVI   ERROR,RECFULL       NO ROOM IN RECORD OR TABLE                   
         LA    R2,SCRNAM1H                                                      
         GOTO1 ERREX                                                            
*                                                                               
         DROP                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM31 - VENDOR CONTACT E-ADDRESS - ACTPUT'                    
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
         USING CCVACTHD,R3         ESTABLISH ACTIVITY ELM                       
*                                                                               
         MVI   CCVACDE,CCVAIDQ     SET ELEMENT CODE                             
         MVI   CCVALEN,CCVACTLQ    SET ELEMENT LENGTH                           
*                                                                               
         GOTO1 DATCON,DUB,(5,0),(3,CCVADTE) SET TODAY'S DATE                    
*                                                                               
         BRAS  RE,PIDGET           FIND PID                                     
*                                                                               
         MVC   CCVAPID,SVPID       SET CHANGER'S PID                            
         MVC   CCVASCID,SVSCID     SET CHANGER'S SECURITY PID                   
*                                                                               
         MVC   CCVACHGS,SVACHGS    SET ACTIVITY INDICATORS                      
*                                                                               
         L     R6,AIO              POINT TO RECORD FOR DISPLAY                  
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
*        CHECK OLD ACTIVITY FOR DUPLICATES                                      
*                                                                               
         MVI   ELCODE,CCVAIDQ      LOOK FOR ACTIVITY ELM                        
         BRAS  RE,GETEL            FIND IN RECORD                               
*                                                                               
ACTPUTLP DS    0H                                                               
*                                                                               
         BNE   ACTPUTDN            SKIP IF NONE FOUND                           
*                                                                               
         CLC   CCVAPID,CCVAPID-CCVAELM(R6)  IF SAME PERSON                      
         BNE   ACTPUTCN                                                         
         CLC   CCVASCID,CCVASCID-CCVAELM(R6) IF SAME SCID                       
         BNE   ACTPUTCN                                                         
         CLC   CCVADTE,CCVADTE-CCVAELM(R6)  AND DATE                            
         BNE   ACTPUTCN                                                         
*                                                                               
         OC    CCVACHGS-CCVAELM(L'CCVACHGS,R6),SVACHGS UPDATE CHANGES           
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
         GOTO1 VRECUP,DMCB,,(R3),(C'R',(R6)),0  ADD ELEMENT                     
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
         LA    R2,SCRNAM1H                                                      
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM31 - SFM VENDOR CONTACT LIST - ACTLMT'                     
***********************************************************************         
*                                                                     *         
*       ROUTINE TO LIMIT NUMBER OF ACTIVITY ELEMENTS TO FOUR          *         
*       (THREE MOST RECENT PLUS THE FIRST ACTIVITY ELEMENT)           *         
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
         LA    R4,1                USE R4 FOR BCT LOOP COUNTER                  
*                                                                               
         MVI   ELCODE,CCVAIDQ      LOOK FOR ACTIVITY ELEM                       
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
         CHI   R4,4                FOUR OR MORE ACTIVITY ELEMS ?                
         BL    ACTLMTX             NO - OK - NO DELETION NEEDED                 
*                                                                               
*        DELETE ELEMENTS - LEAVE FIRST ONE AND LAST TWO                         
*                                                                               
         AHI   R4,-3               SET R4 TO NUMBER OF ELEMS TO DELETE          
         L     R6,AIO              POINT TO RECORD                              
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         MVI   ELCODE,CCVAIDQ      LOOK FOR ACTIVITY ELEM                       
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
         TITLE 'PRSFM31 - SFM VENDOR CONTACT LIST - PIDGET'                     
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
         GOTO1 CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
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
*          DATA SET PPCON45    AT LEVEL 057 AS OF 11/05/03                      
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORD - PFKEYS'                             
***********************************************************************         
*                                                                     *         
*        ANALYZE PFKEYS                                               *         
*              PF4  - INSERT A LINE                                   *         
*              PF6  - DELETE A LINE                                   *         
*                                                                     *         
*NTRY    R5 ==>  LINUP CONTROL BLOCK                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SFM WORKING STORAGE                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING GEND,RC             GENCON WORKING STORAGE                       
*                                                                               
         L     R3,SYSPARMS                                                      
         ICM   R3,15,0(R3)         POINT TO TIOB                                
         USING TIOBD,R3                                                         
*                                                                               
         LA    R6,LNTBL            POINT TO START OF TABLE                      
         L     R4,=A(LSVTAB-T41CFFD) POINT TO START OF LINUP SAVEAREA           
         LA    R4,T41CFFD(R4)                                                   
         USING LSVTABD,R4          ESTABLISH LINUP SAVEAREA ENTRY               
*                                                                               
*        FIND LINE WITH CURSOR                                                  
*                                                                               
         CLC   TIOBCURD,0(R6)      MUST BE AFTER START OF LINE                  
         BL    PFKEYSX                                                          
         CLC   TIOBCURD,2(R6)      AND BEFORE START OF NEXT LINE                
         BL    *+16                                                             
         LA    R6,2(R6)            BUMP TO NEXT TABLE ENTRY                     
         LA    R4,LSVTABL(R4)      BUMP TO NEXT SAVEAREA                        
         B     *-28                                                             
*                                                                               
         MVC   ALINCUR,0(R6)       SAVE A(LINE WITH CURSOR)                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         CLI   TIOBAID,4           PF4 OR PF16                                  
         BE    *+8                                                              
         CLI   TIOBAID,16                                                       
         BE    LNADD                  ADD A LINE                                
*                                                                               
         CLI   TIOBAID,6           PF6 OR PF18                                  
         BE    *+8                                                              
         CLI   TIOBAID,18                                                       
         BE    LNDEL                  DELETE A LINE                             
*                                                                               
         B     PFKEYSX             IGNORE ALL OTHER KEYS                        
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'PPCON45 - PUB LIST RECORD - LNADD'                              
***********************************************************************         
*                                                                     *         
*        ADD A LINE                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNADD    DS    0H                  ADD LINE BEFORE CURSOR LINE                  
*                                                                               
         CLC   CONACT(3),=C'DIS'   CANNOT BE DISPLAYING                         
         BE    PFKEYS1E                                                         
*                                                                               
         LA    R4,NLINS            NUMBER OF LINES ON SCREEN                    
         BCTR  R4,0                DECREMENT FOR INDEXING                       
         BCTR  R4,0                DECREMENT FOR NEXT TO LAST ENTRY             
         MH    R4,=Y(LSVTABL)      DISP  TO NEXT TO LAST IN LINUP SAVE          
         L     RE,=A(LSVTAB-T41CFFD) POINT OT START OF LINUP SAVEAREA           
         LA    RE,T41CFFD(RE)                                                   
         LA    R4,0(R4,RE)         POINT TO NEXT TO LAST IN LINUP SAVE          
*                                                                               
         LA    R6,LNTBLLS          POINT TO LAST ENTRY IN TABLE                 
         LR    R5,R6                                                            
         SH    R5,=H'2'            BACK UP A TABLE ENTRY                        
         SR    R1,R1                                                            
*                                                                               
LNADDLP  DS    0H                                                               
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R5)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         LHI   R0,NFLDS            NUMBER OF FIELDS ON A LINE                   
         SR    R2,R2                                                            
*                                                                               
LNADDLP1 DS    0H                                                               
*                                                                               
         IC    R2,0(RF)            GET LENGTH OF RECEIVING FIELD                
         SH    R2,=H'8'            ALLOW FOR HEADER                             
         TM    1(RF),X'02'         IF THERE IS EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    R2,=H'8'               TAKE OUT ITS LENGTH                       
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),8(RE)       COPY LINE DOWN ONE                           
*                                                                               
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNADDCN1 DS    0H                                                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNADDLP1                                                      
*                                                                               
LNADDDN1 DS    0H                                                               
*                                                                               
         MVC   LSVTABD+LSVTABL(LSVTABL),LSVTABD  COPY LINUP SAVE                
*                                                                               
LNADDCN  DS    0H                                                               
*                                                                               
         SH    R4,=Y(LSVTABL)      BACK UP LINUP SAVE ENTRY                     
*                                                                               
         LR    R6,R5               BACK UP ENTRIES IN TABLE                     
         SH    R5,=H'2'                                                         
*                                                                               
         CLC   0(2,R5),ALINCUR     STOP IF PASSED LINE WITH CURSOR              
         BNL   LNADDLP                                                          
*                                                                               
LNADDDN  DS    0H                                                               
*                                                                               
         AH    R4,=Y(LSVTABL)      POINT TO DATA FOR CURSOR LINE                
         XC    LSVTABD(LSVTABL),LSVTABD  CLEAR LINUP SAVE ENTRY                 
*                                                                               
         LH    R6,0(R6)            POINT TO FIRST OF CURSOR LINE                
         LA    R6,0(R6,RA)                                                      
*                                                                               
         ST    R6,ACURFORC         PLACE CURSOR HERE                            
*                                                                               
         LHI   R0,NFLDS            NUMBER OF FIELDS ON LINE                     
         SR    R3,R3                                                            
*                                                                               
LNADXCLP DS    0H                                                               
*                                                                               
         TWAXC (R6),(R6),PROT=Y    CLEAR FIELD ON SCREEN                        
*                                                                               
         MVI   5(R6),0             CLEAR INPUT LENGTH                           
*******  NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
*                                                                               
LNADXCCN DS    0H                                                               
*                                                                               
         IC    R3,0(R6)            FIELD LENGTH                                 
         LA    R6,0(R3,R6)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNADXCLP                                                      
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PPCON45 - PBL FILE - INSTRUCTION RECORD - LNDEL'                
***********************************************************************         
*                                                                     *         
*        DELETE A LINE                                                *         
*                                                                     *         
*NTRY    R6==> TABLE ENTRY FOR LINE WITH CURSOR                       *         
*        R4==> LINUP TABLE ENTRY FOR LINE WITH CURSOR                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNDEL    DS    0H                  DELETE LINE AT CURSOR                        
*                                                                               
         MVI   DELFLAG,0           CLEAR                                        
*                                                                               
         CLC   CONACT(3),=C'DIS'   CANNOT BE DISPLAYING                         
         BE    PFKEYS1E                                                         
*                                                                               
         LH    R5,0(R6)            POINT TO FIRST FIELD OF CURSOR LINE          
         LA    R5,0(R5,RA)                                                      
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         TM    1(R5),X'20'         IF PROTECTED FIELD                           
         BNO   *+16                                                             
         IC    RF,0(R5)                                                         
         LA    R5,0(RF,R5)            BUMP TO NEXT FIELD                        
         B     *-16                                                             
*                                                                               
         ST    R5,ACURFORC         PLACE CURSOR HERE                            
*                                                                               
*        FIND DATA FOR THIS LINE                                                
*                                                                               
         OC    LSVTABD(LSVTABL),LSVTABD    SKIP IF NO DATA ON LINE              
         BZ    LNDELD10                                                         
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R4)),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,BSPAREC          POINT TO FOUND ELEMENT                       
         OI    ELTCTL-ELTABD(R1),ELTDELQ   FLAG FOR DELETE                      
*                                                                               
         XC    LSVTABD(LSVTABL),LSVTABD   CLEAR TABLE ENTRY                     
*                                                                               
         MVI   DELFLAG,C'D'        SET "ELEMENT TO BE DELETED"                  
*                                                                               
LNDELD10 DS    0H                                                               
*                                                                               
         LR    R5,R6                                                            
         AH    R5,=H'2'            POINT TO NEXT TABLE ENTRY                    
         SR    R1,R1                                                            
         SR    R2,R2                                                            
*                                                                               
LNDELLP  DS    0H                                                               
*                                                                               
         CLC   0(2,R5),=X'FFFF'    STOP IF PASSED END OF TABLE                  
         BNL   LNDELDN                                                          
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R5)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         LHI   R0,NFLDS            NUMBER OF FIELDS ON A LINE                   
*                                                                               
LNDELLP1 DS    0H                                                               
*                                                                               
         IC    R2,0(RF)            GET LENGTH OF RECEIVING FIELD                
         SH    R2,=H'8'            ALLOW FOR HEADER                             
         TM    1(RF),X'02'         IF THERE IS EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    R2,=H'8'               TAKE OUT ITS LENGTH                       
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),8(RE)       COPY LINE UP ONE                             
*                                                                               
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         NI    4(RF),X'FF'-X'20'   REMOVE PREVIOUSLY VALIDATED SWITCH           
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNDELCN1 DS    0H                                                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNDELLP1                                                      
*                                                                               
LNDELDN1 DS    0H                                                               
*                                                                               
         MVC   LSVTABD(LSVTABL),LSVTABD+LSVTABL  COPY LINUP SAVEAREA            
*                                                                               
LNDELCN  DS    0H                                                               
*                                                                               
         LA    R4,LSVTABL(R4)      BUMP TO NEXT SAVEAREA                        
         LR    R6,R5               ADVANCE ENTRIES IN TABLE                     
         AH    R5,=H'2'                                                         
*                                                                               
         B     LNDELLP                                                          
*                                                                               
LNDELDN  DS    0H                                                               
*                                                                               
         SHI   R4,LSVTABL          BACK UP TO LAST IN TABLE                     
         XC    LSVTABD(LSVTABL),LSVTABD   CLEAR TABLE ENTRY                     
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,LNTBLLS        POINT TO LAST LINE IN TABLE                  
         AR    R6,RA               POINT TO FIELD HEADER                        
*                                                                               
         LHI   R0,NFLDS            NUMBER OF FIELDS ON LINE                     
         SR    R3,R3                                                            
*                                                                               
LNDLXCLP DS    0H                                                               
*                                                                               
         TWAXC (R6),(R6),PROT=Y    CLEAR FIELD ON SCREEN                        
*                                                                               
         MVI   5(R6),0             CLEAR INPUT LENGTH                           
******   NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
         OI    4(R6),X'20'         SET AS VALIDATED                             
*                                                                               
LNDLXCCN DS    0H                                                               
*                                                                               
         IC    R3,0(R6)            FIELD LENGTH                                 
         LA    R6,0(R3,R6)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNDLXCLP                                                      
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PPCON45 - PUB LIST POSTING - PFKEYS - EXITS'                    
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYSX  DS    0H                  NORMAL EXIT                                  
         CR    RB,RB               FORCE EQ CC                                  
         XIT1                                                                   
*                                                                               
PFKEYS1E DS    0H                  MUST BE IN UPDATE MODE                       
*                                                                               
         LA    R2,CONACTH          CURSOR TO ACTION FIELD                       
         LHI   R3,ERACTUPD         MUST BE IN UPDATE MODE                       
*                                                                               
         STCM  R3,3,ERROR2CD       SET ERROR EQUATE                             
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         MVI   GTMSYS,C'P'         MESSAGE SYSTEM                               
         MVC   GTMSGNO,ERROR2CD    MESSAGE NUMBER                               
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
PFKEYERX DS    0H                  ERROR EXIT                                   
         LTR   RB,RB               FORCE NE CC                                  
         XIT1                                                                   
*                                                                               
ERACTUPD EQU   401                 UPDATES NOT ALLOWED IN DISPLAY MODE          
*                                                                               
         TITLE 'PPCON45 - PUB LIST POSTING - LNTBL'                             
***********************************************************************         
*                                                                     *         
*        TABLE OF LINE DISPLACEMENTS INTO SCREEN                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
         DC    XL8'00'                                                          
LNTBL    DS    0D                                                               
         DC    Y(SCRNAM1H-T41CFFD)  NAME 1                                      
         DC    Y(SCRNAM2H-T41CFFD)  NAME 2                                      
         DC    Y(SCRNAM3H-T41CFFD)  NAME 3                                      
         DC    Y(SCRNAM4H-T41CFFD)  NAME 4                                      
LNTBLLS  DC    Y(SCRNAMLH-T41CFFD)  NAME LAST                                   
         DC    4X'FF'               END OF TABLE                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         TITLE 'PRSFM31 - VALIDATE CONTACT E-ADDRESS - SUBROUTS'                
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
         TITLE 'T41C31 VENDOR CONTACT LIST RECORDS'                             
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
         TM    FLDATB,X'02'        IF THERE IS EXTENED HEADER                   
         BNO   *+8                                                              
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
HEDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H1,51,C'EIO VENDOR CONTACT LIST REPORT'                          
         PSPEC H2,51,C'------------------------------'                          
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,95,AGYADD                                                     
         PSPEC H4,95,RUN                                                        
         PSPEC H5,95,REPORT                                                     
         PSPEC H5,115,PAGE                                                      
         PSPEC H1,1,REQUESTOR                                                   
         PSPEC H6,1,C'PUB CODE        PUB NAME             CLT PRD'             
         PSPEC H7,1,C'--------        --------             --- ---'             
         PSPEC H6,46,C'CONTACT NAME'                                            
         PSPEC H7,46,C'------------'                                            
         PSPEC H6,67,C'CONTACT ADDRESS'                                         
         PSPEC H7,67,C'---------------'                                         
         PSPEC H6,125,C'FYI COST'                                               
         PSPEC H7,125,C'--- ----'                                               
         DC    X'00'                                                            
PUBZNM   DS    CL20                                                             
MYPUB    DS    XL6                                                              
MYDSKADD DS    XL4                                                              
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
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMA6D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMA7D                                                       
*                                                                               
         DS    XL64                AREA FOR BOOK= NOTICE                        
*                                                                               
LSVTAB   DS    XL((NLINS+1)*LSVTABL)                                            
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
WRKELTAB DS    XL(ELTABL)          WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
QPID     DS    XL2                 USER PID                                     
*                                                                               
SVDIR    DS    X                   LINUP DIRECTION SAVEAREA                     
SVELTKEY DS    XL2                 KEY SAVEAREA                                 
*                                                                               
CCVKSAVE DS    XL(L'CCVKEY)        KEY SAVEAREA                                 
SVEBASE  DS    XL(CCVEBLQ)         E-MAIL ELM SAVEAREA                          
SVACTELM DS    XL(CCVACTLQ)        ACTIVITY ELM SAVEAREA                        
SVPID    DS    XL2                 CHANGER'S PID                                
SVSCID   DS    XL8                 CHANGER'S SECURITY PID                       
SVSECAGY DS    XL(L'FATAGYSC)      SECURITY AGENCY                              
ALINCUR  DS    A                   A(CURSOR)                                    
*                                                                               
DELFLAG  DS    C                   "D"=ELEMENT IS TO BE DELETED                 
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
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
LUBLK    DS    XL(LUBLKL)          LINUP CONTROL BLOCK                          
         DS    0F                                                               
LINDSPS  DS    XL((NLINS+1)*2)                                                  
SVLSVTAB DS    XL(NLINS*LSVTABL)      HOLD COPY OF LINUP SAVE TABLE             
ELTMAX   EQU   (3900/130)-1       MAX NUMBER OF ELEMENTS IN TABLE               
*                                                                               
SAVEBASE DS    XL64                DOMAIN NAME SAVEAREA                         
SAVEBASL DS    XL1                 DOMAIN NAME LENGTH                           
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
LPUB     DS    CL15                PUBLICATION NUMBER ZONE/EDT                  
         DS    CL1                                                              
LPUBN    DS    CL20                PUB NAME                                     
         DS    CL4                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL2                                                              
         DS    CL2                                                              
LPRD     DS    CL3                 PRODUCT                                      
         DS    CL2                                                              
*                                                                               
*        REPORT LINE                                                            
*                                                                               
PLINED   DSECT                     PRINT LINE                                   
PLINE    DS    0CL132              PRINT/LIST LINE                              
PPUB     DS    CL15                PUBLICATION NUMBER ZONE/EDT                  
         DS    CL1                                                              
PPUBN    DS    CL20                PUB NAME                                     
         DS    CL1                                                              
PCLT     DS    CL3                 CLIENT                                       
         DS    CL1                                                              
PPRD     DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
PCNAME   DS    CL20                CONTACT NAME                                 
         DS    CL1                                                              
PCADDR   DS    CL57                CONTACT ADDRESS                              
         DS    CL1                                                              
PFYI     DS    CL3                 FYI                                          
         DS    CL2                                                              
PWOCOSTS DS    CL1                 W/O COSTS                                    
         DS    CL2                                                              
         EJECT                                                                  
       ++INCLUDE PPGENCCL                                                       
         EJECT                                                                  
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*DDFLDHDR                                                                       
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
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL2                 SORT VALU E - LINE # AND SUB LINE #          
LSVKEYL  EQU   *-LSVTABD                                                        
LSVKEYNW DS    XL2                 NEW KEY AFTER WRITING RECORD                 
LSVTABL  EQU   *-LSVTABD                                                        
         SPACE 2                                                                
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL2                 SORT VALUE - LINE # SUB LINE #               
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTKEYNW DS    XL2                 NEW KEY AFTER WRITING RECORD                 
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTELEM  DS    CL(CCVLSTLQ+79)        CONTACT ELEMENT                           
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'169PRSFM31   03/16/09'                                      
         END                                                                    
         TITLE 'T41D30 - INVOICE COMMENTS MAINT/LIST - TRNPID'                  
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1       A(PID)                                              *         
*        P2+0(1)  L'RETURN AREA  IF R2 = 0                            *         
*        P2+1(3)  A(RETURN AREA) IF R2 = 0                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VTRNPID  NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         L     R5,0(R1)            POINT TO PID                                 
*                                                                               
         SR    R6,R6                                                            
         IC    R6,4(R1)            SAVE LENGTH OF RETURN AREA                   
         L     R3,4(R1)            POINT TO RETURN AREA                         
*                                                                               
         LTR   R2,R2               IF R2 GIVEN                                  
         BZ    *+8                                                              
         LA    R3,FLDDATA             USE DATAAREA OF SCREEN FIELD              
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
         BRAS  RE,CLRFLD           INIT OUTPUT                                  
*                                                                               
         LA    R4,KEY                                                           
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,QAGY           USE BUYREC'S AGENCY                       
*                                                                               
         MVC   CT0KNUM,0(R5)       SET PID                                      
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TPIDNOTF                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,QAGY           USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,2(RE)       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE SKIP IF REC NOT FOUND           
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND NAME ELEMENT                                                      
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENOT FOUND                          
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WORK             BUILD NAME IN WORKAREA                       
         XC    WORK,WORK                                                        
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,WORK+7           POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WORK             START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,(R0) SQUASH NAME                              
*                                                                               
*        MOVE NAME TO SCREEN                                                    
*                                                                               
         LTR   R2,R2               IF NO SCREEN FIELD GIVEN                     
         BNZ   TPIDSCR                                                          
*                                                                               
         LR    RF,R6                  GET RETURN AREA LENGTH                    
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES         INIT OUT PUT AREA                         
*                                                                               
         L     RF,4(R1)               SAVE SQUASHED LENGTH                      
*                                                                               
         CR    RF,R6                  IF NAME TOO LONG                          
         BNH   *+6                                                              
         LR    RF,R6                     USE MAX FOR RETURN AREA                
*                                                                               
         B     TPIDMVC                                                          
*                                                                               
TPIDSCR  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         STC   RF,FLDOLEN          SET MAX OUTPUT LENGTH                        
*                                                                               
TPIDMVC  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY NAME                                 
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
