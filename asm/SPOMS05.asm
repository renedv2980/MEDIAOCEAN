*          DATA SET SPOMS05    AT LEVEL 026 AS OF 01/03/07                      
*PHASE T23405A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        SPOMS05 -- DARE FLIGHT RECORDS                       *         
*                                                                     *         
*  INPUTS:       SCREENS SPOMSF5 (MAINTENANCE)                        *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- SYSSPARE                                       *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
T23405   TITLE 'SPOMS05 - DARE FLIGHTS'                                         
***********************************************************************         
*============================= UPDATE LOG ============================*         
* DATE    LEV WHO  DESCRIPTION                                        *         
* ------- -- ----  -------------------------------------------------- *         
* 05FEB97     MHER ADD FIRST FLIGHT DATE CODE                         *         
***********************************************************************         
                                                                                
T23405   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T23405*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,XRECADD        AFTER AN ADD?                                
         BE    XRADD                                                            
         CLI   MODE,XRECPUT        AFTER A PUT?                                 
         BE    XRPUT                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         MVI   KEYFLAG,0                                                        
                                                                                
         LA    R2,FLMMEDH                                                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VKCLT                                                            
                                                                                
         MVI   GERROR1,REQFIELD                                                 
         CLI   5(R2),0             LENGTH OF INPUT = 0?                         
         BE    MISSFLD                                                          
                                                                                
         GOTO1 VALIMED             VALIDATE MEDIA                               
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
                                                                                
* CLIENT                                                                        
VKCLT    DS    0H                                                               
         LA    R2,FLMCLTH                                                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VKPRD                                                            
         XC    SVBCLT,SVBCLT                                                    
         CLI   ACTEQU,ACTLIST      LIST?                                        
         BNE   VKCLT10                                                          
         CLI   5(R2),0                                                          
         BE    VKPRD                                                            
                                                                                
VKCLT10  DS    0H                                                               
         GOTO1 VALICLT             VALIDATE CLIENT                              
         OI    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         OI    KEYFLAG,CLTCHNGD    CLIENT CHANGED                               
         MVC   SVBCLT,BCLT         BINARY CLIENT                                
                                                                                
* PRODUCT                                                                       
VKPRD    DS    0H                                                               
         LA    R2,FLMPRDH                                                       
         TM    KEYFLAG,CLTCHNGD    CLIENT CHANGED?                              
         BO    *+12                YES, SO REVALIDATE PRODUCT                   
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VKEST                                                            
         XC    SVQPRD,SVQPRD                                                    
         CLI   ACTEQU,ACTLIST      LIST?                                        
         BNE   VKPRD05                                                          
         MVC   SVQPRD,8(R2)                                                     
         OC    SVQPRD,SPACES                                                    
         B     VK10                                                             
                                                                                
VKPRD05  CLC   =C'ALL',8(R2)       PRODUCT = ALL?                               
         BNE   VKPRD10                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVBCLT                                                  
         MVC   KEY+4(3),=C'AAA'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PRDAAAER                                                         
         MVC   QPRD,=C'AAA'                                                     
         MVC   SVQPRD,=C'ALL'                                                   
         B     VKPRD20                                                          
                                                                                
VKPRD10  GOTO1 VALIPRD             VALIDATE PRODUCT                             
         MVC   SVQPRD,QPRD         EBCDIC PRODUCT                               
         MVC   SVBPRD,BPRD         BINARY PRODUCT                               
VKPRD20  OI    KEYFLAG,PRDCHNGD    PRODUCT CHANGED                              
                                                                                
* ESTIMATE                                                                      
VKEST    DS    0H                                                               
         CLI   ACTEQU,ACTLIST      LIST?                                        
         BE    VK10                                                             
         LA    R2,FLMESTH                                                       
         TM    KEYFLAG,PRDCHNGD    PRODUCT CHANGED?                             
         BO    *+12                YES, SO REVALIDATE ESTIMATE                  
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK10                                                             
         MVI   SVBEST,0                                                         
                                                                                
         GOTO1 VALIEST             VALIDATE ESTIMATE                            
         MVC   SVBEST,BEST         BINARY ESTIMATE                              
         OI    4(R2),X'20'         PREVIOUSLY VALIDATED                         
                                                                                
         LA    R2,FLMPRDH                                                       
         CLC   =C'POL',8(R2)                                                    
         BE    VKEST10                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVBCLT                                                  
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    POLERR                                                           
                                                                                
VKEST10  OI    4(R2),X'20'                                                      
                                                                                
* ESTIMATE DATES                                                                
         L     R4,AIO                                                           
         USING ESTHDRD,R4                                                       
         GOTO1 DATCON,DMCB,(0,ESTART),(5,FLMESD)                                
         MVI   FLMESD+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(0,EEND),(5,FLMESD+9)                                
         OI    FLMESDH+6,X'80'     TRANSMIT                                     
         GOTO1 DATCON,DMCB,(0,ESTART),(3,SVESTSTR)                              
         GOTO1 DATCON,DMCB,(0,EEND),(3,SVESTEND)                                
         DROP  R4                                                               
*                                                                               
VK10     CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VKX                 YES                                          
*                                                                               
         BAS   RE,ANYDORDS         ANY DARE ORDERS OUT THERE?                   
*                                                                               
         CLI   ACTEQU,ACTADD       ACTION ADD?                                  
         BNE   VK20                NO                                           
         TM    MISCFLG1,MF1GOTOR   HAVE A DARE ORDER SENT?                      
         BNZ   CANTADDF            YES, ERROR                                   
*                                                                               
VK20     XC    KEY,KEY             BUILD KEY FOR GENCON                         
         LA    R4,KEY                                                           
         USING DFLRECD,R4                                                       
         MVI   DFLKTYP,DFLKTYPQ    RECORD TYPE                                  
         MVI   DFLKSUB,DFLKSUBQ    SUB TYPE                                     
         MVC   DFLKAGMD,BAGYMD     BINARY AGENCY MEDIA                          
         MVC   DFLKCLT,SVBCLT      BINARY CLIENT                                
         MVC   DFLKPRD,SVQPRD      EBCDIC PRODUCT                               
         MVC   DFLKEST,SVBEST      BINARY ESTIMATE                              
         DROP  R4                                                               
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF WE HAVE ANY NON-FLIGHT DARE ORDERS FOR          
* THE MEDIA/CLIENT/ESTIMATE THAT HAVE BEEN SENT.                                
* WE'LL CHECK ALL PRODUCT CODES IF USER ENTERS 'ALL' OR 'POL' FOR PRD           
***********************************************************************         
ANYDORDS NTR1                                                                   
         NI    MISCFLG1,X'FF'-MF1GOTOR   NO DARE ORDERS YET                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,BAGYMD                                                   
         MVC   DCKCLT,SVBCLT       SET UP ORDER KEY UPTO CLIENT LEVEL           
*                                                                               
ANYD00   CLC   =C'POL',SVQPRD      GOING ACROSS PRODUCT CODES?                  
         BE    ANYD10                                                           
         CLC   =C'ALL',SVQPRD                                                   
         BE    ANYD10                                                           
         MVC   DCKPRD,SVBPRD       NO, FOR A SPECIFIC PRODUCT                   
*                                                                               
ANYD10   MVC   DCKEST,SVBEST                                                    
         XC    DCKSTA(6),DCKSTA                                                 
         GOTO1 HIGH                                                             
*                                                                               
ANYD15   CLC   KEY(DCKPRD-DOKEY),KEYSAVE   MAKE SURE UPTO CLIENT STILL          
         BNE   ANYDX                       NO                                   
*                                                                               
         CLC   =C'POL',SVQPRD      ACROSS PRODUCTS?                             
         BE    ANYD20                                                           
         CLC   =C'ALL',SVQPRD                                                   
         BE    ANYD20              YES                                          
         CLC   DCKPRD,SVBPRD       NO, MAKE SURE PRODUCT IS SAME THEN           
         BNE   ANYDX                                                            
*                                                                               
ANYD20   CLC   DCKEST,SVBEST                                                    
         BNE   ANYD30                                                           
         CLI   DCKFLTNM,0          ANY FLIGHT NUMBER ON THIS?                   
         BNE   ANYD40                                                           
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DOXMTELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   ANYD40                                                           
         OI    MISCFLG1,MF1GOTOR   WE HAVE A DARE ORDER SENT                    
         B     ANYDX                                                            
*                                                                               
ANYD30   BL    ANYD10              LOWER ESTIMATE, SET IT TO THE EST            
         XR    R1,R1               BUMP UP THE PRODUCT CODE                     
         IC    R1,DCKPRD           BUMP UP THE PRODUCT CODE                     
         CHI   R1,X'FF'                                                         
         BE    ANYD35                                                           
         LA    R1,1(R1)                                                         
         STC   R1,DCKPRD                                                        
         B     ANYD10                                                           
*                                                                               
ANYD35   ICM   R1,7,DCKCLT                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,7,DCKCLT                                                      
         B     ANYD10                                                           
*                                                                               
ANYD40   GOTO1 SEQ                                                              
         B     ANYD15                                                           
*                                                                               
ANYDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
         L     R4,AIO                                                           
         USING DFLRECD,R4                                                       
                                                                                
* CLIENT                                                                        
         GOTO1 CLUNPK,DMCB,DFLKCLT,FLMCLT                                       
         OI    FLMCLTH+6,X'80'                                                  
         MVC   BCLT,DFLKCLT                                                     
                                                                                
* PRODUCT                                                                       
         MVC   FLMPRD,DFLKPRD                                                   
         OI    FLMPRDH+6,X'80'                                                  
                                                                                
* ESTIMATE                                                                      
         EDIT  (B1,DFLKEST),(3,FLMEST),0,ALIGN=LEFT                             
         OI    FLMESTH+6,X'80'                                                  
         MVC   DRFLGTKY,0(R4)                                                   
                                                                                
* ESTIMATE DATES                                                                
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),DFLKCLT                                                 
         MVC   KEY+4(3),DFLKPRD                                                 
         CLC   DFLKPRD,=C'ALL'                                                  
         BNE   *+10                                                             
         MVC   KEY+4(3),=C'AAA'                                                 
         MVC   KEY+7(1),DFLKEST                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DKGET                                                            
         LA    R2,FLMESTH                                                       
         B     INVLESTM                                                         
         DROP  R4                                                               
                                                                                
DKGET    GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING ESTHDRD,R4                                                       
         GOTO1 DATCON,DMCB,(0,ESTART),(5,FLMESD)                                
         MVI   FLMESD+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(0,EEND),(5,FLMESD+9)                                
         OI    FLMESDH+6,X'80'     TRANSMIT                                     
         GOTO1 DATCON,DMCB,(0,ESTART),(3,SVESTSTR)                              
         GOTO1 DATCON,DMCB,(0,EEND),(3,SVESTEND)                                
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO1                                                         
         MVC   KEY,DRFLGTKY                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
DKX      B     XIT                                                              
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                           VALIDATE RECORD                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
VR       DS    0H                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    SVEND,SVEND                                                      
         L     R4,AIO                                                           
         USING DFLRECD,R4                                                       
         MVC   DFLKEY,KEY                                                       
         MVC   DFLRAGYA,TWAAGY     ALPHA AGY                                    
         DROP  R4                                                               
*                                                                               
         NI    MISCFLG1,X'FF'-MF1FLCKD                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VRFLT20                                                          
*                                                                               
         L     R6,AIO              REMOVE ALL FLIGHT ELEMENTS                   
         MVI   ELCODE,DFFLTELQ     ELEMENT CODE                                 
         BAS   RE,GETEL                                                         
         BNE   VRFLT20                                                          
         USING DFFLTEL,R6                                                       
VRFLT10  TM    DFFLTSTA,DFFLTSLK   LOCKED BY DARE?                              
         BZ    *+12                                                             
         OI    MISCFLG1,MF1FLCKD   WE HAVE A FLIGHT LOCKED BY DARE              
         B     *+8                                                              
         MVI   0(R6),X'FF'         NO, OTHERWISE MARK FOR DELETE                
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    VRFLT10                                                          
         MVI   ELCODE,X'FF'        REMOVE ALL FLIGHT ELEM WE MARKED             
         GOTO1 REMELEM                                                          
         DROP  R6                                                               
                                                                                
VRFLT20  MVI   FLTCOUNT,0                                                       
                                                                                
         LA    R2,FLMFL01H                                                      
         LA    R6,ELEM             BUILD ELEMENT                                
         USING DFFLTEL,R6          FLIGHT ELEMENT                               
VRFLT30  LA    RE,FLMFL16H                                                      
         CR    R2,RE                                                            
         BH    VR60                                                             
         CLI   5(R2),0             LENGTH                                       
         BE    VRFLT40                                                          
         XC    ELEM,ELEM                                                        
         MVI   DFFLTEL,DFFLTELQ    ELEMENT CODE                                 
         MVI   DFFLTLEN,DFFLTLNQ   LENGTH                                       
         ZIC   R1,FLTCOUNT                                                      
         LA    R1,1(R1)                                                         
         STC   R1,FLTCOUNT                                                      
         STC   R1,DFFLTNUM                                                      
                                                                                
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         LA    RE,8(R2)                                                         
         ST    RE,DMCB                                                          
         MVC   DMCB(1),5(R2)          LENGTH                                    
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,(R3)                                                
                                                                                
         CLI   4(R1),0             INVALID DATE?                                
         BNE   INVLDATE                                                         
         CLC   PVALBSTA,SVEND      DATES ASCEND?                                
         BNH   SEQERR                                                           
                                                                                
         CLC   PVALBSTA,SVESTSTR   CHECK IF DATE IN EST PERIOD                  
         BL    DATRNERR                                                         
         CLC   PVALBEND,SVESTEND                                                
         BH    DATRNERR                                                         
                                                                                
         MVC   DFFLTSTR,PVALBSTA   START DATE                                   
         MVC   DFFLTEND,PVALBEND   END DATE                                     
         MVC   SVEND,PVALBEND                                                   
*                                                                               
         SR    R0,R0                                                            
         L     R1,AIO              CAN'T CHANGE A FLIGHT ELEM THAT IS           
         LA    R1,DFLEL-DFLKEY(R1)     LOCKED BY DARE                           
VRFLT31  CLI   0(R1),0                                                          
         BE    VRFLT35                                                          
         CLI   0(R1),DFFLTELQ                                                   
         BH    VRFLT35                                                          
         BL    VRFLT32                                                          
VRFLT30D USING DFFLTEL,R1                                                       
         CLC   VRFLT30D.DFFLTNUM,DFFLTNUM   SAME FLIGHT NUMBER?                 
         BH    VRFLT35                                                          
         BL    VRFLT32             IF SAME, THE FLIGHT IS LOCKED                
         CLC   VRFLT30D.DFFLTSTR,DFFLTSTR   AND DATES SHOULD BE SAME            
         BNE   CANTCHNG                                                         
         CLC   VRFLT30D.DFFLTEND,DFFLTEND                                       
         BNE   CANTCHNG                                                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     VRFLT30                                                          
*                                                                               
VRFLT32  IC    R0,1(R1)            SKIP TO NEXT FLIGHT ELEMENT                  
         AR    R1,R0                                                            
         B     VRFLT31                                                          
         DROP  VRFLT30D                                                         
*                                                                               
VRFLT35  ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         GOTO1 ADDELEM                                                          
         B     VRFLT30                                                          
         DROP  R6                                                               
                                                                                
VRFLT40  DS    0H                  MAKE SURE THE REST ARE EMPTY FIELDS          
         LR    R3,R2               SAVE ADDRESS OF FIRST EMPTY FIELD            
VRFLT50  ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    RE,FLMFL16H                                                      
         CR    R2,RE                                                            
         BH    VR60                                                             
         CLI   5(R2),0             LENGTH                                       
         BE    VRFLT50                                                          
         LR    R2,R3                                                            
         B     SKIPERR                                                          
         EJECT                                                                  
*=====================================================*                         
* EDIT NON-FLIGHT END DATE                            *                         
*=====================================================*                         
VR60     TM    MISCFLG1,MF1FLCKD   LOCKED?                                      
         BNZ   VR70                YES                                          
*                                                                               
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,FLMFRSTH                                                      
         CLI   5(R2),0             ANYTHING IN THIS FIELD?                      
         BNE   VR65                                                             
         TM    MISCFLG1,MF1GOTOR   NO, DO WE HAVE DARE ORDERS?                  
         BZ    VR70                                                             
         B     MISSEDAT                YES, MISSING THEN                        
*                                                                               
VR65     LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         LA    RE,8(R2)                                                         
         ST    RE,DMCB                                                          
         MVC   DMCB(1),5(R2)          LENGTH                                    
         OI    DMCB,X'40'                                                       
         GOTO1 PERVAL,DMCB,,(R3)                                                
*                                                                               
         TM    4(R1),X'03'         INVALID DATE?                                
         BNZ   INVLDATE                                                         
*                                                                               
         CLC   PVALBSTA,SVESTSTR   CHECK IF DATE IN EST PERIOD                  
         BL    DATRNERR                                                         
         CLC   PVALBSTA,SVESTEND                                                
         BH    DATRNERR                                                         
*                                                                               
         LA    R6,ELEM                                                          
         USING DFINFEL,R6                                                       
         MVI   DFINFEL,DFINFELQ                                                 
         MVI   DFINFLEN,DFINFLNQ                                                
         MVC   DFINFSDT,PVALBSTA   1ST NON-FLIGHT END DATE                      
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DFFLTELQ     ELEMENT CODE                                 
         BAS   RE,GETEL                                                         
         BNE   VR70                                                             
*                                                                               
         USING DFFLTEL,R6                                                       
         CLC   PVALBSTA,DFFLTSTR                                                
         BNL   B41STFLT                                                         
         GOTO1 ADDAY,DMCB,PVALESTA,PVALEEND,1                                   
         GOTO1 DATCON,DMCB,(0,PVALEEND),(3,PVALBEND)                            
         CLC   PVALBEND,DFFLTSTR                                                
         BNE   INVLFLD                                                          
*                                                                               
VR70     L     R6,AIO                                                           
         MVI   ELCODE,DFFLTELQ     ELEMENT CODE                                 
         BAS   RE,GETEL                                                         
         BE    VRX                                                              
         LA    R2,FLMFL01H         NEED INFO FOR AT LEAST ONE WEEK              
         B     MISSFLD                                                          
*                                                                               
VRX      MVI   ACTELOPT,C'Y'       WANT GENCON TO ADD ACTIVITY ELEM             
         B     XIT                                                              
         DROP  R3,R6                                                            
***********************************************************************         
* AFTER AN ADD                                                                  
***********************************************************************         
XRADD    DS    0H                                                               
         MVI   ACTELOPT,C'N'       NEED TO RESET THIS VALUE                     
XRADDX   B     XIT                                                              
***********************************************************************         
* AFTER A PUT                                                                   
***********************************************************************         
XRPUT    DS    0H                                                               
         MVI   ACTELOPT,C'N'       NEED TO RESET THIS VALUE                     
XRPUTX   B     XIT                                                              
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                            DISPLAY RECORD                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DR       DS    0H                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DR05                                                             
         MVC   CONHED2+50(4),=CL4'D/A='                                         
         GOTO1 HEXOUT,DMCB,DMDSKADD,CONHED2+54,4                                
*                                                                               
DR05     NI    MISCFLG1,X'FF'-MF1FLCKD                                          
*                                                                               
         LA    R2,FLMFL01H                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,DFFLTELQ     ELEMENT CODE                                 
         BAS   RE,GETEL                                                         
         BNE   DRFLT20                                                          
                                                                                
         USING DFFLTEL,R6          FLIGHT ELEMENT                               
DRFLT10  LA    RE,FLMFL16H                                                      
         CR    R2,RE                                                            
         BH    DRX                                                              
                                                                                
         GOTO1 DATCON,DMCB,(3,DFFLTSTR),(5,8(R2))                               
         MVI   16(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,DFFLTEND),(5,17(R2))                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         TM    DFFLTSTA,DFFLTSLK   LOCKED BY DARE?                              
         BZ    *+12                                                             
         OI    6(R2),X'20'         CHANGE TO PROTECTED FOR NEXT INPUT           
         OI    MISCFLG1,MF1FLCKD                                                
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BAS   RE,NEXTEL                                                        
         BE    DRFLT10                                                          
                                                                                
DRFLT20  DS    0H                  CLEAR REST OF FIELDS ON SCREEN               
         LA    RE,FLMFL16H                                                      
         CR    R2,RE                                                            
         BH    DR10                                                             
         XC    8(L'FLMFL01,R2),8(R2)                                            
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     DRFLT20                                                          
         DROP  R6                                                               
*                                                                               
DR10     TM    MISCFLG1,MF1FLCKD                                                
         BZ    *+8                                                              
         OI    FLMFRSTH+6,X'20'                                                 
*                                                                               
         XC    FLMFRST,FLMFRST                                                  
         OI    FLMFRSTH+6,X'80'                                                 
         L     R6,AIO                                                           
         MVI   ELCODE,DFINFELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         USING DFINFEL,R6                                                       
         OC    DFINFSDT,DFINFSDT                                                
         BZ    DRX                                                              
         GOTO1 DATCON,DMCB,(3,DFINFSDT),(17,FLMFRST)                            
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                            LIST RECORD                              *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
LR       DS    0H                                                               
         LA    R4,KEY                                                           
         USING DFLRECD,R4                                                       
                                                                                
         OC    KEY(DFLRLEN-DFLKEY),KEY   FIRST TIME THROUGH?                    
         BNZ   LR10                                                             
                                                                                
* BUILD KEY                                                                     
         XC    KEY,KEY                                                          
         MVI   DFLKTYP,DFLKTYPQ    RECORD TYPE                                  
         MVI   DFLKSUB,DFLKSUBQ    SUB TYPE                                     
         MVC   DFLKAGMD,BAGYMD     MEDIA                                        
         MVC   DFLKCLT,SVBCLT      BINARY CLIENT                                
         MVC   DFLKPRD,SVQPRD      PRODUCT                                      
         MVC   SAVEKEY,KEY                                                      
                                                                                
LR10     GOTO1 HIGH                                                             
                                                                                
LR20     CLC   KEY(DFLKCLT-DFLKEY),SAVEKEY      A/M,CLT,PRD                     
         BNE   LRX                                                              
                                                                                
*********                                                                       
* FILTERING BY CLIENT                                                           
*********                                                                       
         CLI   FLLCLTH+5,0                                                      
         BE    LR30                                                             
         CLC   DFLKCLT,SVBCLT                                                   
         BNE   LR60                                                             
                                                                                
*********                                                                       
* FILTERING BY PRODUCT                                                          
*********                                                                       
LR30     CLI   FLLPRDH+5,0                                                      
         BE    LR40                                                             
         CLC   DFLKPRD,SVQPRD                                                   
         BNE   LR60                                                             
                                                                                
LR40     XC    LISTAR,LISTAR                                                    
                                                                                
         DROP  R4                                                               
         L     R6,AIO                                                           
         USING DFLRECD,R6                                                       
                                                                                
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 CLUNPK,DMCB,DFLKCLT,LCLT                                         
                                                                                
* PRODUCT                                                                       
         MVC   LPRD,DFLKPRD                                                     
                                                                                
* ESTIMATE                                                                      
         EDIT  (B1,DFLKEST),(3,LEST),FILL=0    ESTIMATE                         
         DROP  R6                                                               
                                                                                
* FLIGHT DATES                                                                  
         USING DFFLTEL,R6                                                       
         MVI   ELCODE,DFFLTELQ     FLIGHT ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   LR55                                                             
                                                                                
         MVC   SVFLTSTR,DFFLTSTR                                                
LR50     MVC   SVFLTEND,DFFLTEND                                                
         BAS   RE,NEXTEL                                                        
         BE    LR50                                                             
                                                                                
         GOTO1 DATCON,DMCB,(3,SVFLTSTR),(5,LFLTSTR)                             
         MVI   LFLTSTR+L'LFLTSTR,C'-'                                           
         GOTO1 DATCON,DMCB,(3,SVFLTEND),(5,LFLTEND)                             
         OI    6(R2),X'80'         TRANSMIT                                     
                                                                                
LR55     GOTO1 LISTMON             PRINT LINE                                   
                                                                                
LR60     GOTO1 SEQ                                                              
         B     LR20                                                             
         DROP  R6                                                               
                                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
                                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     MYERREX                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     MYERREX                                                          
*                                                                               
INVLDATE MVI   GERROR1,INVDATE     INVALID DATE                                 
         B     MYERREX                                                          
*                                                                               
CANTCHNG MVI   GERROR1,70          MAY NOT CHANGE THIS FIELD                    
         B     MYERREX                                                          
*                                                                               
INVLESTM MVI   GERROR1,71          INVALID ESTIMATE                             
         B     MYERREX                                                          
*                                                                               
DATRNERR MVI   GERROR1,97          DATE IS OUTSIDE OF ESIMATE RANGE.            
         B     MYERREX                                                          
*                                                                               
SEQERR   MVI   GERROR1,194         DATES MUST BE IN SEQUENCE                    
         B     MYERREX                                                          
*                                                                               
MISSEDAT MVI   GERROR1,206         ORDERS EXISTS, END DATE IS REQUIRED          
         B     MYERREX                                                          
*                                                                               
B41STFLT MVI   GERROR1,207         END DATE MUST BE BEFORE 1ST FLIGHT           
         B     MYERREX                                                          
*                                                                               
POLERR   MVI   GERROR1,252         PRODUCT MUST BE POL                          
         B     MYERREX                                                          
*                                                                               
SKIPERR  MVI   GERROR1,253         CANNOT SKIP THIS FIELD                       
         B     MYERREX                                                          
*                                                                               
PRDAAAER MVI   GERROR1,254         PRODUCT AAA DOES NOT EXIST                   
         B     MYERREX                                                          
*                                                                               
CANTADDF MVC   GERROR,=AL2(259)    CANNOT ADD THIS FLIGHT RECORD!               
         B     MYERREX                                                          
*                                                                               
CURSERR  MVI   GERROR1,REQFIELD                                                 
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVC   TIOBCURI,4(R3)                                                   
         DROP  R1                                                               
         B     MYERREX                                                          
                                                                                
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
MYERREX  GOTO1 MYERR                                                            
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
RELO     DS    A                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* DDBIGBOX                                                                      
* FAGETTXTD                                                                     
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* DDCOMFACSD                                                                    
* DMPRTQL                                                                       
* FAFACTS                                                                       
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* CTGENEDICT                                                                    
* FATIOB                                                                        
* SPGENAGY                                                                      
* SPGENCLT                                                                      
* SPGENEST                                                                      
* SPGENMKT                                                                      
* SPGENPRD                                                                      
* SPADAVCOM                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE CTGENEDICT                                                     
       ++INCLUDE SPGENDRFLT                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPADAVCOM                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSF5D          (OUR MAINTENANCE SCREEN)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSE5D          (OUR LIST SCREEN)                            
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRORD        (RECORD DSECTS)                              
         EJECT                                                                  
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
SVEND    DS    XL3                 SAVE END DATE OF PREVIOUS FLIGHT             
SVESTSTR DS    XL3                 SAVE ESTIMATE START DATE                     
SVESTEND DS    XL3                 SAVE ESTIMATE END DATE                       
SVFLTSTR DS    XL3                 SAVE FLIGHT START DATE                       
SVFLTEND DS    XL3                 SAVE FLIGHT END DATE                         
SVBCLT   DS    XL2                 SAVE BINARY CLIENT                           
SVQPRD   DS    CL3                 SAVE EBCDIC PRODUCT                          
SVBPRD   DS    XL1                 SAVE BINARY PRODUCT                          
SVBEST   DS    X                   SAVE BINARY ESTIMATE                         
FLTCOUNT DS    X                   FLIGHT COUNT                                 
*                                                                               
KEYFLAG  DS    X                                                                
CLTCHNGD EQU   X'80'               CLIENT FIELD CHANGED                         
PRDCHNGD EQU   X'80'               PRODUCT FIELD CHANGED                        
*                                                                               
MISCFLG1 DS    X                                                                
MF1FLCKD EQU   X'80'               WE HAVE A FLIGHT THAT IS LOCKED              
MF1GOTOR EQU   X'40'               WE HAVE DARE ORDERS FOR MED/CLT/EST          
*                                                                               
SAVEKEY  DS    XL18                                                             
DRFLGTKY DS    XL13                                                             
FAKEFLDH DS    CL8                 FAKE FIELD HEADER                            
FAKEFLD  DS    CL60                FAKE FIELD                                   
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                 CLIENT                                       
         DS    CL7                                                              
LPRD     DS    CL3                 PRODUCT                                      
         DS    CL8                                                              
LEST     DS    CL3                 ESTIMATE                                     
         DS    CL4                                                              
LFLTSTR  DS    CL8                 ESTIMATE DATES                               
         DS    C                                                                
LFLTEND  DS    CL8                                                              
LISTLNQ  EQU   *-LCLT              LENGTH OF ONE LINE ON LIST SCREEN            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPOMS05   01/03/07'                                      
         END                                                                    
