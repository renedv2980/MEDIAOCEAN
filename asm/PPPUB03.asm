*          DATA SET PPPUB03    AT LEVEL 030 AS OF 12/19/11                      
*        CHANGE LOG                                                             
*                                                                               
*  BPLA  12/11     ACCEPT 100.000 IN AGY COM - CARRIED AS P'-1'                 
*                                                                               
*  SMYE  11/99     ADDED EXCLUSIONS FIELD (MPDEXCL) AS IN PPPUB02               
*                    (NEWSPAPER PRODUCTION SCREEN)                              
*                                                                               
*  SMYE  08/14/97  DISPLAY DEFAULT VALUES FOR PUBAC IF PUBGENEL NOT             
*                  FOUND (AT PUBFLDS)                                           
*                                                                               
*  SMYE  1/97      FIXED BUG IN "FREQUENCY CODE" TO ADD ELEMENT IF THIS         
*                  (OR ANY OTHER) FIELD IS THE ONLY ONE ENTERED                 
*                  ALSO REDISPLAY ON ADDS OR CHANGES                            
*                                                                               
*  SMYE  12/96     CHANGED DEFAULT VALUES FOR PUBAC (AT EDITPUB)                
*                                                                               
*  SMYE  2/96      INCLUDE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                
*                                                                               
*  SMYE  12/07/95  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                   
*                                                                               
*  BPLA  12/14/90  CD EFFECTIVE DATE NOT BEING CLEARED WHEN SWITCHING           
*                  PUBS                                                         
*                                                                               
*PHASE T40603A                                                                  
         TITLE 'T40603 PUB LFM MAGAZINE PRODUCTION SCREEN'                      
         PRINT NOGEN                                                            
T40603   CSECT                                                                  
         NMOD1 0,T40603                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T406FFD,RA                                                       
         EJECT                                                                  
RDPUB    LA    R3,53                                                            
         LA    R2,PBLPUBH                                                       
         OC    PUBADDR,PUBADDR                                                  
         BZ    ERROR                                                            
         LA    R4,IOAREA                                                        
         LH    R5,=H'4000'                                                      
         BAS   RE,CLEARWRK                                                      
         LA    R4,ELEAREA                                                       
         LH    R5,=H'500'                                                       
         BAS   RE,CLEARWRK                                                      
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
*                                  PUBREC NOW IN IOAREA                         
*                                                                               
         LA    R9,IOAREA                                                        
         USING PUBREC,R9                                                        
         MVI   PUBIND,0            ZERO PUBINDICATOR                            
         MVI   REDSW,0             TURN OFF "REDISPLAY" SWITCH                  
*                                                                               
*                                                                               
SETPUB   LA    R6,PUBREC+33                                                     
CKPUB    CLI   0(R6),X'20'                                                      
         BNE   NEXTP                                                            
         OI    PUBIND,X'10'        PUBGENEL EXISTS                              
         B     CKINDS                                                           
*                                                                               
*                                                                               
NEXTP    CLI   0(R6),0                                                          
         BE    CKINDS                                                           
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CKINDS                                                           
         B     CKPUB                                                            
*                                                                               
CKINDS   LA    R3,COMBERR                                                       
         CLI   BACT,1                                                           
         BNE   PRDSCRN                                                          
         TM    PUBIND,X'10'                                                     
         BO    ERROR                                                            
         B     PRDSCRN                                                          
*                                                                               
*                                                                               
PRDSCRN  CLI   FORMAT,1                                                         
         BE    FORMATP                                                          
*                        MAG PRODUCTION SCREEN IN TWA SO EDIT IT UNLESS         
*                        ACTION=SRDS OR DISPLAY                                 
         CLI   BACT,2                                                           
         BH    FORMATP                                                          
*                        R6 POINTS TO PUBGENEL IF IT EXISTS,OTHERWISE           
*                         TO END OF PUBREC                                      
*                                                                               
EDITPUB  TM    PUBIND,X'10'                                                     
         BO    *+8                                                              
         LA    R6,ELEAREA                                                       
         MVI   ACTSW,0                                                          
         USING PUBPRDD,R6                                                       
         LA    R2,MPDACOMH                                                      
         LA    R3,COMERR                                                        
*                                                                               
         MVC   PUBAC(3),=P'15000'  DEFAULT AGY COMM IS 15% FOR ALL              
*                                                                               
*      * CLI   BMED,C'O'           OUTDOOR ?                                    
*      * BNE   EDITPUBA                                                         
*      * CLI   ANATION,C'C'        CANADIAN ?                                   
*      * BE    EDITPUBA                                                         
*      * MVC   PUBAC(3),=P'16667'  OUTDOOR AND NOT CANADIAN                     
*                                                                               
EDITPUBA CLI   5(R2),0                                                          
         BE    EDITP1                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(3,MPDACOM),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
**NEW 3/9/89                                                                    
         CP    DUB,=P'100000'         CARRY 100.000 AS -.001                    
         BNE   EDITP0                                                           
         ZAP   PUBAC,=P'-1'                                                     
         MVI   ACTSW,1                                                          
         B     EDITP1                                                           
**NEW 3/9/89                                                                    
EDITP0   CP    DUB,=P'99999'                                                    
         BH    ERROR                                                            
         MVC   PUBAC(3),DUB+5                                                   
         MVI   ACTSW,1                                                          
EDITP1   LA    R2,MPDCDISH                                                      
         LA    R3,2                INVALID INPUT FIELD                          
         MVC   PUBCD,=PL2'0'                                                    
         CLI   5(R2),0                                                          
         BE    EDITP1C                                                          
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,MPDCDIS),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         A     R5,FIVE                                                          
         CVD   R5,DUB                                                           
         DP    DUB,=P'10'                                                       
         CP    DUB(6),=P'200'                                                   
         BH    ERROR                                                            
         MVC   PUBCD(2),DUB+4                                                   
         MVI   ACTSW,1                                                          
*                                                                               
EDITP1C  DS    0H                  CD EFFECTIVE DATE                            
         LA    R2,MPDCDEDH                                                      
         XC    PUBCDDAT,PUBCDDAT                                                
         CLI   5(R2),0                                                          
         BE    EDITP1G                                                          
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,PUBCDDAT)                                
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PUBCDDAT)                               
         MVI   ACTSW,1                                                          
EDITP1G  EQU   *                                                                
         LA    R2,MPDCSHDH         CASH DISC DAYS                               
         ZAP   PUBCDDAS,=PL2'0'                                                 
         CLI   5(R2),0                                                          
         BE    EDITP2                                                           
         BAS   RE,ANY                                                           
         BAS   RE,PACK                                                          
         ZAP   PUBCDDAS,DUB+6(2)                                                
         MVI   ACTSW,1                                                          
EDITP2   LA    R2,MPDCDMOH                                                      
         MVC   PUBCLMO(2),=PL2'0'                                               
         CLI   5(R2),0                                                          
         BE    EDITP3                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,MPDCDMO),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER 0                         
         BNE   ERROR                                                            
         MVC   PUBCLMO(2),DUB+4                                                 
         MVI   ACTSW,1                                                          
EDITP3   LA    R2,MPDCDDAH                                                      
         MVC   PUBCLDA(2),=PL2'0'                                               
         CLI   5(R2),0                                                          
         BE    EDITPM1                                                          
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,MPDCDDA),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'     ZERO REMAINDER                                
         BNE   ERROR                                                            
         CP    DUB+4(2),=P'31'                                                  
         BH    ERROR                                                            
         CP    PUBCLMO(2),=P'0'                                                 
         BNL   EDITP3A                                                          
         CP    DUB+4(2),=P'0'                                                   
         BL    ERROR                                                            
EDITP3A  MVC   PUBCLDA(2),DUB+4                                                 
         MVI   ACTSW,1                                                          
EDITPM1  LA    R2,MPDMCLMH         MAT. CLOSING DATE (MONTH)******              
         MVC   PUBMCLMO(2),=PL2'0'                  ADD 12/03/87 *              
         CLI   5(R2),0                                           *              
         BE    EDITPM2                                           *              
         SR    R5,R5                                             *              
         IC    R5,5(R2)                                          *              
         GOTO1 VCASHVAL,DMCB,(0,MPDMCLM),(R5)                    *              
         CLI   DMCB,X'FF'                                        *              
         BE    ERROR                                             *              
         L     R5,DMCB+4                                         *              
         CVD   R5,DUB                                            *              
         DP    DUB,=P'100'                                       *              
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER 0          *              
         BNE   ERROR                                             *              
         MVC   PUBMCLMO(2),DUB+4                                 *              
         MVI   ACTSW,1                                           *              
EDITPM2  LA    R2,MPDMCLDH         MAT. CLOSING DATE (DAY)       *              
         MVC   PUBMCLDA(2),=PL2'0'                               *              
         CLI   5(R2),0                                           *              
         BE    EDITP4                                            *              
         SR    R5,R5                                             *              
         IC    R5,5(R2)                                          *              
         GOTO1 VCASHVAL,DMCB,(0,MPDMCLD),(R5)                    *              
         CLI   DMCB,X'FF'                                        *              
         BE    ERROR                                             *              
         L     R5,DMCB+4                                         *              
         CVD   R5,DUB                                            *              
         DP    DUB,=P'100'                                       *              
         CP    DUB+6(2),=P'0'     ZERO REMAINDER                 *              
         BNE   ERROR                                             *              
         CP    DUB+4(2),=P'31'                                   *              
         BH    ERROR                                             *              
         CP    PUBMCLMO(2),=P'0'                                 *              
         BNL   EDITPM2A                                          *              
         CP    DUB+4(2),=P'0'                                    *              
         BL    ERROR                                             *              
EDITPM2A MVC   PUBMCLDA(2),DUB+4                                 *              
         MVI   ACTSW,1                                ************              
EDITP4   LA    R2,MPDOSMOH                                                      
         MVC   PUBOSMO(2),=PL2'0'                                               
         CLI   5(R2),0                                                          
         BE    EDITP5                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,MPDOSMO),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER 0                         
         BNE   ERROR                                                            
         MVC   PUBOSMO(2),DUB+4                                                 
         MVI   ACTSW,1                                                          
EDITP5   LA    R2,MPDOSDAH                                                      
         MVC   PUBOSDA(2),=PL2'0'                                               
         CLI   5(R2),0                                                          
         BE    EDITP6                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,MPDOSDA),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'     ZERO REMAINDER                                
         BNE   ERROR                                                            
         CP    DUB+4(2),=P'31'                                                  
         BH    ERROR                                                            
         CP    PUBOSMO(2),=P'0'                                                 
         BNL   EDITP5A                                                          
         CP    DUB+4(2),=P'0'                                                   
         BL    ERROR                                                            
EDITP5A  MVC   PUBOSDA(2),DUB+4                                                 
         MVI   ACTSW,1                                                          
*                                                                               
*                                                                               
EDITP6   LA    R2,MPDFREQH                                                      
         XC    PUBMFREQ(2),PUBMFREQ                                             
         CLI   5(R2),0                                                          
         BE    EDITP7                                                           
*                                                                               
FREQOK   MVC   PUBMFREQ(2),8(R2)                                                
         OC    PUBMFREQ(2),SPACES                                               
         MVI   ACTSW,1                                                          
         B     EDITP7                                                           
*                                                                               
*                                                                               
*                                                                               
EDITP7   LA    R2,MPDPAMOH                                                      
         MVC   PUBPAYMO(2),=PL2'0'                                              
         CLI   5(R2),0                                                          
         BE    EDITP8                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,MPDPAMO),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'                                                   
         BNE   ERROR                                                            
         MVC   PUBPAYMO(2),DUB+4                                                
         MVI   ACTSW,1                                                          
EDITP8   LA    R2,MPDPADAH                                                      
         MVI   DATSW,0                                                          
         MVC   PUBPAYDA(2),=PL2'0'                                              
         CLI   5(R2),0                                                          
         BE    EDITP9                                                           
         CLI   8(R2),C'+'          IS IT A 'PLUS' NUMBER                        
         BNE   EDITP8A                                                          
         CP    PUBPAYMO,=P'0'                                                   
         BNE   ERROR               IF USING PLUS DAYS                           
*                                  MONTH MUST BE ZERO                           
         MVI   8(R2),X'F0'                                                      
         MVI   DATSW,C'Y'                                                       
EDITP8A  SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,MPDPADA),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'     ZERO REMAINDER                                
         BNE   ERROR                                                            
         CP    DUB+4(2),=P'31'                                                  
         BH    ERROR                                                            
         CP    PUBPAYMO(2),=P'0'                                                
         BNL   EDITP8B                                                          
         CP    DUB+4(2),=P'0'                                                   
         BL    ERROR                                                            
EDITP8B  MVC   PUBPAYDA(2),DUB+4                                                
         MVI   ACTSW,1                                                          
         CLI   DATSW,C'Y'          IS IT A 'PLUS' NUMBER                        
         BNE   EDITP9                                                           
         OI    PUBPAYDA,X'F0'      YES/SET ON BYTE                              
EDITP9   LA    R2,MPDCLASH                                                      
         XC    PUBMCLAS,PUBMCLAS                                                
         CLI   5(R2),0                                                          
         BE    EDITP10                                                          
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,MOVECL                                                        
         MVI   ACTSW,1                                                          
         B     EDITP10                                                          
*                                                                               
MOVECL   MVC   PUBMCLAS(0),8(R2)                                                
*                                                                               
*                                                                               
EDITP10  LA    R2,MPDEXCLH                                                      
         MVI   PUBEXCL,0                                                        
         CLI   5(R2),0                                                          
         BE    OUTPUT                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
*                                                                               
         LA    R1,MPDEXCL                                                       
FINDEXCL LA    R4,EXCLTAB                                                       
CKEXCL   CLC   0(1,R1),0(R4)                                                    
         BE    MOVEEXCL                                                         
         CLC   2(2,R4),=X'0000'                                                 
         BE    ERROR                                                            
         LA    R4,2(R4)                                                         
         B     CKEXCL                                                           
*                                                                               
MOVEEXCL OC    PUBEXCL,1(R4)                                                    
         MVI   ACTSW,1                                                          
         BCTR  R5,R0                                                            
         BCTR  R5,R0                                                            
         LTR   R5,R5                                                            
         BNP   OUTPUT                                                           
         CLI   1(R1),C','                                                       
         BNE   ERROR                                                            
         LA    R1,2(R1)                                                         
         B     FINDEXCL                                                         
*                B   W   L   T   C                                              
EXCLTAB  DC    X'C280E640D320E310C3080000'                                      
*                                                                               
*                                                                               
OUTPUT   CLI   ACTSW,1                                                          
         BNE   CKDELE                                                           
         TM    PUBIND,X'10'        SEE IF ELEMENT EXISTED                       
         BO    WRTREC                                                           
         MVC   PUBGENEL(2),=X'2032'                                             
         LA    R7,PUBREC+33                                                     
         CLI   0(R7),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'      NO NAME ELEMENT                                        
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         GOTO1 VRECUP,DMCB,(1,PUBREC),PUBGENEL,0(R7)                            
         B     WRTREC                                                           
*                                                                               
CKDELE   TM    PUBIND,X'10'                                                     
         BNO   DONE                                                             
         GOTO1 VRECUP,DMCB,(1,PUBREC),PUBGENEL                                  
         B     WRTREC                                                           
*                                                                               
*                                                                               
WRTREC   MVC   KEY+27(4),PUBADDR                                                
         SR    R5,R5                                                            
         IC    R5,PUBREC+25                                                     
         SLL   R5,8                                                             
         IC    R5,PUBREC+26                                                     
         SR    RE,RE                                                            
         LA    RE,PUBREC                                                        
         AR    RE,R5                                                            
         SR    RF,RF                                                            
         LA    RF,PUBREC+1999                                                   
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         BAS   RE,PUTPUB                                                        
         CLI   ACTSW,1             WAS THIS AN ADD OR CHANGE ?                  
         BNE   DONE                NO                                           
         OI    PUBIND,X'10'        PUBGENEL EXISTS                              
         MVI   REDSW,1             TO INDICATE "REDISPLAY"                      
         B     PUTFLDS                                                          
*****    B     DONE                                                             
         EJECT                                                                  
FORMATP  CLI   SAVSCRN,X'03'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1                                                           
         BNE   FMT5                                                             
         CLI   MPDACOMH+5,0        CK FOR INPUT                                 
         BE    FMT5                                                             
         MVI   BYTE2,0                                                          
         B     EDITPUB                                                          
*                                                                               
FMT2     LA    R4,PBLLAST                                                       
         GOTO1 VCALLOV,WORK,(R4),X'D90406F3'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'03'                                                    
FMT5     DS    0H                                                               
         CLI   BACT,1              SEE IF ADD                                   
         BNE   PUTFLDS                                                          
         B     PUTF20                                                           
*                                                                               
PUBFLDS  EQU   *                                                                
         LA    R2,MPDACOMH                                                      
         LA    R4,15               NUMBER OF MAP INPUT FIELDS                   
PUTF10   TM    1(R2),X'20'                                                      
         BO    PUTF15              YES                                          
         ZIC   R5,0(R2)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         FOUT  (R2)                                                             
         BCT   R4,PUTF15                                                        
*                                                                               
* DISPLAY AGENCY COMMISSION        DEFAULT AGY COMMISSION IS 15%                
*                                                                               
         MVC   MPDACOM(7),=C'15.000 '                                           
         FOUT  MPDACOMH                                                         
*                                                                               
*      * CLI   BMED,C'O'           OUTDOOR ?                                    
*      * BNE   PUTF20                                                           
*      * CLI   ANATION,C'C'        CANADIAN ?                                   
*      * BE    PUTF20                                                           
*      * MVC   MPDACOM(7),=C'16.667 '      OLD                                  
*                                                                               
         B     PUTF20                                                           
*                                                                               
PUTF15   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),8                                                          
         BH    PUTF10                                                           
*                                                                               
PUTF20   DS    0H                                                               
         CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   PROTECT                                                          
         LA    R2,MPDACOMH                                                      
         B     EXIT                                                             
         EJECT                                                                  
PUTFLDS  TM    PUBIND,X'10'        SEE IF PUBGENEL EXISTS                       
         BNO   PUBFLDS          NO- SO SEND FORMATED PUB FIELDS                 
         MVC   PACKED3(3),PUBAC                                                 
**NEW 3/9/89                                                                    
         CP    PUBAC,=P'-1'        -.001 MEANS 100.00 PCT                       
         BNE   PUTFL3                                                           
         MVC   MPDACOM(7),=C'100.000'                                           
         B     PUTFL3C                                                          
**NEW 3/9/89                                                                    
PUTFL3   EDIT  PACKED3,(7,MPDACOM),3,ALIGN=LEFT                                 
PUTFL3C  FOUT  MPDACOMH                                                         
         MVC   PACKED2(2),PUBCD                                                 
         EDIT  PACKED2,(4,MPDCDIS),1,ALIGN=LEFT                                 
         FOUT  MPDCDISH                                                         
         XC    MPDCDED,MPDCDED                                                  
         OC    PUBCDDAT,PUBCDDAT                                                
         BZ    PUTFL5                                                           
*        GOTO1 VDTCNV,DMCB,(1,PUBCDDAT),(3,MPDCDED)                             
         GOTO1 VDATCON,DMCB,(3,PUBCDDAT),(5,MPDCDED)                            
PUTFL5   FOUT  MPDCDEDH                                                         
         MVC   PACKED2,PUBCDDAS                                                 
         EDIT  PACKED2,(3,MPDCSHD),ALIGN=LEFT                                   
         FOUT  MPDCSHDH                                                         
         MVC   PACKED2(2),PUBCLMO                                               
         EDIT  PACKED2,(3,MPDCDMO),FLOAT=-,ALIGN=LEFT                           
         FOUT  MPDCDMOH                                                         
         MVC   PACKED2(2),PUBCLDA                                               
         EDIT  PACKED2,(3,MPDCDDA),FLOAT=-,ALIGN=LEFT                           
         FOUT  MPDCDDAH                                                         
         MVC   PACKED2(2),PUBMCLMO                    ***************           
         EDIT  PACKED2,(3,MPDMCLM),FLOAT=-,ALIGN=LEFT *             *           
         FOUT  MPDMCLMH                               * ADD 12/03/87*           
         MVC   PACKED2(2),PUBMCLDA                    *             *           
         EDIT  PACKED2,(3,MPDMCLD),FLOAT=-,ALIGN=LEFT *             *           
         FOUT  MPDMCLDH                               ***************           
         MVC   PACKED2(2),PUBOSMO                                               
         EDIT  PACKED2,(3,MPDOSMO),FLOAT=-,ALIGN=LEFT                           
         FOUT  MPDOSMOH                                                         
         MVC   PACKED2(2),PUBOSDA                                               
         EDIT  PACKED2,(3,MPDOSDA),FLOAT=-,ALIGN=LEFT                           
         FOUT  MPDOSDAH                                                         
*                                                                               
         FOUT  MPDFREQH,PUBMFREQ                                                
         MVC   PACKED2(2),PUBPAYMO                                              
         EDIT  PACKED2,(3,MPDPAMO),FLOAT=-,ALIGN=LEFT                           
         FOUT  MPDPAMOH                                                         
         MVC   PACKED2(2),PUBPAYDA                                              
         TM    PACKED2,X'F0'                                                    
         BZ    PUTFL6                                                           
         NI    PACKED2,X'0F'                                                    
         EDIT  PACKED2,(3,MPDPADA),FLOAT=+,ALIGN=LEFT                           
         B     PUTFL7                                                           
PUTFL6   EDIT  PACKED2,(3,MPDPADA),FLOAT=-,ALIGN=LEFT                           
PUTFL7   FOUT  MPDPADAH                                                         
         FOUT  MPDCLASH,PUBMCLAS                                                
*                                                                               
         LA    R1,MPDEXCL                                                       
         XC    MPDEXCL,MPDEXCL                                                  
         LA    R4,EXCLTAB1                                                      
CKBITS   MVC   WORK(1),PUBEXCL                                                  
         NC    PUBEXCL,0(R4)                                                    
         CLC   PUBEXCL,WORK                                                     
         BE    NEXTBT                                                           
         MVC   0(1,R1),1(R4)                                                    
         OC    PUBEXCL(1),PUBEXCL                                               
         BZ    PUTFLDS1                                                         
         MVI   1(R1),C','                                                       
         LA    R1,2(R1)                                                         
*                                                                               
NEXTBT   CLC   2(2,R4),=X'0000'                                                 
         BE    PUTFLDS1                                                         
         LA    R4,2(R4)                                                         
         OC    PUBEXCL(1),PUBEXCL                                               
         BZ    PUTFLDS1                                                         
         B     CKBITS                                                           
*                  B   W   L   T   C                                            
EXCLTAB1 DC    X'7FC2BFE6DFD3EFE3F7C30000'                                      
*                                                                               
PUTFLDS1 FOUT  MPDEXCLH                                                         
*                                                                               
         CLI   BACT,3              SRDS                                         
         BE    PROTECT                                                          
         CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   PROTECT                                                          
         CLI   REDSW,1             REDISPLAYING PUBGENEL ?                      
         BE    DONE                YES                                          
         LA    R2,MPDACOMH                                                      
         B     EXIT                                                             
PROTECT  OI    MPDFREQH+1,X'20'                                                 
         OI    MPDPAMOH+1,X'20'                                                 
         OI    MPDPADAH+1,X'20'                                                 
         OI    MPDCLASH+1,X'20'                                                 
         OI    MPDACOMH+1,X'20'                                                 
         OI    MPDCDISH+1,X'20'                                                 
         OI    MPDCDEDH+1,X'20'                                                 
         OI    MPDCSHDH+1,X'20'                                                 
         OI    MPDCDMOH+1,X'20'                                                 
         OI    MPDCDDAH+1,X'20'                                                 
         OI    MPDMCLMH+1,X'20'             ADD 12/03/87                        
         OI    MPDMCLDH+1,X'20'             ADD 12/03/87                        
         OI    MPDOSMOH+1,X'20'                                                 
         OI    MPDOSDAH+1,X'20'                                                 
         OI    MPDEXCLH+1,X'20'                                                 
         B     DONE                                                             
*                                                                               
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
FIVE     DC    F'5'                                                             
SPACES   DC    40C' '                                                           
PUBIND   DS    CL1                                                              
LTLIND   DS    CL1                                                              
ACTSW    DS    CL1                                                              
LTLSW    DS    CL1                                                              
REDSW    DS    CL1                 '1' = REDISPLAY PUBGENEL ELEM                
PACKED2  DS    PL2                                                              
PACKED3  DS    PL3                                                              
VIRGERR  DC    H'0'                                                             
COMBERR  EQU   112                                                              
COMERR   EQU   123                                                              
         EJECT                                                                  
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
         SPACE 2                                                                
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
         SPACE 2                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
         SPACE 2                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         SPACE 2                                                                
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBFILE)                    
         SPACE 3                                                                
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUTPUB   MVC   COMMAND,=C'PUTREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
ADDPUB   MVC   COMMAND,=C'ADDREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),IOAREA,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         STC   R3,ERRAREA          SAVE ERROR NUMBER                            
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DATSW    DS    CL1                                                              
ELEAREA  DS    500C                                                             
*                                                                               
*                                                                               
       ++INCLUDE PUGENOLD                                                       
*                                                                               
         DS    4000C                                                            
*                                                                               
       ++INCLUDE FLDIND                                                         
         ORG   BYTE2                                                            
FORMAT   DS    CL1                                                              
         ORG   KEY                                                              
KMED     DS    CL1                                                              
KPUB     DS    CL6                                                              
KAGY     DS    CL2                                                              
KRCD     DS    CL1                                                              
         DS    CL22                                                             
         EJECT                                                                  
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
         EJECT                                                                  
PUBPRDD  DSECT                                                                  
       ++INCLUDE PUBGENEL                                                       
         EJECT                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPUBFFD                                                       
         ORG PBLLAST                                                            
       ++INCLUDE PPPUBF3D                                                       
         ORG   T406FFD                                                          
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BSCR     DS    CL1                                                              
OLNUM    DS    CL1                                                              
PUBADDR  DS    F                                                                
LTLADDR  DS    F                                                                
BPUB     DS    CL6                                                              
BCLT     DS    CL3                                                              
BDIV     DS    CL3                                                              
BDATE    DS    CL3                                                              
APROF    DS    CL1                                                              
SAVSCRN  DS    CL1                                                              
APROF13  DS    CL1                                                              
BCODE    DS    CL3                                                              
         ORG   BCODE                                                            
BSPACE   DS    0CL17                                                            
         DS    CL1           X'FF'MEANS 3 PACKED FIELDS FOLLOW                  
BSHOW    DS    PL3                                                              
         DS    CL13                                                             
ANATION  DS    CL1                 'C' = CANADIAN                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030PPPUB03   12/19/11'                                      
         END                                                                    
