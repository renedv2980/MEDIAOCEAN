*          DATA SET PPPUB02    AT LEVEL 091 AS OF 12/19/11                      
*PHASE T40602A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T40602  PUB LFM  NEWSPAPER PRODUCTION SCREEN'                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BPLA 12/11    ALLOW 100.000 IN AGY COM - CARRIED AS P'-1' IN PUB              
*                                                                               
* KWAN 01/01    CHANGE DEFAULT AGENCY COMMISSION TO 15% FOR EVERYBODY           
*                                                                               
* SMYE 08/14/97 DISPLAY DEFAULT VALUES FOR PUBAC IF PUBGENEL NOT                
*               FOUND (AT PUTFLDS)                                              
*                                                                               
* SMYE 03/97    DISALLOW NEGATIVE PUBCD ENTRY                                   
*                                                                               
* SMYE 01/97    REDISPLAY SCREEN ON ELEMENT ADDS OR CHANGES                     
*                                                                               
* SMYE 12/96    CHANGED PUBAC DEFAULT TO P'15000' FROM P'0'                     
*                                                                               
* SMYE 02/96    INCLUDE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                   
*                                                                               
* SMYE 12/07/95 CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                      
*                                                                               
* BPLA 12/14/90 CD EFFECTIVE DATE NOT CLEARED FROM PUB TO PUB                   
*                                                                               
* ROSA 11/10/89 PUB CLASSIFICATION FIELD NOT CLEARED WHEN                       
*               SWITCHING FROM PUB TO PUB                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT NOGEN                                                            
T40602   CSECT                                                                  
         NMOD1 0,T40602                                                         
         L     RC,0(1)                                                          
         USING GENOLD,RC                                                        
         USING T406FFD,RA                                                       
*                                                                               
         LA    R7,T40602+4095                                                   
         LA    R7,1(R7)                                                         
         USING T40602+4096,R7      **** NOTE USE OF SECOND BASE                 
*                                                                               
         EJECT                                                                  
         LA    R4,IOAREA                                                        
         LA    R4,4000(R4)         POINT TO ELEAREA                             
         LH    R5,=H'500'                                                       
         BAS   RE,CLEARWRK                                                      
         LA    R4,IOAREA                                                        
         LA    R4,4000(R4)         POINT TO IOAREA2                             
         LA    R4,500(R4)                                                       
         LH    R5,=H'4000'                                                      
         BAS   RE,CLEARWRK                                                      
         LA    R4,IOAREA                                                        
         LH    R5,=H'4000'                                                      
         BAS   RE,CLEARWRK                                                      
         OC    LTLADDR,LTLADDR                                                  
         BZ    RDPUB                                                            
         MVC   KEY+27(4),LTLADDR                                                
         BAS   RE,GETPUB                                                        
         BAS   RE,MOVEIN                                                        
RDPUB    LA    R3,53               RECORD NOT FOUND                             
         LA    R2,PBLPUBH                                                       
         OC    PUBADDR,PUBADDR                                                  
         BZ    ERROR                                                            
         LA    R4,IOAREA                                                        
         LH    R5,=H'4000'                                                      
         BAS   RE,CLEARWRK                                                      
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
*                              LTLREC NOW IN IOAREA2                            
*                              PUBREC NOW IN IOAREA                             
*                                                                               
         LA    R9,IOAREA                                                        
         USING PUBREC,R9                                                        
         LA    R8,IOAREA                                                        
         LA    R8,4000(R8)         POINT TO IOAREA2                             
         LA    R8,500(R8)                                                       
         USING LTLREC,R8                                                        
         XC    PUBIND,PUBIND       CLEAR PUB INDICATOR                          
         XC    LTLIND,LTLIND       CLEAR LTL INDICATOR                          
         MVI   REDSW,0             TURN OFF REDISPLAY INDICATOR                 
         LA    R6,LTLREC+33                                                     
         OC    LTLADDR,LTLADDR                                                  
         BZ    SETPUB                                                           
         OI    LTLIND,X'01'                                                     
CKLTL    CLI   0(R6),X'21'                                                      
         BNE   NEXTL                                                            
         OI    LTLIND,X'10'                                                     
         B     SETPUB                                                           
*                                                                               
NEXTL    CLI   0(R6),0                                                          
         BE    SETPUB                                                           
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    SETPUB                                                           
         B     CKLTL                                                            
*                                                                               
SETPUB   ST    R6,LELADDR          SAVE ADDR OF 21 ELEM                         
*                                                                               
         LA    R6,PUBREC+33                                                     
CKPUB    CLI   0(R6),X'20'                                                      
         BNE   NEXTP                                                            
         OI    PUBIND,X'10'                                                     
         B     CKINDS                                                           
*                                                                               
NEXTP    CLI   0(R6),0                                                          
         BE    CKINDS                                                           
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CKINDS                                                           
         B     CKPUB                                                            
CKINDS   LA    R3,COMBERR                                                       
         CLI   BACT,1              SEE IF ADD                                   
         BNE   PRDSCRN                                                          
         TM    LTLIND,X'10'                                                     
         BNZ   ERROR                                                            
         TM    PUBIND,X'10'                                                     
         BNZ   ERROR               NEITHER INDICATOR SHOULD BE ON               
         B     PRDSCRN                                                          
*                                                                               
PRDSCRN  CLI   FORMAT,1                                                         
         BE    FORMATP                                                          
*               PRODUCTION SCREEN IN TWA SO EDIT IT UNLESS ACTION=SRDS          
*                      OR DISPLAY                                               
         CLI   BACT,2                                                           
         BH    FORMATP                                                          
*                                                                               
*                           R6 POINTS TO PUBGENEL IF IT EXISTS,IF NOT           
*                           IT POINTS TO END OF PUBREC - ELEAREA                
EDITPUB  TM    PUBIND,X'10'                                                     
         BO    *+12                                                             
         LA    R6,IOAREA                                                        
         LA    R6,4000(R6)         POINT TO ELEAREA                             
         MVI   ACTSW,0                                                          
         CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   STDERROR                                                         
         USING PUBPRDD,R6                                                       
         LA    R2,NPDACOMH                                                      
         LA    R3,COMERR                                                        
*****    MVC   PUBAC(3),=PL3'0'                                                 
         MVC   PUBAC(3),=P'15000'                                               
         CLI   5(R2),0                                                          
         BE    EDITP1                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(3,NPDACOM),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
**NEW 3/9/89                                                                    
         CP    DUB,=P'100000'         100.000 CARRY AS -.001                    
         BNE   EDITP0                 BY CONVENTION                             
         ZAP   PUBAC,=P'-1'                                                     
         MVI   ACTSW,1                                                          
         B     EDITP1                                                           
**NEW 3/9/89                                                                    
*                                                                               
EDITP0   CP    DUB,=P'99999'                                                    
         BH    ERROR                                                            
         MVC   PUBAC(3),DUB+5                                                   
         MVI   ACTSW,1                                                          
EDITP1   LA    R2,NPDCDISH                                                      
         LA    R3,2                INVALID INPUT FIELD                          
         MVC   PUBCD(2),=PL2'0'                                                 
         CLI   5(R2),0                                                          
         BE    EDITP1C                                                          
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,NPDCDIS),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         LTR   R5,R5               NEGATIVE ?                                   
         BM    ERROR               YES - ERROR                                  
         A     R5,FIVE                                                          
         CVD   R5,DUB                                                           
         DP    DUB,=P'10'                                                       
         CP    DUB(6),=P'200'        CDIS CANNOT EXCEED 20 PCT                  
         BH    ERROR                                                            
         MVC   PUBCD(2),DUB+4                                                   
         MVI   ACTSW,1                                                          
*                                                                               
EDITP1C  DS    0H                  EFFECTIVE DATE FOR CD                        
         XC    PUBCDDAT,PUBCDDAT                                                
         LA    R2,NPDCDEDH                                                      
         CLI   5(R2),0                                                          
         BE    EDITP1G                                                          
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         OC    DMCB,DMCB                                                        
         BZ    ERROR                                                            
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,PUBCDDAT)                                
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PUBCDDAT)                               
         MVI   ACTSW,1                                                          
EDITP1G  EQU   *                                                                
         LA    R2,NPDCSHDH         CASH DISC DAYS                               
         ZAP   PUBCDDAS,=PL2'0'                                                 
         CLI   5(R2),0                                                          
         BE    EDITP2                                                           
         BAS   RE,ANY                                                           
         BAS   RE,PACK                                                          
         ZAP   PUBCDDAS,DUB+6(2)                                                
         MVI   ACTSW,1                                                          
EDITP2   LA    R2,NPDCPPH                                                       
         MVC   PUBCPP(2),=PL2'0'                                                
         CLI   5(R2),0                                                          
         BE    EDITP3                                                           
         BAS   RE,ANY                                                           
         BAS   RE,PACK                                                          
         MVC   PUBCPP(2),DUB+6                                                  
         MVI   ACTSW,1                                                          
EDITP3   LA    R2,NPDLPCH                                                       
         MVI   PUBLPCI,0                                                        
         MVC   PUBLPC(2),=PL2'0'                                                
         CLI   5(R2),0                                                          
         BE    EDITP3M                                                          
         MVC   WORK(1),5(R2)       SAVE ORIGIONAL LENGHT                        
         MVC   WORK+1(6),8(R2)     AND INPUT                                    
         LA    R5,8(R2)                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         AR    R5,R1              SEE IF FIELD ENDS IN L OR I                   
         CLI   0(R5),C'L'                                                       
         BE    EDITP3F                                                          
         CLI   0(R5),C'I'                                                       
         BNE   EDITP3G                                                          
*                                 MUST CARRY INCHES WITH 2 DECIMALS             
*                                 PACKED WITHOUT SIGN                           
         MVI   0(R5),0                                                          
         STC   R1,5(R2)                                                         
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,NPDLPC),(R5)                                    
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'9999'           MAX IS 99.99 INCHES                       
         BH    ERROR                                                            
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
         MP    DUB,=P'10'                                                       
         MVC   PUBLPC,DUB+5            SHOULD BE PACKED WITHOUT SIGN            
         MVI   PUBLPCI,C'I'            SET ON INCH INDICATOR                    
         MVI   ACTSW,1                 INCHES WITH 2 DECIMALS                   
         B     EDITP3K                                                          
*                                                                               
EDITP3F  MVI   0(R5),0                                                          
         STC   R1,5(R2)                                                         
EDITP3G  SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,NPDLPC),(R5)                                    
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'        MUST GET 0 REMAINDER - NO DECIMALS         
         BNE   ERROR                                                            
         ZAP   DUB,DUB(6)                                                       
         CP    DUB,=P'999'           MAX I CAN CARRY                            
         BH    ERROR                                                            
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
         MVC   PUBLPC(2),DUB+6                                                  
         MVI   ACTSW,1                                                          
*                                                                               
EDITP3K  MVC   5(1,R2),WORK        RESTORE ORIGINAL LENGHT AND INPUT            
         MVC   8(6,R2),WORK+1                                                   
*                                                                               
EDITP3M  LA    R2,NPDFDH           FULL DEPTH IN INCHES 2 DECIMALS              
         MVC   PUBFD(3),=PL3'0'                                                 
         CLI   5(R2),0                                                          
         BE    EDITP4                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,NPDFD),(R5)                                     
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         C     R5,=F'3000'         LOGICAL MAX IS 30 INCHES                     
         BH    ERROR                                                            
         CVD   R5,DUB                                                           
         CP    DUB+5(3),=PL3'0'                                                 
         BNH   ERROR               CAN NOT BE NEGATIVE                          
         MVC   PUBFD(3),DUB+5                                                   
         MVI   ACTSW,1                                                          
*                                                                               
EDITP4   LA    R2,NPDLDTH                                                       
         MVI   PUBLDTI,0              SET OFF UNITS INDICATOR                   
         MVC   PUBLDT(3),=PL3'0'                                                
         CLI   5(R2),0                                                          
         BE    EDITP5                                                           
         MVC   WORK(1),5(R2)        SAVE ORIGINAL LENGHT AND INPUT              
         MVC   WORK+1(7),8(R2)                                                  
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    R5,8(R2)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),C'L'                                                       
         BE    EDITP4F                                                          
         CLI   0(R5),C'I'                                                       
         BNE   EDITP4G                                                          
         MVI   0(R5),0                                                          
         STC   R1,5(R2)                                                         
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,NPDLDT),(R5)                                    
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         CP    DUB,=P'99999'       MAX I CAN CARRY                              
         BH    ERROR                                                            
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
         MVC   PUBLDT,DUB+5                                                     
         MVI   PUBLDTI,C'I'        SET ON INCH INDICATOR                        
         B     EDITP4X                                                          
*                                                                               
EDITP4F  MVI   0(R5),0                                                          
         STC   R1,5(R2)                                                         
*                                                                               
EDITP4G  SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,NPDLDT),(R5)                                    
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'        MUST GET 0 REMAINDER - NO DECIMALS         
         BNE   ERROR                                                            
         ZAP   DUB,DUB(6)                                                       
         CP    DUB,=P'99999'       MAX I CAN CARRY                              
         BH    ERROR                                                            
         MVC   PUBLDT(3),DUB+5                                                  
*                                                                               
EDITP4X  MVI   ACTSW,1                                                          
         MVC   5(1,R2),WORK       RESTORE INPUT LENGHT                          
         MVC   8(7,R2),WORK+1     RESTORE INPUT                                 
*                                                                               
EDITP5   LA    R2,NPDTABLH                                                      
         MVI   PUBTBLD,0                                                        
         CLI   5(R2),0                                                          
         BE    EDITP6                                                           
         CLI   8(R2),C'T'                                                       
         BE    MOVETAB                                                          
         B     ERROR                                                            
MOVETAB  MVC   PUBTBLD,8(R2)                                                    
         MVI   ACTSW,1                                                          
EDITP6   LA    R2,NPDTOFPH                                                      
         MVI   PUBPRESS,0                                                       
         CLI   5(R2),0                                                          
         BE    EDITP7                                                           
         CLI   8(R2),C'1'                                                       
         BL    ERROR                                                            
         CLI   8(R2),C'4'                                                       
         BH    ERROR                                                            
MOVEPR   MVC   PUBPRESS,8(R2)                                                   
         MVI   ACTSW,1                                                          
EDITP7   LA    R2,NPDMDRH                                                       
         MVI   PUBMDROP,0                                                       
         CLI   5(R2),0                                                          
         BE    EDITP8                                                           
         CLI   8(R2),C'1'                                                       
         BE    MOVEROP                                                          
         CLI   8(R2),C'2'                                                       
         BE    MOVEROP                                                          
         CLI   8(R2),C'3'                                                       
         BE    MOVEROP                                                          
         B     ERROR                                                            
MOVEROP  MVC   PUBMDROP,8(R2)                                                   
         MVI   ACTSW,1                                                          
EDITP8   LA    R2,NPDEXCLH                                                      
         MVI   PUBEXCL,0                                                        
         CLI   5(R2),0                                                          
         BE    EDITP9                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
*                                                                               
         LA    R1,NPDEXCL                                                       
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
         BNP   EDITP9                                                           
         CLI   1(R1),C','                                                       
         BNE   ERROR                                                            
         LA    R1,2(R1)                                                         
         B     FINDEXCL                                                         
*                B   W   L   T   C                                              
EXCLTAB  DC    X'C280E640D320E310C3080000'                                      
*                                                                               
*                                                                               
EDITP9   LA    R2,NPDBFDSH                                                      
         MVI   PUBBFDS,0                                                        
         CLI   5(R2),0                                                          
         BE    EDITP10                                                          
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         LA    R1,NPDBFDS                                                       
FINDDAY  LA    R4,DAYTAB                                                        
CKDAY    CLC   0(2,R1),0(R4)                                                    
         BE    MOVEDAY                                                          
         CLC   3(2,R4),=X'0000'    SEE IF END OF TABLE IS NEXT                  
         BE    ERROR                                                            
         LA    R4,3(R4)                                                         
         B     CKDAY                                                            
MOVEDAY  OC    PUBBFDS,2(R4)                                                    
         MVI   ACTSW,1                                                          
         BCTR  R5,R0                                                            
         BCTR  R5,R0                                                            
         BCTR  R5,R0                                                            
         LTR   R5,R5                                                            
         BNP   EDITP10                                                          
         CLI   2(R1),C'-'                                                       
         BNE   ERROR                                                            
         LA    R1,3(R1)                                                         
         B     FINDDAY                                                          
*                M O   T U   W E   T H   F R   S A   S U                        
DAYTAB   DC    X'D4D640E3E420E6C510E3C808C6D904E2C102E2E4010000'                
         DS    0H                                                               
EDITP10  EQU   *                                                                
EDITP12  LA    R2,NPDCDMOH                                                      
         MVC   PUBCLMO(2),=PL2'0'                                               
         CLI   5(R2),0                                                          
         BE    EDITP13                                                          
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,NPDCDMO),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'                                                   
         BNE   ERROR               MUST GET 0 REMAINDER                         
         MVC   PUBCLMO(2),DUB+4                                                 
         MVI   ACTSW,1                                                          
EDITP13  LA    R2,NPDCDDAH                                                      
         MVC   PUBCLDA(2),=PL2'0'                                               
         CLI   5(R2),0                                                          
         BE    EDITPM1                                                          
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,NPDCDDA),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'                                                   
         BNE   ERROR      MUST GET REMAINDER ZERO                               
         CP    DUB+4(2),=P'31'                                                  
         BH    ERROR                                                            
         CP    PUBCLMO(2),=P'0'                                                 
         BNL   EDITP14                                                          
         CP    DUB+4(2),=P'0'                                                   
         BL    ERROR                                                            
EDITP14  MVC   PUBCLDA(2),DUB+4                                                 
         MVI   ACTSW,1                                                          
EDITPM1  LA    R2,NPDMCLMH        MAT. CLOSING DATE (MONTH) ***********         
         MVC   PUBMCLMO(2),=PL2'0'                 ADD 12/04/87       *         
         CLI   5(R2),0                                                *         
         BE    EDITPM2                                                *         
         SR    R5,R5                                                  *         
         IC    R5,5(R2)                                               *         
         GOTO1 VCASHVAL,DMCB,(0,NPDMCLM),(R5)                         *         
         CLI   DMCB,X'FF'                                             *         
         BE    ERROR                                                  *         
         L     R5,DMCB+4                                              *         
         CVD   R5,DUB                                                 *         
         DP    DUB,=P'100'                                            *         
         CP    DUB+6(2),=P'0'                                         *         
         BNE   ERROR               MUST GET 0 REMAINDER               *         
         MVC   PUBMCLMO(2),DUB+4                                      *         
         MVI   ACTSW,1                                                *         
EDITPM2  LA    R2,NPDMCLDH                                            *         
         MVC   PUBMCLDA(2),=PL2'0'                                    *         
         CLI   5(R2),0                                                *         
         BE    EDITP15                                                *         
         SR    R5,R5                                                  *         
         IC    R5,5(R2)                                               *         
         GOTO1 VCASHVAL,DMCB,(0,NPDMCLD),(R5)                         *         
         CLI   DMCB,X'FF'                                             *         
         BE    ERROR                                                  *         
         L     R5,DMCB+4                                              *         
         CVD   R5,DUB                                                 *         
         DP    DUB,=P'100'                                            *         
         CP    DUB+6(2),=P'0'                                         *         
         BNE   ERROR      MUST GET REMAINDER ZERO                     *         
         CP    DUB+4(2),=P'31'                                        *         
         BH    ERROR                                                  *         
         CP    PUBMCLMO(2),=P'0'                                      *         
         BNL   EDITPM2A                                               *         
         CP    DUB+4(2),=P'0'                                         *         
         BL    ERROR                                                  *         
EDITPM2A MVC   PUBMCLDA(2),DUB+4                                      *         
         MVI   ACTSW,1                    *****************************         
EDITP15  LA    R2,NPDRATEH                                                      
         MVI   PUBFLAT,0                                                        
         CLI   5(R2),0                                                          
         BE    EDITLTL                                                          
         CLI   8(R2),C'F'                                                       
         BE    EDITP15A                                                         
         CLI   8(R2),C'S'                                                       
         BE    EDITP15A                                                         
         B     ERROR                                                            
EDITP15A MVC   PUBFLAT,8(R2)                                                    
         MVI   ACTSW,1                                                          
         B     EDITLTL                                                          
*                                                                               
         DROP  R6                                                               
EDITLTL  MVI   LTLSW,0                                                          
         ST    R6,SAVER6           SAVE PUBREC'S R6 - PPUBPRD                   
         L     R6,LELADDR          SAVED ADDR OF 21 ELEM IN LTLREC              
         USING LTLPRDD,R6                                                       
         LA    R2,NPDLCCH                                                       
         MVI   PUBLCFCI,0          SET OFF UNITS INDICATOR                      
         MVC   PUBLCFC(2),=PL2'0'                                               
         CLI   5(R2),0                                                          
         BE    EDITL1M                                                          
         MVC   WORK(1),5(R2)       SAVE ORIGINAL LENGHT AND INPUT               
         MVC   WORK+1(6),8(R2)                                                  
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    R5,8(R2)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),C'L'                                                       
         BE    EDITL1F                                                          
         CLI   0(R5),C'I'                                                       
         BNE   EDITL1G                                                          
*                                 MUST CARRY INCHES WITH 2 DECIMALS             
*                                 PACKED WITHOUT SIGN                           
         MVI   0(R5),0                                                          
         STC   R1,5(R2)                                                         
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,NPDLCC),(R5)                                    
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'9999'           MAX IS 99.99 INCHES                       
         BH    ERROR                                                            
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
         MP    DUB,=P'10'                                                       
         MVC   PUBLCFC,DUB+5           SHOULD BE PACKED WITHOUT SIGN            
         MVI   PUBLCFCI,C'I'           SET ON INCH INDICATOR                    
         B     EDITL1K                                                          
*                                                                               
EDITL1F  MVI   0(R5),0                                                          
         STC   R1,5(R2)                                                         
*                                                                               
EDITL1G  SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,NPDLCC),(R5)                                    
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'        MUST GET 0 REMAINDER - NO DECIMALS         
         BNE   ERROR                                                            
         ZAP   DUB,DUB(6)                                                       
         CP    DUB,=P'999'        MAX I CAN CARRY                               
         BH    ERROR                                                            
         MVC   PUBLCFC(2),DUB+6                                                 
EDITL1K  MVI   LTLSW,1                                                          
         MVC   5(1,R2),WORK       RESTORE INPUT LENGHT AND INPUT                
         MVC   8(6,R2),WORK+1                                                   
*                                                                               
EDITL1M  LA    R2,NPDCWH                                                        
**NEW 3/14/89                                                                   
         MVI   PUBCWI,0                                                         
**NEW 3/14/89                                                                   
         MVC   PUBCOLWD(3),=PL3'0'                                              
         CLI   5(R2),0                                                          
         BE    EDITL2                                                           
         ZIC   R5,5(R2)                                                         
**NEW 3/14/89                                                                   
         CLI   8(R2),C'P'            SEE IF PICA (3 DECIMALS)                   
         BNE   EDITL1Q                                                          
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BNP   ERROR                                                            
         GOTO1 VCASHVAL,DMCB,(3,NPDCW+1),(R5)                                   
         MVI   PUBCWI,C'P'                                                      
         B     EDITL1R                                                          
**NEW 3/14/89                                                                   
EDITL1Q  GOTO1 VCASHVAL,DMCB,(4,NPDCW),(R5)  INCHES (4 DECIMALS)                
EDITL1R  CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         C     R5,=F'99999'                                                     
         BH    ERROR                                                            
         CVD   R5,DUB                                                           
         CP    DUB+5(3),=PL3'0'                                                 
         BNH   ERROR               CAN NOT BE NEGATIVE                          
         MVC   PUBCOLWD(3),DUB+5                                                
         MVI   LTLSW,1                                                          
*                                                                               
EDITL2   LA    R2,NPDCLASH                                                      
         XC    PUBCLASS(2),PUBCLASS                                             
         CLI   5(R2),0                                                          
         BE    OUTPUT                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,MOVECL                                                        
         MVI   LTLSW,1                                                          
         B     OUTPUT                                                           
*                                                                               
MOVECL   MVC   PUBCLASS(0),8(R2)                                                
*                                                                               
         DROP  R6                                                               
OUTPUT   L     R6,SAVER6           RESTORE PUBREC'S R6                          
         USING PUBPRDD,R6          SAVED AT EDITLTL                             
         CLI   ACTSW,1                                                          
         BNE   CKDELE              ACTSW WILL BE 0 FOR SRDS RECS                
         TM    PUBIND,X'10'   SEE IF ELEMENT EXISTED                            
         BO    CKLTLR         PUBREC IS READY TO BE WRITTEN                     
         MVC   PUBGENEL(2),=X'2032'                                             
         LA    R3,PUBREC+33                                                     
         CLI   0(R3),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'      NO NAME ELEMENT                                        
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         GOTO1 VRECUP,DMCB,(1,PUBREC),PUBGENEL,0(R3)                            
         B     CKLTLR                                                           
*                                                                               
CKDELE   CLC   PUBKAGY(2),=C'ZZ'      CHECK SRDS                                
         BE    CKLTLR          DON'T ALTER SRDS RECS                            
         TM    PUBIND,X'10'                                                     
         BNO   CKLTLR              NO NEED TO DELETE IT                         
         GOTO1 VRECUP,DMCB,(1,PUBREC),PUBGENEL                                  
         B     CKLTLR                                                           
         DROP  R6                                                               
*                                                                               
CKLTLR   L     R6,LELADDR          SET R6 TO LTLREC ELEM                        
         USING LTLPRDD,R6                                                       
         CLI   LTLSW,1                                                          
         BNE   CKDELEL                                                          
         TM    LTLIND,X'01'        SEE IF LTLREC EXISTED                        
         BO    ADDELEM                                                          
         OI    LTLIND,X'02'        NEED TO FORMAT LTLREC                        
         MVC   LTLKEY(1),BMED                                                   
         MVC   LTLKEY+1(6),BPUB                                                 
         MVC   LTLKAGY(2),AGYALPHA                                              
         MVI   LTLKCOD,X'85'                                                    
         MVC   LTLKEY+25(2),=X'0035'   RECORD LENGTH                            
         MVC   PUBSPREL(2),=X'2114'     ELEMENT CODE AND LENGTH                 
         B     WRTRECS                                                          
*                                                                               
ADDELEM  TM    LTLIND,X'10'        SEE IF PUBSPREL EXISTED                      
         BO    WRTRECS            IF ONE THEN LTLREC IS READY                   
         MVC   PUBSPREL(2),=X'2114'                                             
         GOTO1 VRECUP,DMCB,(1,LTLREC),PUBSPREL,PUBSPREL                         
         B     WRTRECS                                                          
*                                                                               
CKDELEL  TM    LTLIND,X'10'        SEE IF ELEMENT EXISTED                       
         BNO   WRTRECS                                                          
         GOTO1 VRECUP,DMCB,(1,LTLREC),PUBSPREL                                  
         B     WRTRECS                                                          
*                                                                               
         DROP  R6                                                               
WRTRECS  MVC   KEY+27(4),PUBADDR                                                
         CLC   PUBKAGY,AGYALPHA                                                 
         BNE   STDERROR                                                         
         BAS   R5,CLEARPUB                                                      
         BAS   RE,PUTPUB                                                        
         CLI   ACTSW,1             WAS THIS AN ADD OR CHANGE ?                  
         BNE   DONE                NO                                           
         OI    PUBIND,X'10'        PUBGENEL EXISTS                              
         MVI   REDSW,1             TO INDICATE "REDISPLAY"                      
         L     R6,SAVER6           RESTORE PUBREC'S R6                          
         B     PUTFLDS                                                          
WRTLTL   DS    0H                                                               
         TM    LTLIND,X'02'                                                     
         BNO   CHGLTL                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(10),LTLKEY                                                   
         BAS   RE,MOVEOUT                                                       
         BAS   RE,ADDPUB                                                        
         CLI   LTLSW,1             WAS THIS A "LITTLE" ADD OR CHANGE ?          
         BNE   DONE                NO                                           
         OI    LTLIND,X'10'        PUBSPREL EXISTS                              
         MVI   REDSW,1             TO INDICATE "REDISPLAY"                      
         B     PUTLFLDS            PUT "LITTLE" FIELDS                          
*****    B     DONE                                                             
*                                                                               
CHGLTL   TM    LTLIND,X'01'        SEE IF LTLREC EXISTED                        
         BNO   DONE                                                             
         MVC   KEY+27(4),LTLADDR    REREAD LTLREC BEFORE WRITE                  
         BAS   RE,GETPUB                                                        
         BAS   RE,MOVEOUT                                                       
         MVC   KEY+27(4),LTLADDR                                                
         BAS   R5,CLEARPUB                                                      
         BAS   RE,PUTPUB                                                        
         CLI   LTLSW,1             WAS THIS A "LITTLE" ADD OR CHANGE ?          
         BNE   DONE                NO                                           
         OI    LTLIND,X'10'        PUBSPREL EXISTS                              
         MVI   REDSW,1             TO INDICATE "REDISPLAY"                      
         B     PUTLFLDS            PUT "LITTLE" FIELDS                          
*****    B     DONE                                                             
*                                                                               
CLEARPUB SR    R0,R0                                                            
         IC    R0,PUBREC+25                                                     
         SLL   R0,8                                                             
         IC    R0,PUBREC+26                                                     
         SR    RE,RE                                                            
         LA    RE,PUBREC                                                        
         AR    RE,R0                                                            
         SR    RF,RF                                                            
         LA    RF,PUBREC+1999                                                   
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
         BR    R5                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
FORMATP  CLI   SAVSCRN,X'02'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1                                                           
         BNE   FMT5                                                             
         MVI   BYTE2,0             TO GENERATE TURNAROUND                       
         B     EDITPUB                                                          
*                                                                               
FMT2     LA    R4,PBLLAST                                                       
         GOTO1 VCALLOV,WORK,(R4),X'D90406F2'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'02'                                                    
*                                                                               
FMT5     DS    0H                                                               
*                                  R6 TO PUBREC ELEM                            
         USING PUBPRDD,R6                                                       
         EJECT                                                                  
PUTFLDS  DS    0H                  CLEAR SCREEN                                 
         LA    R2,NPDACOMH                                                      
         LA    R4,20               FOR BCT                          L01         
*        LA    R4,19               FOR BCT L01                                  
PUTF2    TM    1(R2),X'20'         TEST PROTECTED                               
         BO    PUTF3                                                            
         ZIC   R5,0(R2)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         FOUT  (R2)                                                             
         BCT   R4,PUTF3                                                         
         B     PUTF4                                                            
*                                                                               
PUTF3    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),8                                                          
         BH    PUTF2                                                            
*                                                                               
PUTF4    DS    0H                  END OF SCREEN                                
*                                                                               
* DISPLAY AGENCY COMMISSION        DEFAULT IS 15% FOR ALL                       
*                                                                               
         MVC   NPDACOM(7),=C'15.000 '                                           
         FOUT  NPDACOMH                                                         
*                                                                               
PUTF4C   TM    PUBIND,X'10'        SEE IF PUBGENEL EXISTS                       
         BO    PUTF5                                                            
         B     PUTLFLDS                                                         
*                                                                               
PUTF5    DS    0H                                                               
         ZAP   PACKED3,PUBAC                                                    
*                                                                               
         CP    PUBAC,=P'-1'         MEANS 100 PCT                               
         BNE   PUTF6                                                            
         MVC   NPDACOM(7),=C'100.000'                                           
         B     PUTF6C                                                           
*                                                                               
PUTF6    EDIT  PACKED3,(7,NPDACOM),3,ALIGN=LEFT                                 
PUTF6C   FOUT  NPDACOMH                                                         
         MVC   PACKED2(2),PUBCD                                                 
         EDIT  PACKED2,(4,NPDCDIS),1,ALIGN=LEFT                                 
         FOUT  NPDCDISH                                                         
         XC    NPDCDED,NPDCDED                                                  
         OC    PUBCDDAT,PUBCDDAT                                                
         BZ    PUTF8                                                            
*        GOTO1 VDTCNV,DMCB,(1,PUBCDDAT),(3,NPDCDED)                             
         GOTO1 VDATCON,DMCB,(3,PUBCDDAT),(5,NPDCDED)                            
PUTF8    FOUT  NPDCDEDH                                                         
         MVC   PACKED2(2),PUBCPP                                                
         EDIT  PACKED2,(3,NPDCPP),ALIGN=LEFT                                    
         FOUT  NPDCPPH                                                          
         CLI   PUBLPCI,C'I'        SEE IF INCHES                                
         BNE   PUTF8D                                                           
         MVC   PACKED3(2),PUBLPC   CARRIED PACKED WITHOUT SIGN                  
         MVI   PACKED3+2,X'0C'     2 DECIMALS                                   
         ZAP   DUB,PACKED3                                                      
         DP    DUB,=P'10'                                                       
         ZAP   PACKED3,DUB(6)                                                   
         EDIT  PACKED3,(6,NPDLPC),2,ALIGN=LEFT,TRAIL=C'I'                       
         B     PUTF8K                                                           
PUTF8D   MVC   PACKED2(2),PUBLPC                                                
         EDIT  PACKED2,(6,NPDLPC),ALIGN=LEFT,TRAIL=C'L'                         
PUTF8K   FOUT  NPDLPCH                                                          
         OC    PUBFD,PUBFD       COULD BE BINARY ZEROS                          
         BZ    PUTF9                                                            
         CP    PUBFD,=P'0'                                                      
         BE    PUTF9                                                            
         MVC   PACKED3(3),PUBFD                                                 
         EDIT  PACKED3,(5,NPDFD),2,ALIGN=LEFT                                   
         FOUT  NPDFDH                                                           
PUTF9    MVC   PACKED3(3),PUBLDT                                                
         CLI   PUBLDTI,C'I'        SEE IF DOING INCHES                          
         BNE   PUTF9C                                                           
         EDIT  PACKED3,(7,NPDLDT),2,ALIGN=LEFT,TRAIL=C'I'                       
         B     PUTF9D                                                           
PUTF9C   EDIT  PACKED3,(7,NPDLDT),ALIGN=LEFT,TRAIL=C'L'                         
PUTF9D   FOUT  NPDLDTH                                                          
         FOUT  NPDTABLH,PUBTBLD                                                 
         FOUT  NPDTOFPH,PUBPRESS                                                
         FOUT  NPDMDRH,PUBMDROP                                                 
         LA    R1,NPDEXCL                                                       
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
PUTFLDS1 FOUT  NPDEXCLH                                                         
         LA    R1,NPDBFDS                                                       
         LA    R4,BFDTAB1                                                       
CKBITSF  MVC   WORK(1),PUBBFDS                                                  
         NC    PUBBFDS(1),0(R4)                                                 
         CLC   PUBBFDS,WORK                                                     
         BE    NEXTBTF                                                          
         MVC   0(2,R1),1(R4)                                                    
         OC    PUBBFDS,PUBBFDS                                                  
         BZ    PUTFLDS2                                                         
         MVI   2(R1),C'-'                                                       
         LA    R1,3(R1)                                                         
*                                                                               
NEXTBTF  CLC   3(2,R4),=X'0000'                                                 
         BE    PUTFLDS2                                                         
         LA    R4,3(R4)                                                         
         OC    PUBBFDS,PUBBFDS                                                  
         BZ    PUTFLDS2                                                         
         B     CKBITSF                                                          
*                  M O   T U   W E   T H   F R   S A   S U                      
BFDTAB1  DC    X'BFD4D6DFE3E4EFE6C5F7E3C8FBC6D9FDE2C1FEE2E40000'                
*                                                                               
PUTFLDS2 FOUT  NPDBFDSH                                                         
         MVC   PACKED2,PUBCDDAS    CASH DISC DAYS                               
         EDIT  PACKED2,(3,NPDCSHD),ALIGN=LEFT                                   
         FOUT  NPDCSHDH                                                         
         MVC   PACKED2(2),PUBCLMO                                               
         EDIT  PACKED2,(3,NPDCDMO),FLOAT=-,ALIGN=LEFT                           
         FOUT  NPDCDMOH                                                         
         MVC   PACKED2(2),PUBCLDA                                               
         EDIT  PACKED2,(3,NPDCDDA),FLOAT=-,ALIGN=LEFT                           
         FOUT  NPDCDDAH                                                         
         MVC   PACKED2(2),PUBMCLMO                     ***************          
         EDIT  PACKED2,(3,NPDMCLM),FLOAT=-,ALIGN=LEFT  *             *          
         FOUT  NPDMCLMH                                * ADD 12/04/87*          
         MVC   PACKED2(2),PUBMCLDA                     *             *          
         EDIT  PACKED2,(3,NPDMCLD),FLOAT=-,ALIGN=LEFT  *             *          
         FOUT  NPDMCLDH                                ***************          
         FOUT  NPDRATEH,PUBFLAT                                                 
         DROP  R6                                                               
         CLI   REDSW,1             REDISPLAY OF ADD OR CHANGE ?                 
         BE    WRTLTL              YES - RETURN TO TEST LITTLE                  
*                                                                               
PUTLFLDS EQU *                                                                  
         L     R6,LELADDR          SAVED ADDR OF 21 ELEM IN LTLREC              
         USING LTLPRDD,R6                                                       
         TM    LTLIND,X'10'                                                     
         BO    PUTLF5                                                           
         B     CKSRDS                                                           
*                                                                               
PUTLF5   DS    0H                                                               
         CLI   PUBLCFCI,C'I'         SEE IF DOING INCHES                        
         BNE   PUTLF5E                                                          
         MVC   PACKED3(2),PUBLCFC  CARRIED PACKED WITHOUT SIGN                  
         MVI   PACKED3+2,X'0C'     2 DECIMALS                                   
         ZAP   DUB,PACKED3                                                      
         DP    DUB,=P'10'                                                       
         ZAP   PACKED3,DUB(6)                                                   
         EDIT  PACKED3,(6,NPDLCC),2,ALIGN=LEFT,TRAIL=C'I'                       
         B     PUTLF5G                                                          
*                                                                               
PUTLF5E  MVC   PACKED2(2),PUBLCFC                                               
         EDIT  PACKED2,(6,NPDLCC),ALIGN=LEFT,TRAIL=C'L'                         
PUTLF5G  FOUT  NPDLCCH                                                          
PUTL1    CP    PUBCOLWD,=P'0'                                                   
         BE    PUTL2                                                            
         MVC   PACKED3(3),PUBCOLWD                                              
         CLI   PUBCWI,C'P'              SEE IF PICA (3 DECMALS)                 
         BNE   PUTL1C                                                           
         MVI   NPDCW,C'P'                                                       
         EDIT  PACKED3,(6,NPDCW+1),3,ALIGN=LEFT                                 
         B     PUTL1D                                                           
PUTL1C   EDIT  PACKED3,(6,NPDCW),4,ALIGN=LEFT                                   
PUTL1D   FOUT  NPDCWH                                                           
PUTL2    EQU   *                                                                
         FOUT  NPDCLASH,PUBCLASS                                                
*                                                                               
         DROP  R6                                                               
*                                                                               
         CLI   BACT,3              SRDS                                         
         BNE   CKSRDS                                                           
PROTECT  OI    NPDLCCH+1,X'20'                                                  
         OI    NPDFDH+1,X'20'                                                   
         OI    NPDCWH+1,X'20'                                                   
         OI    NPDCLASH+1,X'20'                                                 
PROTECTS OI    NPDACOMH+1,X'20'                                                 
         OI    NPDCDISH+1,X'20'                                                 
         OI    NPDCDEDH+1,X'20'                                                 
         OI    NPDCPPH+1,X'20'                                                  
         OI    NPDLPCH+1,X'20'                                                  
         OI    NPDLDTH+1,X'20'                                                  
         OI    NPDTABLH+1,X'20'                                                 
         OI    NPDTOFPH+1,X'20'                                                 
         OI    NPDMDRH+1,X'20'                                                  
         OI    NPDEXCLH+1,X'20'                                                 
         OI    NPDBFDSH+1,X'20'                                                 
         OI    NPDCSHDH+1,X'20'                                                 
         OI    NPDCDMOH+1,X'20'                                                 
         OI    NPDCDDAH+1,X'20'                                                 
         OI    NPDMCLMH+1,X'20'     ADD 12/04/87                                
         OI    NPDMCLDH+1,X'20'     ADD 12/04/87                                
         OI    NPDRATEH+1,X'20'                                                 
         MVI   SAVSCRN,0                                                        
         CLI   BACT,2                                                           
         BH    DONE                                                             
         LA    R2,NPDACOMH                                                      
         B     EXIT                                                             
*                                                                               
CKSRDS   CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   PROTECT                                                          
         CLI   REDSW,1             REDISPLAYING ELEMENTS ?                      
         BE    DONE                YES                                          
         LA    R2,NPDACOMH                                                      
         B     EXIT                                                             
*                                                                               
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
STDERROR DS    0H                                                               
         LA    R3,STDERR                                                        
         LA    R2,PBLACTH                                                       
         B     ERROR                                                            
STDERR   EQU   113                                                              
*                                                                               
         EJECT                                                                  
MOVEIN   LA    R4,IOAREA                                                        
         LH    R5,=H'4000'                                                      
         LA    R6,IOAREA                                                        
         LA    R6,4000(R6)         POINT TO IOAREA2                             
         LA    R6,500(R6)                                                       
MOVEIN1  CH    R5,=H'250'                                                       
         BNH   MOVEREST                                                         
         MVC   0(250,R6),0(R4)                                                  
         LA    R4,250(R4)                                                       
         LA    R6,250(R6)                                                       
         SH    R5,=H'250'                                                       
         LTR   R5,R5                                                            
         BNZ   MOVEIN1                                                          
         BR    RE                                                               
*                                                                               
MOVEREST BCTR  R5,R0                                                            
         EX    R5,MOVEVARI                                                      
         BR    RE                                                               
*                                                                               
MOVEVARI MVC   0(0,R6),0(R4)                                                    
*                                                                               
*                                                                               
         DC    F'0'                                                             
MOVEOUT  ST    RE,MOVEOUT-4                                                     
         LA    R4,IOAREA                                                        
         LH    R5,=H'4000'                                                      
         BAS   RE,CLEARWRK                                                      
         LA    R4,IOAREA                                                        
         LA    R6,IOAREA                                                        
         LA    R6,4000(R6)         POINT TO IOAREA2                             
         LA    R6,500(R6)                                                       
         LH    R5,=H'4000'                                                      
MOVEOUT1 CH    R5,=H'250'                                                       
         BNH   MOVEIT                                                           
         MVC   0(250,R4),0(R6)                                                  
         LA    R4,250(R4)                                                       
         LA    R6,250(R6)                                                       
         SH    R5,=H'250'                                                       
         LTR   R5,R5                                                            
         BNZ   MOVEOUT1                                                         
*                                                                               
MOVEOUTX L     RE,MOVEOUT-4                                                     
         BR    RE                                                               
*                                                                               
MOVEIT   BCTR  R5,R0                                                            
         EX    R5,MOVEVARO                                                      
         B     MOVEOUTX                                                         
*                                                                               
MOVEVARO MVC   0(0,R4),0(R6)                                                    
         EJECT                                                                  
*                                                                               
FIVE     DC    F'5'                                                             
LELADDR  DS    A                   ADDR OF PUBSPREL                             
SAVER6   DS    F                                                                
PUBIND   DS    CL1                                                              
LTLIND   DS    CL1                                                              
ACTSW    DS    CL1                                                              
LTLSW    DS    CL1                                                              
REDSW    DS    CL1                 '1' = REDISPLAY ELEMENT(S)                   
PACKED2  DS    PL2                                                              
PACKED3  DS    PL3                                                              
PACKED4  DS    PL4                                                              
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
       ++INCLUDE PUGENOLD                                                       
*                                                                               
         DS    4000C                                                            
ELEAREA  DS    500C                                                             
IOAREA2  DS    4000C                                                            
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
LTLRECD  DSECT                                                                  
       ++INCLUDE LTLREC                                                         
         EJECT                                                                  
LTLPRDD  DSECT                                                                  
       ++INCLUDE PUBSPREL                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPUBFFD                                                       
         ORG PBLLAST                                                            
       ++INCLUDE PPPUBF2D                                                       
         ORG T406FFD                                                            
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
**PAN#1  DC    CL21'091PPPUB02   12/19/11'                                      
         END                                                                    
