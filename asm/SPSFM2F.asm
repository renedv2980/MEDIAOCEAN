*          DATA SET SPSFM2F    AT LEVEL 021 AS OF 09/22/03                      
*PHASE T2172FA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T2172F - INFO COMMERCIAL MAINT PROGRAM                      *         
*                                                                     *         
*  CALLED FROM: SPOT CONTROLLER (T21700), WHICH CALLS                 *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS: SCREENS SCSFM9E  (T2179E) -- MAINTENANCE                   *         
*          SCREENS SCSFM9D  (T2179D) -- LIST                          *         
*                                                                     *         
*  OUTPUTS: UPDATED INFOMERCIAL DATA RECORD                           *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - INFO RECORD                                           *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T2172F INFOMERCIAL MAINTENANCE'                                 
T2172F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2172F*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD           GENERAL PRINT AREAS                         
         USING SPOOLD,R8                                                        
*                                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+8                                                              
         BAS   RE,VK                                                            
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BAS   RE,VR                                                            
         BAS   RE,DR                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+8                                                              
         BAS   RE,DR                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+8                                                              
         BAS   RE,DK                                                            
         XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       NTR1                                                                   
         TWAXC SINESTNH,SINESTNH,PROT=Y                                         
         TWAXC SINSTADH,SINSTADH,PROT=Y                                         
         TWAXC SINPRDDH,SINPRDDH,PROT=Y                                         
         TWAXC SINSTANH,SINSTANH,PROT=Y                                         
         LA    R4,SAVEKEY                                                       
         USING INFKEY,R4                                                        
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   INFKTYP,=X'0D79'                                                 
         LA    R2,SINMEDIH         AGENCY/MEDIA                                 
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         MVC   INFKAGMD,BAGYMD      AGENCY/MEDIA                                
         LA    R2,SINCLTH           CLIENT                                      
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         MVC   INFKCLT,BCLT                                                     
* SEE IF CLIENT IS AN INFOMERCIAL CLIENT                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO                                                           
         USING CLTHDR,R6                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         TM    COPT1,COP1INFQ        CHECK INFOMERCIAL BIT                      
         BO    VK05                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'** Client not an Infomercial client **'           
         GOTO1 ERREX2                                                           
         DROP  R6                                                               
*                                                                               
VK05     LA    R2,SINPRODH                                                      
         GOTO1 ANY                                                              
         CLC   SINPROD,=C'POL'     DON'T ACCEPT PRODUCT POL                     
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
         GOTO1 VALIPRD                                                          
         MVC   INFKPRD,BPRD        PRODUCT                                      
         MVC   SINPRDD,PRDNM      PRODUCT DESCRIPTION                           
         FOUT  SINPRDDH                                                         
         LA    R2,SINESTH                                                       
         GOTO1 ANY                                                              
         GOTO1 VALIEST                                                          
         MVC   INFKEST,BEST         ESTIMATE                                    
         MVC   SINESTN,ESTNM        ESTIMATE NAME                               
         FOUT  SINESTNH                                                         
         LA    R2,SINSTATH          STATION                                     
         GOTO1 ANY                                                              
         CLC   SINSTAT(3),=C'ALL'                                               
         BE    VK10                                                             
         GOTO1 VALISTA                                                          
         MVC   INFKMKT(5),BMKTSTA    PACKED MARKET AND STATION FIELD            
         MVC   SINSTAN,QMKT          MARKET NUMBER                              
         OI    SINSTANH+6,X'80'                                                 
         OI    SINSTANH+4,X'08'                                                 
         OI    SINSTANH+5,4                                                     
         LA    R2,SINSTANH                                                      
         GOTO1 VALIMKT                                                          
         MVC   SINSTAD,MKTNM         MARKET NAME                                
         FOUT  SINSTADH                                                         
         B     *+10                                                             
VK10     MVC   INFKMKT(5),ZEROS      MARKET/STATION FIELDS                      
*                                                                               
VKEND    MVC   KEY,SAVEKEY                                                      
         XIT1                                                                   
         EJECT                                                                  
* DISPLAY KEY FOR LIST FUNCTION                                                 
DK       NTR1                                                                   
         TWAXC SINSTADH,SINSTADH,PROT=Y                                         
         TWAXC SINSTANH,SINSTANH,PROT=Y                                         
         MVC   SINSTAT,SPACES                                                   
         OI    SINSTATH+6,X'80'                                                 
         L     R4,AIO                                                           
         USING INFKEY,R4                                                        
         MVC   MYSAVE,INFKEY       KEEP THE KEY FIELDS AROUND                   
         LA    R4,MYSAVE                                                        
         CLC   INFKTYP,=X'0D79'                                                 
         BNE   DKEND                                                            
* GET MEDIA FROM AGENCY RECORD BY MATCHING THE CODE                             
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),AGENCY                                                  
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         USING AGYMEDEL,R3                                                      
         CLC   AGYMEDBT,INFKAGMD         COMPARE AGENCY/MEDIA CODES             
         BE    DK02                                                             
*                                                                               
DK01     BAS   RE,NEXTEL                                                        
         BNE   DKEND                                                            
         CLC   AGYMEDBT,INFKAGMD         COMPARE AGENCY/MEDIA CODES             
         BNE   DK01                                                             
*                                                                               
DK02     MVC   SINMEDI,AGYMEDCD          MEDIA                                  
         FOUT  SINMEDIH                                                         
         MVC   BCLT,INFKCLT              PACKED FORMAT                          
         GOTO1 CLUNPK,DMCB,BCLT,QCLT     UNPACK CLIENT FOR DISPLAY              
         MVC   SINCLT,QCLT                                                      
         FOUT  SINCLTH                                                          
* GET PRODUCT FROM CLIENT REC                                                   
* READ CLIENT HEADER *                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO                                                           
         USING CLTHDR,R6                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         LA    R5,CLIST                                                         
*                                                                               
DK03     CLC   INFKPRD,3(R5)    PRODUCT NUMBER                                  
         BE    DK05                                                             
         CLI   3(R5),X'00'                                                      
         BE    DK15                                                             
         LA    R5,4(R5)                                                         
         B     DK03                                                             
*                                                                               
DK05     CLI   2(R5),X'40'    IS LAST CHARACTER A BLANK?                        
         BNE   *+8                                                              
         MVI   2(R5),0      CHANGE THE BLANK TO A ZERO FOR THE DISPLAY          
         MVC   SINPROD,0(R5)  PRODUCT MNEMONIC                                  
         FOUT  SINPRODH                                                         
         LA    R2,SINPRODH                                                      
         CLI   2(R5),0         IS PRODUCT LENGTH 2 CHARACTERS                   
         BNE   *+12                                                             
         MVI   5(R2),2         LENGTH FIELD                                     
         B     DK07                                                             
         MVI   5(R2),3         PRODUCT LENGTH IS 3 CHARACTERS                   
*                                                                               
DK07     GOTO1 VALIPRD                                                          
         MVC   SINPRDD,PRDNM      PRODUCT DESCRIPTION                           
         FOUT  SINPRDDH                                                         
         LA    R2,SINESTH                                                       
         EDIT  (1,INFKEST),(3,SINEST),ALIGN=LEFT  ESTIMATE NO.                  
         FOUT  SINESTH                                                          
         XC    KEY,KEY                                                          
         LA    R6,KEY              READ ESTIMATE HEADER                         
         USING ESTHDR,R6                                                        
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,INFKEST                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   SINESTN,EDESC    ESTIMATE NAME                                   
         FOUT  SINESTNH                                                         
         OC    INFKMKT(5),INFKMKT                                               
         BNZ   DK10                                                             
         MVC   SINSTAT,=C'ALL'                                                  
         FOUT  SINSTATH                                                         
         B     DK15                                                             
*                                                                               
DK10     XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING STAPACKD,R2                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPMKST,INFKMKT                                                 
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,WORK                                                    
         MVC   SINSTAT,STAPQSTA                                                 
*                                                                               
         LA    R2,SINSTATH                                                      
         CLI   8(R2),C'0'           CHECK IF A CABLE STATION                    
         BL    *+8                                                              
         MVI   SINSTAT+4,C'/'                                                   
         FOUT  SINSTATH                                                         
         GOTO1 VALISTA                                                          
         MVC   INFKMKT(5),BMKTSTA    PACKED MARKET AND STATION FIELD            
         MVC   SINSTAN,QMKT          MARKET NUMBER                              
         OI    SINSTANH+6,X'80'                                                 
         OI    SINSTANH+4,X'08'                                                 
         OI    SINSTANH+5,4                                                     
         LA    R2,SINSTANH                                                      
         GOTO1 VALIMKT                                                          
         MVC   SINSTAD,MKTNM         MARKET NAME                                
         FOUT  SINSTADH                                                         
*                                                                               
DK15     MVC   KEY,MYSAVE          RESTORE KEY                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         DROP  R3,R4,R6                                                         
*                                                                               
DKEND    XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* CODE TO VALIDATE RECORD                                                       
*                                                                               
VR       NTR1                                                                   
         L     R3,AIO             ADDRESS OF INFO RECORD                        
*                                                                               
* DELETE 05 ELEMENTS                                                            
         MVI   ELCODE,X'05'       CPO ELEMENT                                   
         SPACE 1                                                                
         GOTO1 REMELEM            REMOVES ELEMENT                               
         XC    ELEM,ELEM                                                        
*                                                                               
* TARGET CPO ORDERS                                                             
VR05     LA    R3,ELEM                                                          
         USING INFCPOEL,R3                                                      
         MVI   INFCPOCD,X'05'                                                   
         MVI   INFCPOLN,26                                                      
         LA    R2,SINTCOH         CPO ORDERS                                    
         CLI   5(R2),0                                                          
         BE    VR10                                                             
         BAS   RE,VRNUM                                                         
         MVC   2(4,R3),DMCB+4       TARGET CPO ORDER                            
*                                                                               
* TARGET CPO UPS1                                                               
VR10     LA    R2,SINTCU1H         CPO UPS1                                     
         CLI   5(R2),0                                                          
         BE    VR15                                                             
         BAS   RE,VRNUM                                                         
         MVC   6(4,R3),DMCB+4       TARGET CPO UPS1                             
*                                                                               
* TARGET CPO UPS2                                                               
VR15     LA    R2,SINTCU2H         CPO UPS2                                     
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         BAS   RE,VRNUM                                                         
         MVC   10(4,R3),DMCB+4       TARGET CPO UPS2                            
*                                                                               
*                                                                               
* TARGET CPO UPS3                                                               
VR20     LA    R2,SINTCU3H         CPO UPS3                                     
         CLI   5(R2),0                                                          
         BE    VR25                                                             
         BAS   RE,VRNUM                                                         
         MVC   14(4,R3),DMCB+4       TARGET CPO UPS3                            
* TARGET CPO UPS4                                                               
VR25     LA    R2,SINTCU4H         CPO UPS4                                     
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         BAS   RE,VRNUM                                                         
         MVC   18(4,R3),DMCB+4       TARGET CPO UPS4                            
* TARGET CPO UPS5                                                               
VR30     LA    R2,SINTCU5H         CPO UPS5                                     
         CLI   5(R2),0                                                          
         BE    VR35                                                             
         BAS   RE,VRNUM                                                         
         MVC   22(4,R3),DMCB+4       TARGET CPO UPS5                            
*                                                                               
VR35     OC    INFCPO,INFCPO          ANY DATA                                  
         BZ    VR37                                                             
         GOTO1 ADDELEM            ADD THE 05 ELEMENT                            
*                                                                               
* DELETE 06 ELEMENTS                                                            
VR37     MVI   ELCODE,X'06'       TARGET RATIO ELEMENT                          
         SPACE 1                                                                
         GOTO1 REMELEM            REMOVES ELEMENT                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING INFRATEL,R3                                                      
         MVI   INFRATCD,X'06'                                                   
         MVI   INFRATLN,26                                                      
*                                                                               
* TARGET RATIO ORDERS                                                           
         LA    R2,SINTROH         RATIO ORDER                                   
         CLI   5(R2),0                                                          
         BE    VR40                                                             
         BAS   RE,VRNUM                                                         
         MVC   2(4,R3),DMCB+4       TARGET RATIO ORDER                          
*                                                                               
* TARGET RATIO UPS1                                                             
VR40     LA    R2,SINTRU1H        RATIO UPS1                                    
         CLI   5(R2),0                                                          
         BE    VR45                                                             
         BAS   RE,VRNUM                                                         
         MVC   6(4,R3),DMCB+4       TARGET UPS1                                 
* TARGET RATIO UPS2                                                             
VR45     LA    R2,SINTRU2H        RATIO UPS2                                    
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         BAS   RE,VRNUM                                                         
         MVC   10(4,R3),DMCB+4       TARGET UPS2                                
* TARGET RATIO UPS3                                                             
VR50     LA    R2,SINTRU3H        RATIO UPS3                                    
         CLI   5(R2),0                                                          
         BE    VR55                                                             
         BAS   RE,VRNUM                                                         
         MVC   14(4,R3),DMCB+4       TARGET UPS3                                
* TARGET RATIO UPS4                                                             
VR55     LA    R2,SINTRU4H        RATIO UPS4                                    
         CLI   5(R2),0                                                          
         BE    VR60                                                             
         BAS   RE,VRNUM                                                         
         MVC   18(4,R3),DMCB+4       TARGET UPS4                                
* TARGET RATIO UPS5                                                             
VR60     LA    R2,SINTRU5H        RATIO UPS5                                    
         CLI   5(R2),0                                                          
         BE    VR65                                                             
         BAS   RE,VRNUM                                                         
         MVC   22(4,R3),DMCB+4       TARGET UPS5                                
*                                                                               
VR65     OC    INFRAT,INFRAT             ANY DATA?                              
         BZ    VR67                                                             
         GOTO1 ADDELEM               ADD THE 06 ELEMENT                         
**                                                                              
* VALIDATE BREAKEVEN COST FIELDS                                                
* DELETE 07 ELEMENTS                                                            
VR67     MVI   ELCODE,X'07'       TARGET BREAK EVEN ELEMENT                     
         SPACE 1                                                                
         GOTO1 REMELEM            REMOVES ELEMENT                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING INFCOSEL,R3                                                      
         MVI   INFCOSCD,X'07'                                                   
         MVI   INFCOSLN,26                                                      
*                                                                               
* TARGET BREAK EVEN ORDERS                                                      
         LA    R2,SINBEOH         BE ORDER                                      
         CLI   5(R2),0                                                          
         BE    VR70                                                             
         BAS   RE,VRNUM3                                                        
         MVC   2(4,R3),DMCB+4       BE ORDER                                    
*                                                                               
* BE UPS1                                                                       
VR70     LA    R2,SINBEU1H        BE UPS1                                       
         CLI   5(R2),0                                                          
         BE    VR75                                                             
         BAS   RE,VRNUM3                                                        
         MVC   6(4,R3),DMCB+4       BE UPS1                                     
* BE UPS2                                                                       
VR75     LA    R2,SINBEU2H        BE UPS2                                       
         CLI   5(R2),0                                                          
         BE    VR80                                                             
         BAS   RE,VRNUM3                                                        
         MVC   10(4,R3),DMCB+4       BE UPS2                                    
* BE UPS3                                                                       
VR80     LA    R2,SINBEU3H        RATIO UPS3                                    
         CLI   5(R2),0                                                          
         BE    VR85                                                             
         BAS   RE,VRNUM3                                                        
         MVC   14(4,R3),DMCB+4       BE UPS3                                    
* BE UPS4                                                                       
VR85     LA    R2,SINBEU4H        BE UPS4                                       
         CLI   5(R2),0                                                          
         BE    VR90                                                             
         BAS   RE,VRNUM3                                                        
         MVC   18(4,R3),DMCB+4       BE UPS4                                    
* BE UPS5                                                                       
VR90     LA    R2,SINBEU5H        BE UPS5                                       
         CLI   5(R2),0                                                          
         BE    VR95                                                             
         BAS   RE,VRNUM3                                                        
         MVC   22(4,R3),DMCB+4       BE UPS5                                    
VR95     OC    INFCOS,INFCOS         ANY DATA?                                  
         BZ    VR97                                                             
         GOTO1 ADDELEM                                                          
* VALIDATE MISCELLANEOUS DATA FIELDS - TARGET ORDERS                            
* DELETE 08 ELEMENT                                                             
VR97     MVI   ELCODE,X'08'       INFO MISC. ELEMENT                            
         GOTO1 REMELEM            REMOVES ELEMENT                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING INFMSCEL,R3       INFO MISC ELEMENT                              
         MVI   INFMSCCD,X'08'                                                   
         MVI   INFMSCLN,10                                                      
* TARGET ORDERS                                                                 
         LA    R2,SINTEOH         TARGET ORDER                                  
         CLI   5(R2),0                                                          
         BE    VR100                                                            
         BAS   RE,VRNUM2                                                        
         MVC   INFMSCOR,DMCB+4                                                  
* BUDGET IN DOLLARS                                                             
VR100    LA    R2,SINBUDGH        BUDGET                                        
         CLI   5(R2),0                                                          
         BE    VR110                                                            
         BAS   RE,VRNUM2                                                        
         MVC   INFMSCBU,DMCB+4                                                  
VR110    OC    ELEM+2(8),ELEM+2   ANY TARGET ORDERS OR BUDGET                   
         BZ    VR113                                                            
         GOTO1 ADDELEM                                                          
* VALIDATE 01 ELEMENT - UPSELL CATEGORY NAMES                                   
* SHOULD ONLY BE INPUT IF STATION IS ALL                                        
*                                                                               
VR113    CLC   SINSTAT(3),=C'ALL'                                               
         BE    VR115                                                            
         LA    R2,SINUPS1H                                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
         LA    R2,SINUPS2H                                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
         LA    R2,SINUPS3H                                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
         LA    R2,SINUPS4H                                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
         LA    R2,SINUPS5H                                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
* VALIDATE 09 ELEMENT - UPSELL PRICES                                           
* SHOULD ONLY BE INPUT IF STATION IS ALL                                        
*                                                                               
         CLC   SINSTAT(3),=C'ALL'                                               
         BE    VR115                                                            
         LA    R2,SINPRC1H                                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
         LA    R2,SINPRC2H                                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
         LA    R2,SINPRC3H                                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
         LA    R2,SINPRC4H                                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
         LA    R2,SINPRC5H                                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
         LA    R2,SINPRCRH    RETAIL PRICE                                      
         CLI   5(R2),0                                                          
         BNE   VRERR                                                            
         B     VR115                                                            
*                                                                               
VRERR    MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
* STATION WAS ALL- THERE CAN BE INPUT                                           
* DELETE AND REBUILD 01 ELEMENT                                                 
*                                                                               
VR115    MVI   ELCODE,X'01'       UPSELL CATEGORY NAMES ELEMENT                 
         GOTO1 REMELEM            REMOVES ELEMENT                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING INFNAMEL,R3                                                      
         MVI   INFNAMCD,X'01'                                                   
         MVI   INFNAMLN,62                                                      
         LA    R2,SINUPS1H                                                      
         CLI   5(R2),0                                                          
         BE    VR120                                                            
         MVC   2(12,R3),SINUPS1      UPSELL NAME 1                              
*                                                                               
VR120    LA    R2,SINUPS2H                                                      
         CLI   5(R2),0                                                          
         BE    VR125                                                            
         MVC   14(12,R3),SINUPS2      UPSELL NAME 2                             
*                                                                               
VR125    LA    R2,SINUPS3H                                                      
         CLI   5(R2),0                                                          
         BE    VR130                                                            
         MVC   26(12,R3),SINUPS3      UPSELL NAME 3                             
*                                                                               
VR130    LA    R2,SINUPS4H                                                      
         CLI   5(R2),0                                                          
         BE    VR135                                                            
         MVC   38(12,R3),SINUPS4      UPSELL NAME 4                             
*                                                                               
VR135    LA    R2,SINUPS5H                                                      
         CLI   5(R2),0                                                          
         BE    VR140                                                            
         MVC   50(12,R3),SINUPS5      UPSELL NAME 5                             
*                                                                               
VR140    OC    INFNAMES,INFNAMES                                                
         BZ    VR145                                                            
         GOTO1 ADDELEM                                                          
*                                                                               
* STATION WAS ALL- THERE CAN BE INPUT                                           
* DELETE AND REBUILD 09 ELEMENT                                                 
*                                                                               
VR145    MVI   ELCODE,X'09'       UPSELL PRICE ELEMENT                          
         GOTO1 REMELEM            REMOVES ELEMENT                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING INFPRICE,R3          UPSELL PRICE ELEMENT                        
         MVI   INFPRCD,X'09'                                                    
         MVI   INFPRLN,26                                                       
         LA    R2,SINPRC1H                                                      
         CLI   5(R2),0                                                          
         BE    VR150                                                            
         BAS   RE,VRNUM                                                         
         MVC   2(4,R3),DMCB+4        UPSELL PRICE 1                             
*                                                                               
VR150    LA    R2,SINPRC2H                                                      
         CLI   5(R2),0                                                          
         BE    VR155                                                            
         BAS   RE,VRNUM                                                         
         MVC   6(4,R3),DMCB+4        UPSELL PRICE 2                             
*                                                                               
VR155    LA    R2,SINPRC3H                                                      
         CLI   5(R2),0                                                          
         BE    VR160                                                            
         BAS   RE,VRNUM                                                         
         MVC   10(4,R3),DMCB+4       UPSELL PRICE 3                             
*                                                                               
VR160    LA    R2,SINPRC4H                                                      
         CLI   5(R2),0                                                          
         BE    VR165                                                            
         BAS   RE,VRNUM                                                         
         MVC   14(4,R3),DMCB+4       UPSELL PRICE 4                             
*                                                                               
VR165    LA    R2,SINPRC5H                                                      
         CLI   5(R2),0                                                          
         BE    VR170                                                            
         BAS   RE,VRNUM                                                         
         MVC   18(4,R3),DMCB+4       UPSELL PRICE 5                             
*                                                                               
VR170    LA    R2,SINPRCRH           RETAIL PRICE                               
         CLI   5(R2),0                                                          
         BE    VR175                                                            
         BAS   RE,VRNUM3                                                        
         MVC   22(4,R3),DMCB+4       RETAIL PRICE                               
*                                                                               
VR175    OC    INFPR(24),INFPR           ANY PRICES?                            
         BZ    VREND                                                            
         GOTO1 ADDELEM                                                          
*                                                                               
VREND    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
* CLEAR OUT SCREEN INPUT                                                        
* GET ELEMENT DATA                                                              
DR       NTR1                                                                   
         CLC   KEY(2),=X'0D79'                                                  
         BNE   DREND                                                            
         TWAXC SINTCOH           CLEAR RECORD DATA FROM SCREEN                  
         L     R3,AIO         RECORD AREA                                       
         USING INFNAMEL,R3     UPSELL CATEGORY NAMES ELEMENT                    
         MVI   ELCODE,X'01'   UPSELL NAMES ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         MVC   SINUPS1,2(R3)   NAME1                                            
         FOUT  SINUPS1H                                                         
         MVC   SINUPS2,14(R3)   NAME2                                           
         FOUT  SINUPS2H                                                         
         MVC   SINUPS3,26(R3)   NAME3                                           
         FOUT  SINUPS3H                                                         
         MVC   SINUPS4,38(R3)   NAME4                                           
         FOUT  SINUPS4H                                                         
         MVC   SINUPS5,50(R3)   NAME5                                           
         FOUT  SINUPS5H                                                         
*                                                                               
* DISPLAY CPO ELEMENT IF THERE                                                  
DR10     L     R3,AIO         RECORD AREA                                       
         USING INFCPOEL,R3    CPO EMENT                                         
         MVI   ELCODE,X'05'  CPO ELEMENT                                        
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
         EDIT  (4,2(R3)),(6,SINTCO),2,ZERO=BLANK                                
         FOUT  SINTCOH                                                          
         EDIT  (4,6(R3)),(6,SINTCU1),2,ZERO=BLANK                               
         FOUT  SINTCU1H                                                         
         EDIT  (4,10(R3)),(6,SINTCU2),2,ZERO=BLANK                              
         FOUT  SINTCU2H                                                         
         EDIT  (4,14(R3)),(6,SINTCU3),2,ZERO=BLANK                              
         FOUT  SINTCU3H                                                         
         EDIT  (4,18(R3)),(6,SINTCU4),2,ZERO=BLANK                              
         FOUT  SINTCU4H                                                         
         EDIT  (4,22(R3)),(6,SINTCU5),2,ZERO=BLANK                              
         FOUT  SINTCU5H                                                         
*                                                                               
* DISPLAY TARGET RATIO ELEMENT IF IT IS THERE                                   
DR20     L     R3,AIO         RECORD AREA                                       
         MVI   ELCODE,X'06'  CPO ELEMENT                                        
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
         EDIT  (4,2(R3)),(6,SINTRO),2,ZERO=BLANK                                
         FOUT  SINTROH                                                          
         EDIT  (4,6(R3)),(6,SINTRU1),2,ZERO=BLANK                               
         FOUT  SINTRU1H                                                         
         EDIT  (4,10(R3)),(6,SINTRU2),2,ZERO=BLANK                              
         FOUT  SINTRU2H                                                         
         EDIT  (4,14(R3)),(6,SINTRU3),2,ZERO=BLANK                              
         FOUT  SINTRU3H                                                         
         EDIT  (4,18(R3)),(6,SINTRU4),2,ZERO=BLANK                              
         FOUT  SINTRU4H                                                         
         EDIT  (4,22(R3)),(6,SINTRU5),2,ZERO=BLANK                              
         FOUT  SINTRU5H                                                         
*                                                                               
* DISPLAY TARGET BREAK EVEN ELEMENT IF IT IS PRESENT                            
DR30     L     R3,AIO         RECORD AREA                                       
         MVI   ELCODE,X'07'  CPO ELEMENT                                        
         BAS   RE,GETEL                                                         
         BNE   DR40                                                             
         EDIT  (4,2(R3)),(8,SINBEO),2,ZERO=BLANK                                
         FOUT  SINBEOH                                                          
         EDIT  (4,6(R3)),(8,SINBEU1),2,ZERO=BLANK                               
         FOUT  SINBEU1H                                                         
         EDIT  (4,10(R3)),(8,SINBEU2),2,ZERO=BLANK                              
         FOUT  SINBEU2H                                                         
         EDIT  (4,14(R3)),(8,SINBEU3),2,ZERO=BLANK                              
         FOUT  SINBEU3H                                                         
         EDIT  (4,18(R3)),(8,SINBEU4),2,ZERO=BLANK                              
         FOUT  SINBEU4H                                                         
         EDIT  (4,22(R3)),(8,SINBEU5),2,ZERO=BLANK                              
         FOUT  SINBEU5H                                                         
*                                                                               
* DISPLAY INFO MISC DATA  ELEMENT IF IT IS PRESENT                              
DR40     L     R3,AIO         RECORD AREA                                       
         MVI   ELCODE,X'08'   INFO MISC. ELEMENT                                
         BAS   RE,GETEL                                                         
         USING INFMSCEL,R3                                                      
         BNE   DR50                                                             
         EDIT  (4,INFMSCOR),(7,SINTEO),ZERO=BLANK                               
         FOUT  SINTEOH                                                          
         EDIT  (4,INFMSCBU),(7,SINBUDG),ZERO=BLANK                              
         FOUT  SINBUDGH                                                         
* DISPLAY PRICE ELEMENT IF THERE                                                
DR50     L     R3,AIO         RECORD AREA                                       
         USING INFPRICE,R3    UPSELL PRICE ELEMENT                              
         MVI   ELCODE,X'09'                                                     
         BAS   RE,GETEL                                                         
         BNE   DREND                                                            
         EDIT  (4,2(R3)),(6,SINPRC1),2,ZERO=BLANK                               
         FOUT  SINPRC1H                                                         
         EDIT  (4,6(R3)),(6,SINPRC2),2,ZERO=BLANK                               
         FOUT  SINPRC2H                                                         
         EDIT  (4,10(R3)),(6,SINPRC3),2,ZERO=BLANK                              
         FOUT  SINPRC3H                                                         
         EDIT  (4,14(R3)),(6,SINPRC4),2,ZERO=BLANK                              
         FOUT  SINPRC4H                                                         
         EDIT  (4,18(R3)),(6,SINPRC5),2,ZERO=BLANK                              
         FOUT  SINPRC5H                                                         
         EDIT  (4,22(R3)),(8,SINPRCR),2,ZERO=BLANK                              
         FOUT  SINPRCRH                                                         
*                                                                               
DREND    XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE ORDER FIELDS, SHOULD BE NUMERIC AND NOT NEGATIVE.                    
VRNUM    NTR1                                                                   
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
*        L     R6,8(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R5)                                      
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BE    *+8                                                              
         B     ERRX                                                             
*                                                                               
         L     R5,DMCB+4                                                        
         C     R5,=F'0'            CAN'T BE NEGATIVE                            
         BL    ERRX                                                             
         C     R5,=F'99999'        CAN'T EXCEED 999.99                          
         BH    ERRX                                                             
         XIT1                                                                   
         EJECT                                                                  
* VALIDATE TARGET ORDER AND BUDGET FIELDS. NUMERIC AND NON NEGATIVE.            
VRNUM2   NTR1                                                                   
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
*        L     R6,8(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R5)                                          
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BE    *+8                                                              
         B     ERRX                                                             
*                                                                               
         L     R5,DMCB+4                                                        
         C     R5,=F'0'           CAN'T BE NEGATIVE                             
         BL    ERRX                                                             
         BE    VRNUM10            ALLOWED TO ENTER 0                            
         SR    R4,R4                                                            
         D     R4,=F'100'          GET RID OF CENTS FIELD                       
         C     R4,=F'0'    CENTS NOT ALLOWED,SHOULD BE NO REMAINDER             
         BNE   ERRX                                                             
VRNUM10  ST    R5,DMCB+4                                                        
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE BREAK EVEN FIELDS, SHOULD BE NUMERIC AND NOT NEGATIVE.               
VRNUM3   NTR1                                                                   
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
*        L     R6,8(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R5)                                      
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BE    *+8                                                              
         B     ERRX                                                             
*                                                                               
         L     R5,DMCB+4                                                        
         C     R5,=F'0'            CAN'T BE NEGATIVE                            
         BL    ERRX                                                             
         C     R5,=F'9999999'      CAN'T EXCEED 99999.99                        
         BH    ERRX                                                             
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*                                                                               
         EJECT                                                                  
         SPACE 3                                                                
         GETEL R3,24,ELCODE                                                     
         SPACE 3                                                                
         EJECT                                                                  
ERRX     GOTO1 ERREX                                                            
ZEROS    DC    20X'00'                                                          
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD               SPOOL DSECT                             
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
         PRINT ON                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
*                                                                               
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENINFO                                                      
       ++INCLUDE SPGENNDEF              NETWORK DEFINITION DSECT                
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
*                                                                               
       ++INCLUDE SCSFM9ED  MAINT SCREEN                                         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
* STORAGE DSECT                                                                 
*                                                                               
         ORG   SYSSPARE                                                         
SAVEKEY  DS    CL24                                                             
MYSAVE   DS    CL13                                                             
         SPACE 5                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPSFM2F   09/22/03'                                      
         END                                                                    
