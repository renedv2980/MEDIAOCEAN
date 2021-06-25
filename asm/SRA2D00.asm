*          DATA SET SRA2D00    AT LEVEL 002 AS OF 07/29/02                      
*          DATA SET SRA2D00    AT LEVEL 083 AS OF 04/01/02                      
*PHASE T17600A                                                                  
*INCLUDE BINSR31                                                                
         TITLE '$DISPLAY ADWARE TABLE'                                          
A2D      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,**$A2D**,R9,CLEAR=YES,RR=RE                                
         USING WORKD,RC            RC=A(W/S)                                    
         USING A2SAVED,SVBLOCK                                                  
         ST    RD,SAVERD                                                        
         ST    RE,RELO                                                          
         MVC   IPARMS,0(R1)                                                     
         L     RA,ATWA                                                          
         USING SRA2DFFD,RA         RA=A(TWA)                                    
         BRAS  RE,INIT                                                          
*                                                                               
         L     RF,AUTL                                                          
         USING UTLD,RF                                                          
         MVC   TRMNUM,TNUM                                                      
         TM    TSTAT1,TSTATDDS     DDS IS OK                                    
         BNZ   MAIN02                                                           
         CLC   TAGY,=CL2'MC'       MCCANN IS OK                                 
         BE    MAIN02                                                           
         MVI   FERN,1                                                           
         B     XMOD                                                             
*                                                                               
MAIN02   BRAS  RE,VALTYPE          VALIDATE INPUT FIELDS                        
         BNE   XMOD                                                             
         BRAS  RE,VALKEY                                                        
         BNE   XMOD                                                             
         BRAS  RE,BLDTBLS                                                       
*                                                                               
         LA    RF,SRVL1H           SET CURSOR AFTER FILTERS                     
         OC    VMED,VMED                                                        
         BZ    *+8                                                              
         ST    RF,FADRH                                                         
         OC    VCLT,VCLT                                                        
         BZ    *+8                                                              
         ST    RF,FADRH                                                         
         OC    VPRD,VPRD                                                        
         BZ    *+8                                                              
         ST    RF,FADRH                                                         
         OC    VEST,VEST                                                        
         BZ    *+8                                                              
         ST    RF,FADRH                                                         
*                                                                               
         CLI   TYPE,C'C'           ADWARE CLIENT LIST                           
         BNE   *+12                                                             
         BRAS  RE,DISACLI                                                       
         B     XMOD                                                             
*                                                                               
         CLI   TYPE,C'D'           DDS CLIENT LIST                              
         BNE   *+12                                                             
         BRAS  RE,DISDCLI                                                       
         B     XMOD                                                             
*                                                                               
         CLI   TYPE,C'P'           ADWARE PRODUCT LIST                          
         BNE   *+12                                                             
         BRAS  RE,DISAPRD                                                       
         B     XMOD                                                             
*                                                                               
         CLI   TYPE,C'Q'           DDS PRODUCT LIST                             
         BNE   *+12                                                             
         BRAS  RE,DISDPRD                                                       
         B     XMOD                                                             
*                                                                               
         CLI   TYPE,C'E'           ADWARE ESTIMATE LIST                         
         BNE   *+12                                                             
         BRAS  RE,DISAEST                                                       
         B     XMOD                                                             
*                                                                               
         CLI   TYPE,C'F'           DDS ESTIMATE LIST                            
         BNE   *+12                                                             
         BRAS  RE,DISDEST                                                       
         B     XMOD                                                             
*                                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL VALUES AND BUILD TABLES IN XA                        *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     RF,=V(BINSRCH)                                                   
         A     RF,RELO                                                          
         ST    RF,BINSRCH                                                       
         L     RF,=A(OKMSGS)                                                    
         A     RF,RELO                                                          
         ST    RF,AOKMSGS                                                       
         L     RF,=A(ERRMSGS)                                                   
         A     RF,RELO                                                          
         ST    RF,AERRMSGS                                                      
         L     RF,=A(CPETAB)                                                    
         A     RF,RELO                                                          
         ST    RF,ACPETAB                                                       
*                                                                               
         L     RF,ASYSFACS                                                      
         USING SYSFACD,RF                                                       
         MVC   ASSB,VSSB                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   AGETTXT,CGETTXT                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   ADMGR,CDATAMGR                                                   
*                                                                               
         L     RF,ASSB             GET A(TSAR BLOCK)                            
         USING SSBD,RF                                                          
         ICM   R0,15,SSBTSAR       GET LENGTH OF TSAR BLOCK                     
         TM    SSBTSAR,X'80'       TWO BLOCKS?                                  
         BZ    *+8                                                              
         SLL   R0,1                DOUBLE LENGTH                                
         ST    R0,LTSAR                                                         
         MVC   SYSNAME,SSBSYSN4                                                 
*                                                                               
         ICM   RF,15,SSBTKADR                                                   
         USING TCBD,RF                                                          
         ICM   R0,15,TCBTSAR                                                    
         ST    R0,ATSAR                                                         
         DROP  RF                                                               
*                                                                               
         BRAS  RE,RDTWAB                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE TYPE FIELD                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALTYPE  NTR1  ,                                                                
         MVI   HDRN,1                                                           
         LA    R2,SRVTYPEH         VALIDATE TYPE FIELD                          
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         XR    RF,RF               SEE IF FIELD HAS INPUT                       
         ICM   RF,1,FHIL                                                        
         BZ    EXITL                                                            
*                                                                               
         CLI   FHDA,C'?'           WANT HELP?                                   
         BE    VAL06               YES                                          
*                                                                               
         LA    R3,TYPTAB                                                        
         USING TYPTABD,R3                                                       
         BCTR  RF,0                                                             
VAL02    CLI   TYPTYPE,255                                                      
         BNE   *+12                                                             
         MVI   FERN,2              INVALID INPUT FIELD                          
         B     EXITL                                                            
         EX    RF,VTYPCLC1                                                      
         BE    VAL04                                                            
         EX    RF,VTYPCLC2                                                      
         BE    VAL04                                                            
         AHI   R3,TYPTABL                                                       
         B     VAL02                                                            
*                                                                               
VTYPCLC1 CLC   FHDA(0),TYPUCLC                                                  
VTYPCLC2 CLC   FHDA(0),TYPLCLC                                                  
*                                                                               
VAL04    MVC   TYPE,TYPTYPE                                                     
         MVC   FHDA(L'TYPLCLC),TYPLCLC                                          
         CLC   SVTYPE,TYPE                                                      
         BE    EXITOK                                                           
         NI    FLAG,255-FLSCROLL                                                
         B     EXITOK                                                           
*                                                                               
VAL06    MVI   HDRN,2                                                           
         LA    R2,SRVL1H           OUTPUT HELP INFORMATION                      
         USING FHD,R2                                                           
         LA    R3,TYPTAB                                                        
         USING TYPTABD,R3                                                       
         XR    RF,RF                                                            
*                                                                               
VAL08    CLI   TYPTYPE,255         MUST FORCE EXIT                              
         BE    EXITL                                                            
         MVC   FHDA+00(L'TYPLCLC),TYPLCLC                                       
         MVC   FHDA+12(L'TYPLHELP),TYPLHELP                                     
         AHI   R3,TYPTABL                                                       
         ICM   RF,1,FHLN                                                        
         BZ    EXITL                                                            
         BXH   R2,RF,VAL08                                                      
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FIELD                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   NTR1  ,                                                                
         LA    R2,SRVKEYH                                                       
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BNE   VKY02                                                            
*                                                                               
         CLC   VMED,SVMED          SEE IF FILTERS CHANGED                       
         BE    *+8                                                              
         NI    FLAG,255-FLSCROLL                                                
         CLC   VCLT,SVCLT                                                       
         BE    *+8                                                              
         NI    FLAG,255-FLSCROLL                                                
         CLC   VPRD,SVPRD                                                       
         BE    *+8                                                              
         NI    FLAG,255-FLSCROLL                                                
         CLC   VEST,SVEST                                                       
         BE    *+8                                                              
         NI    FLAG,255-FLSCROLL                                                
         B     EXITOK                                                           
*                                                                               
VKY02    ICM   RF,14,=C',=, '      NO SECOND PORTIONS TO FIELDS                 
         ICM   RF,01,EFFS                                                       
         GOTO1 ASCANNER,PLIST,SRVKEYH,(X'84',SCANBLK),(RF),0                    
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    EXITOK              INPUT TO THIS FIELD IS OPTIONAL              
*                                                                               
         LA    R3,SCANBLK      *** VALIDATE MEDIA                               
         USING SCANBLKD,R3                                                      
         CLI   SC1STLEN,1          MEDIA CAN ONLY BE 1 BYTE                     
         BE    *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVC   VMED,SC1STFLD                                                    
         CLC   VMED,SVMED              SEE IF FILTER CHANGED                    
         BE    *+8                                                              
         NI    FLAG,255-FLSCROLL                                                
         CLI   SC1STFLD,C'T'                                                    
         BE    VKY04                                                            
         CLI   SC1STFLD,C'R'                                                    
         BE    VKY04                                                            
         MVI   FERN,4                                                           
         B     EXITL                                                            
*                                                                               
VKY04    AHI   R0,-1           *** VALIDATE CLIENT                              
         BZ    EXITOK                                                           
         AHI   R3,SCBLKLQ                                                       
         LHI   RF,4                ADWARE CLIENT TYPE                           
         MVI   FERN,5                                                           
         CLI   TYPE,C'C'                                                        
         BE    VKY06                                                            
         CLI   TYPE,C'P'                                                        
         BE    VKY06                                                            
         CLI   TYPE,C'E'                                                        
         BE    VKY06                                                            
         LHI   RF,3                DDS CLIENT TYPE                              
         MVI   FERN,6                                                           
*                                                                               
VKY06    CLM   RF,1,SC1STLEN                                                    
         BNL   *+14                                                             
         MVC   FERRDSP,SC1STNUM                                                 
         B     EXITL                                                            
*                                                                               
         MVI   FERN,0              RESET ERRORS                                 
         XC    FERRDSP,FERRDSP                                                  
         MVC   VCLT,SC1STFLD       SET CLIENT (LEFT ALIGNED)                    
         CLC   VCLT,SVCLT          SEE IF FILTER CHANGED                        
         BE    *+8                                                              
         NI    FLAG,255-FLSCROLL                                                
*                                                                               
         AHI   R0,-1           *** VALIDATE PRODUCT                             
         BZ    EXITOK                                                           
         AHI   R3,SCBLKLQ                                                       
         MVI   FERN,12             CHECK CAN HAVE PRODUCT FILTER                
         CLI   TYPE,C'C'                                                        
         BE    VKYERR                                                           
         CLI   TYPE,C'D'                                                        
         BE    VKYERR                                                           
*                                                                               
         LHI   RF,4                ADWARE PRODUCT TYPE                          
         MVI   FERN,7                                                           
         CLI   TYPE,C'P'                                                        
         BE    VKY08                                                            
         CLI   TYPE,C'E'                                                        
         BE    VKY08                                                            
         LHI   RF,3                DDS PRODUCT TYPE                             
         MVI   FERN,8                                                           
*                                                                               
VKY08    CLM   RF,1,SC1STLEN                                                    
         BL    VKYERR                                                           
         MVI   FERN,0              RESET ERRORS                                 
         MVC   VPRD,SC1STFLD       SET PRODUCT (LEFT ALIGNED)                   
         CLC   VPRD,SVPRD          SEE IF FILTER CHANGED                        
         BE    *+8                                                              
         NI    FLAG,255-FLSCROLL                                                
*                                                                               
         AHI   R0,-1           *** VALIDATE ESTIMATE                            
         BZ    EXITOK                                                           
         AHI   R3,SCBLKLQ                                                       
         MVI   FERN,13             CHECK CAN HAVE ESTIMATE FILTER               
         CLI   TYPE,C'C'                                                        
         BE    VKYERR                                                           
         CLI   TYPE,C'D'                                                        
         BE    VKYERR                                                           
         CLI   TYPE,C'P'                                                        
         BE    VKYERR                                                           
         CLI   TYPE,C'Q'                                                        
         BE    VKYERR                                                           
*                                                                               
         LHI   RF,4                ADWARE ESTIMATE TYPE                         
         MVI   FERN,9                                                           
         CLI   TYPE,C'C'                                                        
         BE    VKY10                                                            
         CLI   TYPE,C'P'                                                        
         BE    VKY10                                                            
         CLI   TYPE,C'E'                                                        
         BE    VKY10                                                            
         LHI   RF,3                DDS ESTIMATE TYPE                            
         MVI   FERN,10                                                          
*                                                                               
VKY10    CLM   RF,1,SC1STLEN                                                    
         BL    VKYERR                                                           
         MVI   FERN,0                                                           
         MVC   VEST,SC1STFLD       SET ESTIMATE (LEFT ALIGNED)                  
         CLC   VEST,SVEST          SEE IF FILTER CHANGED                        
         BNE   *+8                                                              
         NI    FLAG,255-FLSCROLL                                                
*                                                                               
         AHI   R0,-1                                                            
         BZ    EXITOK                                                           
         AHI   R3,SCBLKLQ                                                       
         MVI   FERN,11                                                          
*                                                                               
VKYERR   MVC   FERRDSP,SC1STNUM    SET ERROR DISPLACEMENT                       
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TABLES IN XA                                                  *         
***********************************************************************         
         SPACE 1                                                                
BLDTBLS  NTR1  ,                                                                
         BRAS  RE,ON31                                                          
*                                                                               
         L     R0,ATSAR            CLEAR TSAR BUFFER                            
         L     R1,LTSAR                                                         
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   BINPAR2,ATSAR       SET A(TABLE)                                 
*                                                                               
         L     RE,LTSAR            GET NUMBER OF ENTRIES                        
         SRDL  RE,32                                                            
*                                                                               
         LHI   R0,ACLIRLQ                                                       
         LHI   R1,ACLIKLQ                                                       
         CLI   TYPE,C'C'           ADWARE CLIENT                                
         BE    BTB02                                                            
         LHI   R0,DCLIRLQ                                                       
         LHI   R1,DCLIKLQ                                                       
         CLI   TYPE,C'D'           DDS CLIENT                                   
         BE    BTB02                                                            
         LHI   R0,APRDRLQ                                                       
         LHI   R1,APRDKLQ                                                       
         CLI   TYPE,C'P'           ADWARE PRODUCT                               
         BE    BTB02                                                            
         LHI   R0,DPRDRLQ                                                       
         LHI   R1,DPRDKLQ                                                       
         CLI   TYPE,C'Q'           DDS PRODUCT                                  
         BE    BTB02                                                            
         LHI   R0,AESTRLQ                                                       
         LHI   R1,AESTKLQ                                                       
         CLI   TYPE,C'E'           ADWARE ESTIMATE                              
         BE    BTB02                                                            
         LHI   R0,DESTRLQ                                                       
         LHI   R1,DESTKLQ                                                       
         CLI   TYPE,C'F'           DDS ESTIMATE                                 
         BE    BTB02                                                            
         DC    H'0'                                                             
*                                                                               
BTB02    ST    R0,BINPAR4          SET RECORD LENGTH                            
         ST    R1,BINPAR5          SET KEY LENGTH                               
         MVI   BINPAR5,0           SET DISP TO KEY IN RECORD                    
         DR    RE,R0                                                            
         ST    RF,BINPAR6          SET NUMBER OF ENTRIES                        
*                                                                               
         MVI   BINPAR4,X'01'       SET AN END KEY                               
         GOTO1 BINSRCH,BINPARM,EFFS                                             
*                                                                               
         L     R2,ACPETAB          BUILD ADWARE CLIENT TABLE                    
         USING CPED,R2                                                          
BTB04    CLI   0(R2),255                                                        
         BE    BLDTBLX                                                          
         XC    WORK,WORK                                                        
*                                                                               
         CLI   TYPE,C'C'           ADWARE CLIENT                                
         BNE   BTB06                                                            
X        USING ACLID,WORK                                                       
         MVC   X.ACMED,CPEMED                                                   
         MVC   X.ACCLT,CPECLT                                                   
         MVC   X.ACDCLT,CPEDCLT                                                 
         B     BTB18                                                            
*                                                                               
BTB06    CLI   TYPE,C'D'           DDS CLIENT                                   
         BNE   BTB08                                                            
X        USING DCLID,WORK                                                       
         MVC   X.DCMED,CPEMED                                                   
         MVC   X.DCCLT,CPECLT                                                   
         MVC   X.DCDCLT,CPEDCLT                                                 
         B     BTB18                                                            
*                                                                               
BTB08    CLI   TYPE,C'P'           ADWARE PRODUCT                               
         BNE   BTB10                                                            
X        USING APRDD,WORK                                                       
         MVC   X.APMED,CPEMED                                                   
         MVC   X.APCLT,CPECLT                                                   
         MVC   X.APPRD,CPEPRD                                                   
         MVC   X.APDCLT,CPEDCLT                                                 
         MVC   X.APDPRD,CPEDPRD                                                 
         B     BTB18                                                            
*                                                                               
BTB10    CLI   TYPE,C'Q'           DDS PRODUCT                                  
         BNE   BTB12                                                            
X        USING DPRDD,WORK                                                       
         MVC   X.DPMED,CPEMED                                                   
         MVC   X.DPDCLT,CPEDCLT                                                 
         MVC   X.DPDPRD,CPEDPRD                                                 
         MVC   X.DPCLT,CPECLT                                                   
         MVC   X.DPPRD,CPEPRD                                                   
         B     BTB18                                                            
*                                                                               
BTB12    CLI   TYPE,C'E'           ADWARE ESTIMATE                              
         BNE   BTB14                                                            
X        USING AESTD,WORK                                                       
         MVC   X.AEMED,CPEMED                                                   
         MVC   X.AECLT,CPECLT                                                   
         MVC   X.AEPRD,CPEPRD                                                   
         MVC   X.AEEST,CPEEST                                                   
         MVC   X.AEDCLT,CPEDCLT                                                 
         MVC   X.AEDPRD,CPEDPRD                                                 
         MVC   X.AEDEST,CPEDEST                                                 
         B     BTB18                                                            
*                                                                               
BTB14    CLI   TYPE,C'F'           DDS ESTIMATE                                 
         BNE   BTB16                                                            
X        USING DESTD,WORK                                                       
         MVC   X.DEMED,CPEMED                                                   
         MVC   X.DEDCLT,CPEDCLT                                                 
         MVC   X.DEDPRD,CPEDPRD                                                 
         MVC   X.DEDEST,CPEDEST                                                 
         MVC   X.DECLT,CPECLT                                                   
         MVC   X.DEPRD,CPEPRD                                                   
         MVC   X.DEEST,CPEEST                                                   
         B     BTB18                                                            
*                                                                               
BTB16    DC    H'0'                                                             
*                                                                               
BTB18    MVI   BINPAR4,X'01'       INSERT IF NOT FOUND                          
         GOTO1 BINSRCH,BINPARM,WORK                                             
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         AHI   R2,CPELQ            NEXT IN TABLE                                
         B     BTB04                                                            
*                                                                               
BLDTBLX  BRAS  RE,OFF31                                                         
         B     EXITOK                                                           
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* DISPLAY ADWARE - DDS CLIENT INFORMATION                             *         
***********************************************************************         
         SPACE 1                                                                
DISACLI  NTR1  ,                                                                
         MVI   HDRN,3                                                           
         BRAS  RE,ON31                                                          
*                                                                               
         XC    WORK,WORK           FILTER FIRST RECORD IN LIST                  
X        USING ACLID,WORK                                                       
         MVC   X.ACMED,VMED                                                     
         MVC   X.ACCLT,VCLT                                                     
*                                                                               
         TM    FLAG,FLSCROLL       CAN WE SCROLL?                               
         BZ    DAC02                                                            
         MVC   X.ACMED,SVHMED                                                   
         MVC   X.ACCLT,SVHCLT                                                   
         MVC   X.ACDCLT,SVHDCLT                                                 
*                                                                               
DAC02    MVI   BINPAR4,X'02'       SET READ HIGH                                
         GOTO1 BINSRCH,BINPARM,WORK                                             
         DROP  X                                                                
*                                                                               
         L     R3,0(R1)                                                         
         USING ACLID,R3                                                         
         CLC   ACLID(ACLIKLQ),EFFS                                              
         BE    DAC12                                                            
*                                                                               
         OC    VMED,VMED           SET A MEDIA?                                 
         BZ    *+14                NO                                           
         CLC   ACMED,VMED                                                       
         BNE   DAC12                                                            
*                                                                               
         LA    R2,SRVL1H                                                        
         USING FHD,R2                                                           
         LA    R5,SRVLAST          R5 = A(END OF SCREEN)                        
         BCTR  R5,0                                                             
         XR    R4,R4                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
*                                                                               
         LHI   R0,CLIDLQ           CLIENT DISPLAY LENGTH                        
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0                                                            
         STH   RF,FULL+2           FULL+2(2) = NUMBER THAT FIT ON LINE          
*                                                                               
DAC04    LH    R0,FULL+2           R0 = NUMBER OF COLUMNS                       
         XR    RF,RF               RF = DISPLACEMENT INTO LINE                  
*                                                                               
DAC06    LA    R2,SRVL1H           R2 = CURRENT LINE                            
         OI    FHAT,FHATHI                                                      
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
         MVC   0(12,RE),=CL12'M/ACli  Cli'                                      
         XC    LAST,LAST                                                        
         B     DAC10                                                            
*                                                                               
DAC08    CLC   ACLID(ACLIKLQ),EFFS                                              
         BE    DAC12               LAST RECORD                                  
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   ACMED,VMED                                                       
         BNE   DAC12                                                            
*                                                                               
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
         CLC   ACMED,LAST+ACMED-ACLID                                           
         BE    *+14                SAME MEDIA AS BEFORE                         
         MVC   0(1,RE),ACMED                                                    
         MVI   1(RE),C'/'                                                       
*                                                                               
         AHI   RE,2                                                             
         CLC   ACCLT,LAST+ACCLT-ACLID                                           
         BE    *+10                SAME ADWARE CLIENT AS BEFORE                 
         MVC   0(L'ACCLT,RE),ACCLT                                              
*                                                                               
         AHI   RE,L'ACCLT+2                                                     
         MVC   0(3,RE),ACDCLT                                                   
*                                                                               
         MVC   LAST(ACLIRLQ),ACLID                                              
         AHI   R3,ACLIRLQ          NEXT RECORD                                  
*                                                                               
DAC10    IC    R4,FHLN             NEXT ROW IN COLUMN                           
         BXLE  R2,R4,DAC08                                                      
         AH    RF,FULL             NEXT COLUMN                                  
         BCT   R0,DAC06                                                         
*                                                                               
DAC12    MVC   SVHMED,VMED         SET CURRENT FILTERS                          
         MVC   SVHCLT,VCLT                                                      
         MVC   SVHPRD,VPRD                                                      
         MVC   SVHEST,VEST                                                      
         XC    SVHDCLT,SVHDCLT                                                  
         XC    SVHDPRD,SVHDPRD                                                  
         XC    SVHDPRD,SVHDPRD                                                  
*                                                                               
         CLC   ACLID(ACLIKLQ),EFFS LAST RECORD IN LIST?                         
         BE    DAC14               YES                                          
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   ACMED,VMED          PAST END OF FILTER?                          
         BNE   DAC14               YES                                          
*                                                                               
         MVI   HDRN,9                                                           
         MVC   SVHMED,ACMED                                                     
         MVC   SVHCLT,ACCLT                                                     
         MVC   SVHDCLT,ACDCLT                                                   
*                                                                               
DAC14    BRAS  RE,OFF31                                                         
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY DDS - ADWARE CLIENT INFORMATION                             *         
***********************************************************************         
         SPACE 1                                                                
DISDCLI  NTR1  ,                                                                
         MVI   HDRN,4                                                           
         BRAS  RE,ON31                                                          
*                                                                               
         XC    WORK,WORK           FILTER FIRST RECORD IN LIST                  
X        USING DCLID,WORK                                                       
         MVC   X.DCMED,VMED                                                     
         MVC   X.DCDCLT,VCLT                                                    
*                                                                               
         TM    FLAG,FLSCROLL       CAN WE SCROLL?                               
         BZ    DDC02                                                            
         MVC   X.DCMED,SVHMED                                                   
         MVC   X.DCDCLT,SVHDCLT                                                 
         MVC   X.DCCLT,SVHCLT                                                   
*                                                                               
DDC02    MVI   BINPAR4,X'02'       SET READ HIGH                                
         GOTO1 BINSRCH,BINPARM,WORK                                             
         DROP  X                                                                
*                                                                               
         L     R3,0(R1)                                                         
         USING DCLID,R3                                                         
         CLC   DCLID(DCLIKLQ),EFFS                                              
         BE    DDC12                                                            
*                                                                               
         OC    VMED,VMED           SET A MEDIA?                                 
         BZ    *+14                NO                                           
         CLC   DCMED,VMED                                                       
         BNE   DDC12                                                            
*                                                                               
         LA    R2,SRVL1H                                                        
         USING FHD,R2                                                           
         LA    R5,SRVLAST          R5 = A(END OF SCREEN)                        
         BCTR  R5,0                                                             
         XR    R4,R4                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
*                                                                               
         LHI   R0,CLIDLQ           CLIENT DISPLAY LENGTH                        
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0                                                            
         STH   RF,FULL+2           FULL+2(2) = NUMBER THAT FIT ON LINE          
*                                                                               
DDC04    LH    R0,FULL+2           R0 = NUMBER OF COLUMNS                       
         XR    RF,RF               RF = DISPLACEMENT INTO LINE                  
*                                                                               
DDC06    LA    R2,SRVL1H           R2 = CURRENT LINE                            
         OI    FHAT,FHATHI                                                      
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
         MVC   0(12,RE),=CL12'M/Cli  ACli'                                      
         XC    LAST,LAST                                                        
         B     DDC10                                                            
*                                                                               
DDC08    CLC   DCLID(DCLIKLQ),EFFS                                              
         BE    DDC12               LAST RECORD                                  
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   DCMED,VMED                                                       
         BNE   DDC12                                                            
*                                                                               
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
*                                                                               
         CLC   DCMED,LAST+DCMED-DCLID                                           
         BE    *+14                SAME MEDIA AS BEFORE                         
         MVC   0(1,RE),DCMED                                                    
         MVI   1(RE),C'/'                                                       
*                                                                               
         AHI   RE,2                                                             
         CLC   DCDCLT,LAST+DCDCLT-DCLID                                         
         BE    *+10                SAME DDS CLIENT AS BEFORE                    
         MVC   0(L'DCDCLT,RE),DCDCLT                                            
*                                                                               
         AHI   RE,L'DCDCLT+2                                                    
         MVC   0(L'DCCLT,RE),DCCLT                                              
*                                                                               
         MVC   LAST(DCLIRLQ),DCLID                                              
         AHI   R3,DCLIRLQ          NEXT RECORD                                  
*                                                                               
DDC10    IC    R4,FHLN             NEXT ROW IN COLUMN                           
         BXLE  R2,R4,DDC08                                                      
         AH    RF,FULL             NEXT COLUMN                                  
         BCT   R0,DDC06                                                         
*                                                                               
DDC12    MVC   SVHMED,VMED         SET CURRENT FILTERS                          
         MVC   SVHDCLT,VCLT                                                     
         MVC   SVHDPRD,VPRD                                                     
         MVC   SVHDEST,VEST                                                     
         XC    SVHCLT,SVHCLT                                                    
         XC    SVHPRD,SVHPRD                                                    
         XC    SVHEST,SVHEST                                                    
*                                                                               
         CLC   DCLID(DCLIKLQ),EFFS LAST RECORD IN LIST?                         
         BE    DDC14               YES                                          
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   DCMED,VMED          PAST END OF FILTER?                          
         BNE   DDC14               YES                                          
*                                                                               
         MVI   HDRN,10                                                          
         MVC   SVHMED,DCMED                                                     
         MVC   SVHCLT,DCCLT                                                     
         MVC   SVHDCLT,DCDCLT                                                   
*                                                                               
DDC14    BRAS  RE,OFF31                                                         
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY ADWARE - DDS PRODUCT INFORMATION                            *         
***********************************************************************         
         SPACE 1                                                                
DISAPRD  NTR1  ,                                                                
         MVI   HDRN,5                                                           
         BRAS  RE,ON31                                                          
*                                                                               
         XC    WORK,WORK           FILTER FIRST RECORD IN LIST                  
X        USING APRDD,WORK                                                       
         MVC   X.APMED,VMED                                                     
         MVC   X.APCLT,VCLT                                                     
         MVC   X.APPRD,VPRD                                                     
*                                                                               
         TM    FLAG,FLSCROLL       CAN WE SCROLL?                               
         BZ    DAP02                                                            
         MVC   X.APMED,SVHMED                                                   
         MVC   X.APCLT,SVHCLT                                                   
         MVC   X.APPRD,SVHPRD                                                   
         MVC   X.APDCLT,SVHDCLT                                                 
         MVC   X.APDPRD,SVHDPRD                                                 
*                                                                               
DAP02    MVI   BINPAR4,X'02'       SET READ HIGH                                
         GOTO1 BINSRCH,BINPARM,WORK                                             
         DROP  X                                                                
*                                                                               
         L     R3,0(R1)                                                         
         USING APRDD,R3                                                         
         CLC   APRDD(APRDKLQ),EFFS END OF FILE?                                 
         BE    DAP12               NO                                           
         OC    VMED,VMED           SET A MEDIA?                                 
         BZ    *+14                NO                                           
         CLC   APMED,VMED                                                       
         BNE   DAP12                                                            
         OC    VCLT,VCLT           SET A CLIENT?                                
         BZ    *+14                NO                                           
         CLC   APCLT,VCLT                                                       
         BNE   DAP12                                                            
*                                                                               
         LA    R2,SRVL1H                                                        
         USING FHD,R2                                                           
         LA    R5,SRVLAST          R5 = A(END OF SCREEN)                        
         BCTR  R5,0                                                             
         XR    R4,R4                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
*                                                                               
         LHI   R0,PRDDLQ           CLIENT DISPLAY LENGTH                        
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0                                                            
         STH   RF,FULL+2           FULL+2(2) = NUMBER THAT FIT ON LINE          
*                                                                               
DAP04    LH    R0,FULL+2           R0 = NUMBER OF COLUMNS                       
         XR    RF,RF               RF = DISPLACEMENT INTO LINE                  
*                                                                               
DAP06    LA    R2,SRVL1H           R2 = CURRENT LINE                            
         OI    FHAT,FHATHI                                                      
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
         MVC   0(21,RE),=CL21'M/ACli/APrd  Cli/Prd'                             
         XC    LAST,LAST                                                        
         B     DAP10                                                            
*                                                                               
DAP08    CLC   APRDD(APRDKLQ),EFFS                                              
         BE    DAP12               LAST RECORD                                  
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   APMED,VMED                                                       
         BNE   DAP12                                                            
         OC    VCLT,VCLT           CLIENT FILTER?                               
         BZ    *+14                NO                                           
         CLC   APCLT,VCLT                                                       
         BNE   DAP12                                                            
*                                                                               
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
*                                                                               
         CLC   APMED,LAST+APMED-APRDD                                           
         BE    *+14                SAME MEDIA AS BEFORE                         
         MVC   0(1,RE),APMED                                                    
         MVI   1(RE),C'/'                                                       
         AHI   RE,2                                                             
         CLC   APCLT,LAST+APCLT-APRDD                                           
         BE    *+14                SAME ADWARE CLIENT AS BEFORE                 
         MVC   0(L'APCLT,RE),APCLT                                              
         MVI   L'APCLT(RE),C'/'                                                 
         AHI   RE,L'APCLT+1                                                     
         MVC   0(L'APPRD,RE),APPRD                                              
         AHI   RE,L'APPRD+2                                                     
*                                                                               
         CLC   APDCLT,LAST+APDCLT-APRDD                                         
         BE    *+14                SAME DDS CLIENT AS BEFORE                    
         MVC   0(L'APDCLT,RE),APDCLT                                            
         MVI   L'APDCLT(RE),C'/'                                                
         AHI   RE,L'APDCLT+1                                                    
         MVC   0(L'APDPRD,RE),APDPRD                                            
*                                                                               
         MVC   LAST(APRDRLQ),APRDD                                              
         AHI   R3,APRDRLQ          NEXT RECORD                                  
*                                                                               
DAP10    IC    R4,FHLN             NEXT ROW IN COLUMN                           
         BXLE  R2,R4,DAP08                                                      
         AH    RF,FULL             NEXT COLUMN                                  
         BCT   R0,DAP06                                                         
*                                                                               
DAP12    MVC   SVHMED,VMED         SET CURRENT FILTERS                          
         MVC   SVHCLT,VCLT                                                      
         MVC   SVHPRD,VPRD                                                      
         MVC   SVHEST,VEST                                                      
         XC    SVHDCLT,SVHDCLT                                                  
         XC    SVHDPRD,SVHDPRD                                                  
         XC    SVHDEST,SVHDEST                                                  
*                                                                               
         CLC   APRDD(APRDKLQ),EFFS LAST RECORD IN LIST?                         
         BE    DAP14               YES                                          
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   APMED,VMED                                                       
         BNE   DAP14                                                            
         OC    VCLT,VCLT           CLIENT FILTER?                               
         BZ    *+14                NO                                           
         CLC   APCLT,VCLT                                                       
         BNE   DAP14                                                            
*                                                                               
         MVI   HDRN,11                                                          
         MVC   SVHMED,APMED                                                     
         MVC   SVHCLT,APCLT                                                     
         MVC   SVHPRD,APPRD                                                     
         MVC   SVHDCLT,APDCLT                                                   
         MVC   SVHDPRD,APDPRD                                                   
*                                                                               
DAP14    BRAS  RE,OFF31                                                         
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY DDS - ADWARE PRODUCT INFORMATION                            *         
***********************************************************************         
         SPACE 1                                                                
DISDPRD  NTR1  ,                                                                
         MVI   HDRN,6                                                           
         BRAS  RE,ON31                                                          
*                                                                               
         XC    WORK,WORK           FILTER FIRST RECORD IN LIST                  
X        USING DPRDD,WORK                                                       
         MVC   X.DPMED,VMED                                                     
         MVC   X.DPDCLT,VCLT                                                    
         MVC   X.DPDPRD,VPRD                                                    
*                                                                               
         TM    FLAG,FLSCROLL       CAN WE SCROLL?                               
         BZ    DDP02                                                            
         MVC   X.DPMED,SVHMED                                                   
         MVC   X.DPDCLT,SVHDCLT                                                 
         MVC   X.DPDPRD,SVHDPRD                                                 
         MVC   X.DPCLT,SVHCLT                                                   
         MVC   X.DPPRD,SVHPRD                                                   
*                                                                               
DDP02    MVI   BINPAR4,X'02'       SET READ HIGH                                
         GOTO1 BINSRCH,BINPARM,WORK                                             
         DROP  X                                                                
*                                                                               
         L     R3,0(R1)                                                         
         USING DPRDD,R3                                                         
         CLC   DPRDD(DPRDKLQ),EFFS END OF FILE?                                 
         BE    DDP12               NO                                           
         OC    VMED,VMED           SET A MEDIA?                                 
         BZ    *+14                NO                                           
         CLC   DPMED,VMED                                                       
         BNE   DDP12                                                            
         OC    VCLT,VCLT           SET A CLIENT?                                
         BZ    *+14                NO                                           
         CLC   DPDCLT,VCLT                                                      
         BNE   DDP12                                                            
*                                                                               
         LA    R2,SRVL1H                                                        
         USING FHD,R2                                                           
         LA    R5,SRVLAST          R5 = A(END OF SCREEN)                        
         BCTR  R5,0                                                             
         XR    R4,R4                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
*                                                                               
         LHI   R0,PRDDLQ           CLIENT DISPLAY LENGTH                        
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0                                                            
         STH   RF,FULL+2           FULL+2(2) = NUMBER THAT FIT ON LINE          
*                                                                               
DDP04    LH    R0,FULL+2           R0 = NUMBER OF COLUMNS                       
         XR    RF,RF               RF = DISPLACEMENT INTO LINE                  
*                                                                               
DDP06    LA    R2,SRVL1H           R2 = CURRENT LINE                            
         OI    FHAT,FHATHI                                                      
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
         MVC   0(21,RE),=CL21'M/Cli/Prd  ACli/APrd'                             
         XC    LAST,LAST                                                        
         B     DDP10                                                            
*                                                                               
DDP08    CLC   DPRDD(DPRDKLQ),EFFS                                              
         BE    DDP12               LAST RECORD                                  
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   DPMED,VMED                                                       
         BNE   DDP12                                                            
         OC    VCLT,VCLT           CLIENT FILTER?                               
         BZ    *+14                NO                                           
         CLC   DPDCLT,VCLT                                                      
         BNE   DDP12                                                            
*                                                                               
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
*                                                                               
         CLC   DPMED,LAST+DPMED-DPRDD                                           
         BE    *+14                SAME MEDIA AS BEFORE                         
         MVC   0(1,RE),DPMED                                                    
         MVI   1(RE),C'/'                                                       
         AHI   RE,2                                                             
         CLC   DPDCLT,LAST+DPDCLT-DPRDD                                         
         BE    *+14                SAME DDS CLIENT AS BEFORE                    
         MVC   0(L'DPDCLT,RE),DPDCLT                                            
         MVI   L'DPDCLT(RE),C'/'                                                
         AHI   RE,L'DPDCLT+1                                                    
         MVC   0(L'DPDPRD,RE),DPDPRD                                            
         AHI   RE,L'DPDPRD+2                                                    
*                                                                               
         CLC   DPCLT,LAST+DPCLT-DPRDD                                           
         BE    *+14                SAME ADWARE CLIENT AS BEFORE                 
         MVC   0(L'DPCLT,RE),DPCLT                                              
         MVI   L'DPCLT(RE),C'/'                                                 
         AHI   RE,L'DPCLT+1                                                     
         MVC   0(L'DPPRD,RE),DPPRD                                              
*                                                                               
         MVC   LAST(DPRDRLQ),DPRDD                                              
         AHI   R3,DPRDRLQ          NEXT RECORD                                  
*                                                                               
DDP10    IC    R4,FHLN             NEXT ROW IN COLUMN                           
         BXLE  R2,R4,DDP08                                                      
         AH    RF,FULL             NEXT COLUMN                                  
         BCT   R0,DDP06                                                         
*                                                                               
DDP12    MVC   SVHMED,VMED         SET CURRENT FILTERS                          
         MVC   SVHDCLT,VCLT                                                     
         MVC   SVHDPRD,VPRD                                                     
         XC    SVHCLT,SVHCLT                                                    
         XC    SVHPRD,SVHPRD                                                    
*                                                                               
         CLC   DPRDD(DPRDKLQ),EFFS LAST RECORD IN LIST?                         
         BE    DDP14               YES                                          
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   DPMED,VMED          PAST END OF FILTER?                          
         BNE   DDP14               YES                                          
         OC    VCLT,VCLT           CLIENT FILTER?                               
         BZ    *+14                NO                                           
         CLC   DPDCLT,VCLT         PAST END OF FILTER?                          
         BNE   DDP14               YES                                          
*                                                                               
         MVI   HDRN,12                                                          
         MVC   SVHMED,DPMED                                                     
         MVC   SVHCLT,DPCLT                                                     
         MVC   SVHPRD,DPPRD                                                     
         MVC   SVHDCLT,DPDCLT                                                   
         MVC   SVHDPRD,DPDPRD                                                   
*                                                                               
DDP14    BRAS  RE,OFF31                                                         
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY ADWARE - DDS ESTIMATE INFORMATION                           *         
***********************************************************************         
         SPACE 1                                                                
DISAEST  NTR1  ,                                                                
         MVI   HDRN,7                                                           
         BRAS  RE,ON31                                                          
*                                                                               
         XC    WORK,WORK           FILTER FIRST RECORD IN LIST                  
X        USING AESTD,WORK                                                       
         MVC   X.AEMED,VMED                                                     
         MVC   X.AECLT,VCLT                                                     
         MVC   X.AEPRD,VPRD                                                     
         MVC   X.AEEST,VEST                                                     
*                                                                               
         TM    FLAG,FLSCROLL       CAN WE SCROLL?                               
         BZ    DAE02                                                            
         MVC   X.AEMED,SVHMED                                                   
         MVC   X.AECLT,SVHCLT                                                   
         MVC   X.AEPRD,SVHPRD                                                   
         MVC   X.AEEST,SVHEST                                                   
         MVC   X.AEDCLT,SVHDCLT                                                 
         MVC   X.AEDPRD,SVHDPRD                                                 
         MVC   X.AEDEST,SVHDEST                                                 
*                                                                               
DAE02    MVI   BINPAR4,X'02'       SET READ HIGH                                
         GOTO1 BINSRCH,BINPARM,WORK                                             
         DROP  X                                                                
*                                                                               
         L     R3,0(R1)                                                         
         USING AESTD,R3                                                         
         CLC   AESTD(AESTKLQ),EFFS END OF FILE?                                 
         BE    DAE12               NO                                           
         OC    VMED,VMED           SET A MEDIA?                                 
         BZ    *+14                NO                                           
         CLC   AEMED,VMED                                                       
         BNE   DAE12                                                            
         OC    VCLT,VCLT           SET A CLIENT?                                
         BZ    *+14                NO                                           
         CLC   AECLT,VCLT                                                       
         BNE   DAE12                                                            
         OC    VPRD,VPRD           SET A PRODUCT?                               
         BZ    *+14                NO                                           
         CLC   AEPRD,VPRD                                                       
         BNE   DAE12                                                            
*                                                                               
         LA    R2,SRVL1H                                                        
         USING FHD,R2                                                           
         LA    R5,SRVLAST          R5 = A(END OF SCREEN)                        
         BCTR  R5,0                                                             
         XR    R4,R4                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
*                                                                               
         LHI   R0,ESTDLQ           CLIENT DISPLAY LENGTH                        
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0                                                            
         STH   RF,FULL+2           FULL+2(2) = NUMBER THAT FIT ON LINE          
*                                                                               
DAE04    LH    R0,FULL+2           R0 = NUMBER OF COLUMNS                       
         XR    RF,RF               RF = DISPLACEMENT INTO LINE                  
*                                                                               
DAE06    LA    R2,SRVL1H           R2 = CURRENT LINE                            
         OI    FHAT,FHATHI                                                      
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
         MVC   0(30,RE),=CL30'M/ACli/APrd/AEst  Cli/Prd/Est'                    
         XC    LAST,LAST                                                        
         B     DAE10                                                            
*                                                                               
DAE08    CLC   AESTD(AESTKLQ),EFFS                                              
         BE    DAE12               LAST RECORD                                  
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   AEMED,VMED                                                       
         BNE   DAE12                                                            
         OC    VCLT,VCLT           CLIENT FILTER?                               
         BZ    *+14                NO                                           
         CLC   AECLT,VCLT                                                       
         BNE   DAE12                                                            
         OC    VPRD,VPRD           PRODUCT FILTER?                              
         BZ    *+14                NO                                           
         CLC   AEPRD,VPRD                                                       
         BNE   DAE12                                                            
*                                                                               
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
*                                                                               
         CLC   AEMED,LAST+AEMED-AESTD                                           
         BE    *+14                SAME MEDIA AS BEFORE                         
         MVC   0(L'AEMED,RE),AEMED                                              
         MVI   L'AEMED(RE),C'/'                                                 
*                                                                               
         AHI   RE,L'AEMED+1                                                     
         CLC   AECLT,LAST+AECLT-AESTD                                           
         BE    *+14                SAME ADWARE CLIENT AS BEFORE                 
         MVC   0(L'AECLT,RE),AECLT                                              
         MVI   L'AECLT(RE),C'/'                                                 
*                                                                               
         AHI   RE,L'AECLT+1                                                     
         CLC   AEPRD,LAST+AEPRD-AESTD                                           
         BE    *+14                SAME ADWARE PRODUCT AS BEFORE                
         MVC   0(L'AEPRD,RE),AEPRD                                              
         MVI   L'AEPRD(RE),C'/'                                                 
*                                                                               
         AHI   RE,L'AEPRD+1                                                     
         MVC   0(L'AEEST,RE),AEEST                                              
         AHI   RE,L'AEEST+2                                                     
*                                                                               
         CLC   AEDCLT,LAST+AEDCLT-AESTD                                         
         BE    *+14                SAME DDS CLIENT AS BEFORE                    
         MVC   0(L'AEDCLT,RE),AEDCLT                                            
         MVI   L'AEDCLT(RE),C'/'                                                
*                                                                               
         AHI   RE,L'AEDCLT+1                                                    
         CLC   AEDPRD,LAST+AEDPRD-AESTD                                         
         BE    *+14                SAME DDS PRODUCT AS BEFORE                   
         MVC   0(L'AEDPRD,RE),AEDPRD                                            
         MVI   L'AEDPRD(RE),C'/'                                                
*                                                                               
         AHI   RE,L'AEDPRD+1                                                    
         MVC   0(L'AEDEST,RE),AEDEST                                            
*                                                                               
         MVC   LAST(AESTRLQ),AESTD                                              
         AHI   R3,AESTRLQ          NEXT RECORD                                  
*                                                                               
DAE10    IC    R4,FHLN             NEXT ROW IN COLUMN                           
         BXLE  R2,R4,DAE08                                                      
         AH    RF,FULL             NEXT COLUMN                                  
         BCT   R0,DAE06                                                         
*                                                                               
DAE12    MVC   SVHMED,VMED         SET CURRENT FILTERS                          
         MVC   SVHCLT,VCLT                                                      
         MVC   SVHPRD,VPRD                                                      
         MVC   SVHEST,VEST                                                      
         XC    SVHDCLT,SVHDCLT                                                  
         XC    SVHDPRD,SVHDPRD                                                  
         XC    SVHDEST,SVHDEST                                                  
*                                                                               
         CLC   AESTD(AESTKLQ),EFFS LAST RECORD IN LIST?                         
         BE    DAE14               YES                                          
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   AEMED,VMED                                                       
         BNE   DAE14                                                            
         OC    VCLT,VCLT           CLIENT FILTER?                               
         BZ    *+14                NO                                           
         CLC   AECLT,VCLT                                                       
         BNE   DAE14                                                            
         OC    VPRD,VPRD           PRODUCT FILTER?                              
         BZ    *+14                NO                                           
         CLC   AEPRD,VPRD                                                       
         BNE   DAE14                                                            
*                                                                               
         MVI   HDRN,13                                                          
         MVC   SVHMED,AEMED                                                     
         MVC   SVHCLT,AECLT                                                     
         MVC   SVHPRD,AEPRD                                                     
         MVC   SVHEST,AEEST                                                     
         MVC   SVHDCLT,AEDCLT                                                   
         MVC   SVHDPRD,AEDPRD                                                   
         MVC   SVHDEST,AEDEST                                                   
*                                                                               
DAE14    BRAS  RE,OFF31                                                         
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY DDS - ADWARE ESTIMATE INFORMATION                           *         
***********************************************************************         
         SPACE 1                                                                
DISDEST  NTR1  ,                                                                
         MVI   HDRN,8                                                           
         BRAS  RE,ON31                                                          
*                                                                               
         XC    WORK,WORK           FILTER FIRST RECORD IN LIST                  
X        USING DESTD,WORK                                                       
         MVC   X.DEMED,VMED                                                     
         MVC   X.DEDCLT,VCLT                                                    
         MVC   X.DEDPRD,VPRD                                                    
         MVC   X.DEDEST,VEST                                                    
*                                                                               
         TM    FLAG,FLSCROLL       CAN WE SCROLL?                               
         BZ    DDE02                                                            
         MVC   X.DEMED,SVHMED                                                   
         MVC   X.DEDCLT,SVHDCLT                                                 
         MVC   X.DEDPRD,SVHDPRD                                                 
         MVC   X.DEDEST,SVHDEST                                                 
         MVC   X.DECLT,SVHCLT                                                   
         MVC   X.DEPRD,SVHPRD                                                   
         MVC   X.DEEST,SVHEST                                                   
*                                                                               
DDE02    MVI   BINPAR4,X'02'       SET READ HIGH                                
         GOTO1 BINSRCH,BINPARM,WORK                                             
         DROP  X                                                                
*                                                                               
         L     R3,0(R1)                                                         
         USING DESTD,R3                                                         
         CLC   DESTD(DESTKLQ),EFFS END OF FILE?                                 
         BE    DDE12               NO                                           
         OC    VMED,VMED           SET A MEDIA?                                 
         BZ    *+14                NO                                           
         CLC   DEMED,VMED                                                       
         BNE   DDE12                                                            
         OC    VCLT,VCLT           SET A CLIENT?                                
         BZ    *+14                NO                                           
         CLC   DEDCLT,VCLT                                                      
         BNE   DDE12                                                            
         OC    VPRD,VPRD           SET A PRODUCT?                               
         BZ    *+14                NO                                           
         CLC   DEDPRD,VPRD                                                      
         BNE   DDE12                                                            
*                                                                               
         LA    R2,SRVL1H                                                        
         USING FHD,R2                                                           
         LA    R5,SRVLAST          R5 = A(END OF SCREEN)                        
         BCTR  R5,0                                                             
         XR    R4,R4                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
*                                                                               
         LHI   R0,ESTDLQ           CLIENT DISPLAY LENGTH                        
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0                                                            
         STH   RF,FULL+2           FULL+2(2) = NUMBER THAT FIT ON LINE          
*                                                                               
DDE04    LH    R0,FULL+2           R0 = NUMBER OF COLUMNS                       
         XR    RF,RF               RF = DISPLACEMENT INTO LINE                  
*                                                                               
DDE06    LA    R2,SRVL1H           R2 = CURRENT LINE                            
         OI    FHAT,FHATHI                                                      
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
         MVC   0(30,RE),=CL30'M/Cli/Prd/Est  ACli/APrd/AEst'                    
         XC    LAST,LAST                                                        
         B     DDE10                                                            
*                                                                               
DDE08    CLC   DESTD(DESTKLQ),EFFS                                              
         BE    DDE12               LAST RECORD                                  
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   DEMED,VMED                                                       
         BNE   DDE12                                                            
         OC    VCLT,VCLT           CLIENT FILTER?                               
         BZ    *+14                NO                                           
         CLC   DEDCLT,VCLT                                                      
         BNE   DDE12                                                            
         OC    VPRD,VPRD           PRODUCT FILTER?                              
         BZ    *+14                NO                                           
         CLC   DEDPRD,VPRD                                                      
         BNE   DDE12                                                            
*                                                                               
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
*                                                                               
         CLC   DEMED,LAST+DEMED-DESTD                                           
         BE    *+14                SAME MEDIA AS BEFORE                         
         MVC   0(L'DEMED,RE),DEMED                                              
         MVI   L'DEMED(RE),C'/'                                                 
*                                                                               
         AHI   RE,L'DEMED+1                                                     
         CLC   DEDCLT,LAST+DEDCLT-DESTD                                         
         BE    *+14                SAME DDS CLIENT AS BEFORE                    
         MVC   0(L'DEDCLT,RE),DEDCLT                                            
         MVI   L'DEDCLT(RE),C'/'                                                
*                                                                               
         AHI   RE,L'DEDCLT+1                                                    
         CLC   DEDPRD,LAST+DEDPRD-DESTD                                         
         BE    *+14                SAME DDS PRODUCT AS BEFORE                   
         MVC   0(L'DEDPRD,RE),DEDPRD                                            
         MVI   L'DEDPRD(RE),C'/'                                                
*                                                                               
         AHI   RE,L'DEDPRD+1                                                    
         MVC   0(L'DEDEST,RE),DEDEST                                            
*                                                                               
         AHI   RE,L'DEDEST+2                                                    
         CLC   DECLT,LAST+DECLT-DESTD                                           
         BE    *+14                SAME ADWARE CLIENT AS BEFORE                 
         MVC   0(L'DECLT,RE),DECLT                                              
         MVI   L'DECLT(RE),C'/'                                                 
*                                                                               
         AHI   RE,L'DECLT+1                                                     
         CLC   DEPRD,LAST+DEPRD-DESTD                                           
         BE    *+14                SAME ADWARE PRODUCT AS BEFORE                
         MVC   0(L'DEPRD,RE),DEPRD                                              
         MVI   L'DEPRD(RE),C'/'                                                 
*                                                                               
         AHI   RE,L'DEPRD+1                                                     
         MVC   0(L'DEEST,RE),DEEST                                              
*                                                                               
         MVC   LAST(DESTRLQ),DESTD                                              
         AHI   R3,DESTRLQ          NEXT RECORD                                  
*                                                                               
DDE10    IC    R4,FHLN             NEXT ROW IN COLUMN                           
         BXLE  R2,R4,DDE08                                                      
         AH    RF,FULL             NEXT COLUMN                                  
         BCT   R0,DDE06                                                         
*                                                                               
DDE12    MVC   SVHMED,VMED         SET CURRENT FILTERS                          
         MVC   SVHDCLT,VCLT                                                     
         MVC   SVHDPRD,VPRD                                                     
         MVC   SVHDEST,VEST                                                     
         XC    SVHCLT,SVHCLT                                                    
         XC    SVHPRD,SVHPRD                                                    
         XC    SVHEST,SVHEST                                                    
*                                                                               
         CLC   DESTD(DESTKLQ),EFFS LAST RECORD IN LIST?                         
         BE    DDE14               YES                                          
*                                                                               
         OC    VMED,VMED           MEDIA FILTER?                                
         BZ    *+14                NO                                           
         CLC   DEMED,VMED                                                       
         BNE   DDE14                                                            
         OC    VCLT,VCLT           CLIENT FILTER?                               
         BZ    *+14                NO                                           
         CLC   DEDCLT,VCLT                                                      
         BNE   DDE14                                                            
         OC    VPRD,VPRD           PRODUCT FILTER?                              
         BZ    *+14                NO                                           
         CLC   DEDPRD,VPRD                                                      
         BNE   DDE14                                                            
*                                                                               
         MVI   HDRN,14                                                          
         MVC   SVHMED,DEMED                                                     
         MVC   SVHDCLT,DEDCLT                                                   
         MVC   SVHDPRD,DEDPRD                                                   
         MVC   SVHDEST,DEDEST                                                   
         MVC   SVHCLT,DECLT                                                     
         MVC   SVHPRD,DEPRD                                                     
         MVC   SVHEST,DEEST                                                     
*                                                                               
DDE14    BRAS  RE,OFF31                                                         
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY ERROR MESSAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISERR   NTR1  ,                                                                
         LA    R2,SRVMSGH                                                       
         USING FHD,R2                                                           
         MVC   SRVMSG,SPACES                                                    
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R4,FHDA             SET UP (SYS) AT START OF FIELD               
         MVI   0(R4),C'('                                                       
         MVC   1(4,R4),SYSNAME                                                  
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(2,R4),=C'E#'        SET ERROR                                  
         AHI   R4,2                                                             
*                                                                               
         XR    R0,R0               SET ERROR NUMBER                             
         IC    R0,FERN                                                          
         CHI   R0,255                                                           
         BNE   *+6                                                              
         XR    R0,R0                                                            
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                                                             
*                                                                               
         LTR   R0,R0               SPECIAL ERROR MESSAGE?                       
         BNE   DERR02              NO                                           
         XR    RF,RF                                                            
         ICM   RF,7,FERNA                                                       
         MVC   0(30,R4),0(RF)                                                   
         B     DERR04                                                           
*                                                                               
DERR02   AHI   R0,-1                                                            
         MHI   R0,EMSGL                                                         
         L     RF,AERRMSGS                                                      
         AR    RF,R0                                                            
         MVC   0(EMSGL,R4),0(RF)                                                
*                                                                               
DERR04   ICM   R2,15,FADRH         SET CURSOR ON BAD FIELD                      
         BZ    EXITOK                                                           
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY OK MESSAGE                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISOK    NTR1  ,                                                                
         LA    R2,SRVMSGH                                                       
         USING FHD,R2                                                           
         MVC   SRVMSG,SPACES                                                    
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R4,FHDA             SET UP (SYS) AT START OF FIELD               
         MVI   0(R4),C'('                                                       
         MVC   1(4,R4),SYSNAME                                                  
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(2,R4),=C'I#'      SET OK                                       
         AHI   R4,2                                                             
*                                                                               
         XR    R0,R0               SET ERROR NUMBER                             
         IC    R0,HDRN                                                          
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                                                             
*                                                                               
         AHI   R0,-1                                                            
         MHI   R0,IMSGL                                                         
         L     RF,AOKMSGS                                                       
         AR    RF,R0                                                            
         MVC   0(IMSGL,R4),0(RF)                                                
*                                                                               
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         ICM   RF,15,FADRH                                                      
         BNZ   *+8                                                              
         LA    RF,SRVL1H                                                        
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* READ TWA B AND EXTRACT SAVED VALUES                                 *         
***********************************************************************         
         SPACE 1                                                                
RDTWAB   NTR1  ,                                                                
         XC    PLIST(24),PLIST     READ TWA 11 FOR THIS TERMINAL                
         LHI   R0,SRPAGENO                                                      
         SLL   R0,32-8                                                          
         ICM   R0,3,TRMNUM                                                      
         MVC   PLIST+20(2),=C'L='                                               
         MVC   PLIST+22(2),RECLEN                                               
         GOTO1 ADMGR,PLIST,(0,DMREAD),TEMPSTR,(R0),ATIA                         
         CLI   8(R1),0                                                          
         BE    *+6                 SHOULDN'T BE ANY ERRORS                      
         DC    H'0'                                                             
*                                                                               
         L     RE,ATIA             COPY SAVED DATA FROM PAGE 11                 
         AHI   RE,SR$HELP-SRSD                                                  
         LA    R0,SVBLOCK                                                       
         LHI   R1,SVBLOCKL                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OI    FLAG,FLSCROLL       SET OK TO SCROLL                             
         CLC   SVID,=CL8'*A2DA2D*' DID I HAVE VALUES SAVED?                     
         BE    EXITOK              YES                                          
*                                                                               
         NI    FLAG,255-FLSCROLL                                                
         LA    R0,SVBLOCK                                                       
         LHI   R1,SVBLOCKL                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   SVID,=CL8'*A2DA2D*'                                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE TWA 11                                                        *         
***********************************************************************         
         SPACE 1                                                                
WTTWAB   NTR1  ,                                                                
         XC    PLIST(24),PLIST     READ TWA 11 FOR UPDATE                       
         LHI   R0,SRPAGENO                                                      
         SLL   R0,32-8                                                          
         ICM   R0,3,TRMNUM                                                      
         MVC   PLIST+20(2),=C'L='                                               
         MVC   PLIST+22(2),RECLEN                                               
         GOTO1 ADMGR,PLIST,(X'80',DMREAD),TEMPSTR,(R0),ATIA                     
         CLI   8(R1),0                                                          
         BE    *+6                 SHOULDN'T BE ANY ERRORS                      
         DC    H'0'                                                             
*                                                                               
         MVC   SVTYPE,TYPE                                                      
         MVC   SVMED,VMED          SAVE CURRENT FILTERS                         
         MVC   SVCLT,VCLT                                                       
         MVC   SVPRD,VPRD                                                       
         MVC   SVEST,VEST                                                       
*                                                                               
         L     RE,ATIA             MOVE BACK SAVED STORAGE                      
         AHI   RE,SR$DUMP-SRSD                                                  
         LA    R0,SVBLOCK                                                       
         LHI   RF,SVBLOCKL                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R0,SRPAGENO         WRITE BACK TWA 11                            
         SLL   R0,32-8                                                          
         ICM   R0,3,TRMNUM                                                      
         GOTO1 ADMGR,PLIST,(0,DMWRT),TEMPSTR,(R0),ATIA                          
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ON31     O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=X'7FFFFFFF'                                                  
         BSM   0,RE                                                             
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XMOD     CLI   FERN,0              OUTPUT MESSAGE                               
         BE    *+12                                                             
         BRAS  RE,DISERR                                                        
         B     *+8                                                              
         BRAS  RE,DISOK                                                         
         BRAS  RE,WTTWAB                                                        
         L     RD,SAVERD                                                        
         XMOD1 ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
CLIDLQ   EQU   15                  WIDTH OF CLIENT DISPLAY COLUMN               
PRDDLQ   EQU   24                  WIDTH OF PRODUCT DISPLAY COLUMN              
ESTDLQ   EQU   33                  WIDTH OF ESTIMATE DISPLAY COLUMN             
EMSGL    EQU   45                                                               
IMSGL    EQU   45                                                               
*                                                                               
FF       EQU   X'FF'                                                            
SPACES   DC    80C' '                                                           
EFFS     DC    32X'FF'                                                          
*                                                                               
RECLEN   DC    AL2(TWAMAX)                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT '                                                      
TEMPSTR  DC    CL8'TEMPSTR'                                                     
         EJECT                                                                  
***********************************************************************         
* TYPE TABLE - VALID RECORD TYPES SUPPORTED                           *         
***********************************************************************         
         SPACE 1                                                                
TYPTAB   DC    CL01'C',CL09'ACLIENT  ',CL09'AClient  '                          
         DC    CL50'Translate Adware Client - DDS Client             '          
         DC    CL01'D',CL09'DCLIENT  ',CL09'DClient  '                          
         DC    CL50'Translate DDS Client - Adware Client             '          
         DC    CL01'P',CL09'APRODUCT ',CL09'AProduct '                          
         DC    CL50'Translate Adware Product - DDS Product           '          
         DC    CL01'Q',CL09'DPRODUCT ',CL09'DProduct '                          
         DC    CL50'Translate DDS Product - Adware Product           '          
         DC    CL01'E',CL09'AESTIMATE',CL09'AEstimate'                          
         DC    CL50'Translate Adware Estimate - DDS Estimate         '          
         DC    CL01'F',CL09'DESTIMATE',CL09'Destimate'                          
         DC    CL50'Translate DDS Estimate - Adware Estimate         '          
         DC    AL1(255)                                                         
*                                                                               
TYPTABD  DSECT                     DSECT TO COVER TYPE TABLE                    
TYPTYPE  DS    CL1                                                              
TYPUCLC  DS    CL9                                                              
TYPLCLC  DS    CL9                                                              
TYPLHELP DS    CL50                                                             
TYPTABL  EQU   *-TYPTABD                                                        
*                                                                               
A2D      CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* OK MESSAGES                                                         *         
***********************************************************************         
         SPACE 1                                                                
OKMSGS   DS    0CL(IMSGL)                                                       
OK01     DC    CL(IMSGL)'Enter desired record type to translate'                
OK02     DC    CL(IMSGL)'Help displayed - enter desired record type'            
OK03     DC    CL(IMSGL)'Adware-DDS client conversion'                          
OK04     DC    CL(IMSGL)'DDS-Adware client conversion'                          
OK05     DC    CL(IMSGL)'Adware-DDS product conversion'                         
OK06     DC    CL(IMSGL)'DDS-Adware product conversion'                         
OK07     DC    CL(IMSGL)'Adware-DDS estimate conversion'                        
OK08     DC    CL(IMSGL)'DDS-Adware estimate conversion'                        
OK09     DC    CL(IMSGL)'Adware-DDS client conversion - more to come'           
OK10     DC    CL(IMSGL)'DDS-Adware client conversion - more to come'           
OK11     DC    CL(IMSGL)'Adware-DDS product conversion - more to come'          
OK12     DC    CL(IMSGL)'DDS-Adware product conversion - more to come'          
OK13     DC    CL(IMSGL)'Adware-DDS estimate conversion - more to come'         
OK14     DC    CL(IMSGL)'DDS-Adware estimate conversion - more to come'         
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
         SPACE 1                                                                
ERRMSGS  DS    0CL(EMSGL)                                                       
ERR01    DC    CL(EMSGL)'You are not authorised for this application'           
ERR02    DC    CL(EMSGL)'Invalid record type - ? for help or re-enter'          
ERR03    DC    CL(EMSGL)'Media code can only be 1 character, R or T'            
ERR04    DC    CL(EMSGL)'Valid Media codes are R (Radio) and T (TV)'            
ERR05    DC    CL(EMSGL)'Adware client is a maximum of 4 characters'            
ERR06    DC    CL(EMSGL)'DDS client is a maximum of 3 characters'               
ERR07    DC    CL(EMSGL)'Adware product is a maximum of 4 characters'           
ERR08    DC    CL(EMSGL)'DDS product is a maximum of 3 characters'              
ERR09    DC    CL(EMSGL)'Adware estimate is a maximum of 4 characters'          
ERR10    DC    CL(EMSGL)'DDS estimate is a maximum of 3 characters'             
ERR11    DC    CL(EMSGL)'This parameter is extraneous'                          
ERR12    DC    CL(EMSGL)'You cannot have a product with this record'            
ERR13    DC    CL(EMSGL)'You cannot have an estimate with this record'          
         EJECT                                                                  
***********************************************************************         
* TRANSLATION TABLE FOR ADWARE/DDS DETAILS                            *         
***********************************************************************         
         SPACE 1                                                                
         DC    C'                      '                                        
CPETAB   DC    C'RA4CVCFL 2001C4ACFA005'                                        
         DC    C'RA4CVCFL 2002C4ACFA001'                                        
         DC    C'RA4CVCFL 2003C4ACFA005'                                        
         DC    C'RA4CVCFL 2004C4ACFA001'                                        
         DC    C'RA4CVCFL 2005C4ACFA005'                                        
         DC    C'RA4CVCFL 2006C4ACFA001'                                        
         DC    C'RA4CVCFL 2007C4ACFA002'                                        
         DC    C'RDBUCCFL 0066U4 BAL013'                                        
         DC    C'RDBUCCFL 2488U4 BAL002'                                        
         DC    C'RDBUCCFL 2506U4 BAL011'                                        
         DC    C'RDBUCCFL 2507U4 BAL001'                                        
         DC    C'RDBUCCFL 2508U4 BUF011'                                        
         DC    C'RDBUCCFL 2509U4 BUF001'                                        
         DC    C'RDBUCCFL 2511U4 HAR001'                                        
         DC    C'RDBUCCFL 2512U4 ROC011'                                        
         DC    C'RDBUCCFL 2513U4 SYR001'                                        
         DC    C'RDBUCCFL 2514U1 PHO008'                                        
         DC    C'RDCADCFL 2077L4 BOS004'                                        
         DC    C'RDCADCFL 2286L2 AUS007'                                        
         DC    C'RDCADCFL 2287L2 AUS001'                                        
         DC    C'RDCADCFL 2294L2 DFW008'                                        
         DC    C'RDCADCFL 2295L2 DFW001'                                        
         DC    C'RDCADCFL 2302L2 HOU009'                                        
         DC    C'RDCADCFL 2303L2 HOU001'                                        
         DC    C'RDCADCFL 2307L2 JCK001'                                        
         DC    C'RDCADCFL 2314L2 NEW009'                                        
         DC    C'RDCADCFL 2315L2 NEW001'                                        
         DC    C'RDCADCFL 2318L2 SAN009'                                        
         DC    C'RDCADCFL 2319L2 SAN001'                                        
         DC    C'RDCADCFL 2392L3 FTM004'                                        
         DC    C'RDCADCFL 2394L1 LOS008'                                        
         DC    C'RDCADCFL 2395L1 LOS001'                                        
         DC    C'RDCADCFL 2404L5 COL001'                                        
         DC    C'RDCADCFL 2408L5 MIL001'                                        
         DC    C'RDCADCFL 2412L5 MIN001'                                        
         DC    C'RDCADCFL 2434L1 SSS008'                                        
         DC    C'RDCADCFL 2449L1 SDI008'                                        
         DC    C'RDCADCFL 2456L1 SFR008'                                        
         DC    C'RDCADCFL 2466L2 LUB001'                                        
         DC    C'RDCHECFL 0251V2 HMB010'                                        
         DC    C'RDCHECFL 0256V2 SAN010'                                        
         DC    C'RDCHECFL 0258V2 STL010'                                        
         DC    C'RDCHECFL 0259V5 CHI022'                                        
         DC    C'RDCHECFL 0310V2 AMA010'                                        
         DC    C'RDCHECFL 0319V1 LVG042'                                        
         DC    C'RDCHECFL 0323V2 LKC010'                                        
         DC    C'RDCHECFL 2090V4 PHI001'                                        
         DC    C'RDCHECFL 2353V5 CHI019'                                        
         DC    C'RDCHECFL 2354V5 CHI003'                                        
         DC    C'RDCHECFL 2358V4 ALB001'                                        
         DC    C'RDCHECFL 2361V4 BAL016'                                        
         DC    C'RDCHECFL 2362V4 BAL001'                                        
         DC    C'RDCHECFL 2370V4 BUF001'                                        
         DC    C'RDCHECFL 2390V4 PHI002'                                        
         DC    C'RDCHECFL 2394V4 PIT001'                                        
         DC    C'RDCHECFL 2406V4 ROC001'                                        
         DC    C'RDCHECFL 2414V4 SYR001'                                        
         DC    C'RDCHECFL 2421V2 AMA009'                                        
         DC    C'RDCHECFL 2422V2 AMA001'                                        
         DC    C'RDCHECFL 2425V2 JCK009'                                        
         DC    C'RDCHECFL 2426V2 JCK001'                                        
         DC    C'RDCHECFL 2433V2 LKC009'                                        
         DC    C'RDCHECFL 2434V2 LKC001'                                        
         DC    C'RDCHECFL 2437V2 MEM009'                                        
         DC    C'RDCHECFL 2441V2 NEW009'                                        
         DC    C'RDCHECFL 2442V2 NEW001'                                        
         DC    C'RDCHECFL 2445V2 SAN009'                                        
         DC    C'RDCHECFL 2458V2 TUL001'                                        
         DC    C'RDCHECFL 2469V3 CHR008'                                        
         DC    C'RDCHECFL 2470V3 CHR001'                                        
         DC    C'RDCHECFL 2478V3 CHN001'                                        
         DC    C'RDCHECFL 2482V3 CLM001'                                        
         DC    C'RDCHECFL 2486V3 FTM001'                                        
         DC    C'RDCHECFL 2490V3 GHP001'                                        
         DC    C'RDCHECFL 2497V3 HUN008'                                        
         DC    C'RDCHECFL 2498V3 HUN001'                                        
         DC    C'RDCHECFL 2505V3 KNO008'                                        
         DC    C'RDCHECFL 2506V3 KNO001'                                        
         DC    C'RDCHECFL 2522V3 ORL001'                                        
         DC    C'RDCHECFL 2526V3 PAN001'                                        
         DC    C'RDCHECFL 2538V3 ROL001'                                        
         DC    C'RDCHECFL 2550V3 WPB001'                                        
         DC    C'RDCHECFL 2557V1 PHO037'                                        
         DC    C'RDCHECFL 2558V1 PHO007'                                        
         DC    C'RDCHECFL 2693V5 CLE002'                                        
         DC    C'RDCHECFL 2701V5 CHI002'                                        
         DC    C'RDCHECFL 2709V5 FTW019'                                        
         DC    C'RDCHECFL 2710V5 FTW001'                                        
         DC    C'RDCHECFL 2713V5 GBA019'                                        
         DC    C'RDCHECFL 2714V5 GBA001'                                        
         DC    C'RDCHECFL 2729V5 MAD019'                                        
         DC    C'RDCHECFL 2737V5 MIN019'                                        
         DC    C'RDCHECFL 2738V5 MIN001'                                        
         DC    C'RDCHECFL 2742V3 AUG001'                                        
         DC    C'RDCHECFL 2749V5 WRH019'                                        
         DC    C'RDCHECFL 2781V3 FLM008'                                        
         DC    C'RDCHECFL 2782V3 FLM001'                                        
         DC    C'RDCHECFL 2794V3 LEX001'                                        
         DC    C'RDCHECFL 2797V3 LOU008'                                        
         DC    C'RDCHECFL 2798V3 LOU001'                                        
         DC    C'RDCHECFL 2801V3 MFL008'                                        
         DC    C'RDCHECFL 2802V3 MFL001'                                        
         DC    C'RDCHECFL 2813V3 SAV008'                                        
         DC    C'RDCHECFL 2814V3 SAV001'                                        
         DC    C'RDCHECFL 2868V5 DEM019'                                        
         DC    C'RDCHECFL 2869V5 DEM001'                                        
         DC    C'RDCHECFL 2937V5 CSD001'                                        
         DC    C'RDCHECFL 2940V5 PBM019'                                        
         DC    C'RDCHECFL 2941V5 PBM001'                                        
         DC    C'RDCHECFL 2947V1 PHO002'                                        
         DC    C'RDCHECFL 2948V1 PHO003'                                        
         DC    C'RDCHECFL 2953V1 LVG037'                                        
         DC    C'RDCHECFL 2964V4 BIN001'                                        
         DC    C'RDCHECFL 2966V1 SDI037'                                        
         DC    C'RDCHECFL 2992V5 CHI020'                                        
         DC    C'RDCHECFL 2993V2 HMB009'                                        
         DC    C'RDCHECFL 2994V2 HMB001'                                        
         DC    C'RDCHECFL 3008V1 CRE037'                                        
         DC    C'RDCHECFL 3009V1 CRE038'                                        
         DC    C'RDCHECFL 3010V4 WAS016'                                        
         DC    C'RDCHECFL 3015V1 COS002'                                        
         DC    C'RDCHECFL 3017V3 TSP008'                                        
         DC    C'RDCHECFL 3018V1 FRV037'                                        
         DC    C'RDCHECFL 3019V1 FRV001'                                        
         DC    C'RDCHECFL 3020V1 FRV038'                                        
         DC    C'RDCHECFL 3021V1 FRV002'                                        
         DC    C'RDCHECFL 3022V1 FRV039'                                        
         DC    C'RDCHECFL 3023V1 FRV003'                                        
         DC    C'RDCHECFL 3025V5 LAF001'                                        
         DC    C'RDCHETFL 0213V1 SEA041'                                        
         DC    C'RDCHETFL 2854V1 SEA004'                                        
         DC    C'RDCHETFL 2956V2 NEW016'                                        
         DC    C'RDCHETFL 2957V2 NEW002'                                        
         DC    C'RDPONCFL 2328N2 WTB004'                                        
         DC    C'RDPONCFL 2329N2 WTB001'                                        
         DC    C'RDTGMTFL 2253M1 PHO004'                                        
         DC    C'RDTGMTFL 2309M4 BPG007'                                        
         DC    C'RDTGMTFL 2325M2 SAN007'                                        
         DC    C'RDTGMTFL 2339M4 BPG001'                                        
         DC    C'RDTGMTFL 2342M2 SPR008'                                        
         DC    C'RDTGMTFL 2343M2 SPR001'                                        
         DC    C'RDTGMTFL 2347M1 PHO001'                                        
         DC    C'RDTGMTFL 2357M2 SAN008'                                        
         DC    C'RDTGMTFL 2358M2 STL009'                                        
         DC    C'RDTGMTFL 2359M1 COS004'                                        
         DC    C'RDTGMTFL 2360M2 MEM010'                                        
         DC    C'RGACVPROM0014AC PRO004'                                        
         DC    C'RGACVPROM0015AC PRO001'                                        
         DC    C'RGACVPROM0016AC PRO002'                                        
         DC    C'RGC  LDS 0097GC LDS001'                                        
         DC    C'RGCRGLDS 0097GC LDS001'                                        
         DC    C'RGDIRSRV 0011DT SRV005'                                        
         DC    C'RGDIRSRV 0012DT SRV001'                                        
         DC    C'RGDITGEN 0001DI GEN004'                                        
         DC    C'RGDITGEN 0002DI GEN001'                                        
         DC    C'RGISUGEN 0002IZ GEN001'                                        
         DC    C'RGISUGEN 0002IZ GEN007'                                        
         DC    C'RGMAMTFL 2006GT GB 001'                                        
         DC    C'RGMFLBRN 0023CO FM 001'                                        
         DC    C'RGOLDCFL 2001OL LF 003'                                        
         DC    C'RGOLDCFL 2002OL LF 001'                                        
         DC    C'RHACDBRD 0007ACHPRO001'                                        
         DC    C'RH2CVTFL 2007C2HTFH001'                                        
         DC    C'RH3CVCFL 2003C3HCFH006'                                        
         DC    C'RH3CVCFL 2004C3HCFH001'                                        
         DC    C'RH3POPFL 2005P3HPD 004'                                        
         DC    C'RR1BUCFLR2078B1 BB 007'                                        
         DC    C'RR1BUCFLR2079B1 BB 001'                                        
         DC    C'RR1BUCFLR2079B1 BB 004'                                        
         DC    C'RR1CACFLR2083D1 MB 005'                                        
         DC    C'RR1CACFLR2087D1 MB 006'                                        
         DC    C'RR1CACFLR2088D1 MB 001'                                        
         DC    C'RR1CVTFL 2191C1 TF 004'                                        
         DC    C'RR1GCTFL 0061G1 GF 004'                                        
         DC    C'RR2BUCFLR2125B2 BB 004'                                        
         DC    C'RR2BUCFLR2126B2 BB 001'                                        
         DC    C'RR2BUCFLR2159B2 BB 001'                                        
         DC    C'RR2BUCFLR2160B2 BB 001'                                        
         DC    C'RR2CACFLR2050D2 MB 001'                                        
         DC    C'RR2CACFLR2058D2 MB 001'                                        
         DC    C'RR2CACFLR2066D2 MB 001'                                        
         DC    C'RR2CACFLR2074D2 MB 001'                                        
         DC    C'RR2CACFLR2082D2 MB 001'                                        
         DC    C'RR2CACFLR2088D2 MB 001'                                        
         DC    C'RR2CVCAV 2046C2 CA 006'                                        
         DC    C'RR2CVCFL 0125C2 CF 007'                                        
         DC    C'RR2CVCFL 2037C2 CF 010'                                        
         DC    C'RR2CVCFL 2052C2 CF 023'                                        
         DC    C'RR2CVIMP 2049C2 CI 006'                                        
         DC    C'RR2CVSIL 2034C2 TS 005'                                        
         DC    C'RR2CVSUB 2042C2 TU 006'                                        
         DC    C'RR2CVTAH 2039C2 TH 022'                                        
         DC    C'RR2CVTAH 2040C2 TH 006'                                        
         DC    C'RR2CVTFL 0117C2 TF 020'                                        
         DC    C'RR2CVTFL 0122C2 TF 021'                                        
         DC    C'RR2CVTFL 0124C2 TF 001'                                        
         DC    C'RR2CVTFL 2036C2 TF 002'                                        
         DC    C'RR2CVTFL 2044C2 TF 012'                                        
         DC    C'RR2CVTFL 2045C2 TF 011'                                        
         DC    C'RR2CVTFL 2066C2 TF 013'                                        
         DC    C'RR2CVTFL 2066C2 TF 022'                                        
         DC    C'RR2POPFLR2025P2 PD 012'                                        
         DC    C'RR2POPFLR2028P2 PD 008'                                        
         DC    C'RR3BUCFLR2042B3 BB 001'                                        
         DC    C'RR3BUCFLR2044B3 BB 001'                                        
         DC    C'RR3BUCFLR2046B3 BB 001'                                        
         DC    C'RR3BUCFLR2048B3 BB 001'                                        
         DC    C'RR3BUCFLR2050B3 BB 001'                                        
         DC    C'RR3BUCFLR2052B3 BB 001'                                        
         DC    C'RR3BUCFLR2054B3 BB 001'                                        
         DC    C'RR3BUCFLR2056B3 BB 001'                                        
         DC    C'RR3BUCFLR2058B3 BB 001'                                        
         DC    C'RR3BUCFLR2060B3 BB 001'                                        
         DC    C'RR3POPFLR2055P3 PD 001'                                        
         DC    C'RR4CACFLR2270D4 MB 005'                                        
         DC    C'RR4GCTFL 2040G4 GF 004'                                        
         DC    C'RR4GCTFL 2041G4 GF 001'                                        
         DC    C'RR4POPFLR0047P4 PD 003'                                        
         DC    C'RR5BUCFLR2052B5 BB 007'                                        
         DC    C'RR5BUCFLR2058B5 BB 008'                                        
         DC    C'RR5BUCFLR2059B5 BB 009'                                        
         DC    C'RR5CACFL 0051D5 MB 007'                                        
         DC    C'RR5CACFL 0053D5 MB 001'                                        
         DC    C'RR5CVCFL 2037C5 CF 007'                                        
         DC    C'RR5CVCFL 2038C5 CF 008'                                        
         DC    C'RR5CVCFL 2045C5 CF 008'                                        
         DC    C'RR5CVCFL 2057C5 CF 008'                                        
         DC    C'RR5CVCFL 2068C5 CF 009'                                        
         DC    C'RR5CVCFL 2070C5 CF 010'                                        
         DC    C'RR5CVTFL 0131C5 TF 005'                                        
         DC    C'RR5CVTFL 0136C5 TF 006'                                        
         DC    C'RR5GCTFL 2029G5 GF 002'                                        
         DC    C'RR5POPFLR2032P5 PR 003'                                        
         DC    C'RSCHVCFL 2069V5HCHI005'                                        
         DC    C'RSCHVCFL 2070V5HCHI001'                                        
         DC    C'RSCHVCFL 2100V3HMFL005'                                        
         DC    C'RSCHVCFL 4069V5HCHI004'                                        
         DC    C'TACHVTBL 2001CHACFA001'                                        
         DC    C'TA2CVCFL 2001C2ACFA001'                                        
         DC    C'TA2POPFL 9012P2 PD 022'                                        
         DC    C'TA2POPFL 9012P2APD 022'                                        
         DC    C'TA4POPFL 2002P4APD 001'                                        
         DC    C'TDBUCBON 7201U2 BON022'                                        
         DC    C'TDBUCCFL 0064U1 PHO010'                                        
         DC    C'TDBUCCFL 0068U1 TUC010'                                        
         DC    C'TDBUCCFL 0100U1 COS010'                                        
         DC    C'TDBUCCFL 0104U3 MOB008'                                        
         DC    C'TDBUCCFL 0107U3 CHR008'                                        
         DC    C'TDBUCCFL 0108U3 SAV008'                                        
         DC    C'TDBUCCFL 0114U2 OKC009'                                        
         DC    C'TDBUCCFL 0118U1 EUG010'                                        
         DC    C'TDBUCCFL 0124U1 SSS010'                                        
         DC    C'TDBUCCFL 2001U5 CHI011'                                        
         DC    C'TDBUCCFL 2002U5 CHI001'                                        
         DC    C'TDBUCCFL 2005U5 COL011'                                        
         DC    C'TDBUCCFL 2006U5 COL001'                                        
         DC    C'TDBUCCFL 2009U5 FTW012'                                        
         DC    C'TDBUCCFL 2010U5 FTW001'                                        
         DC    C'TDBUCCFL 2013U5 GBA011'                                        
         DC    C'TDBUCCFL 2014U5 GBA002'                                        
         DC    C'TDBUCCFL 2017U5 IND012'                                        
         DC    C'TDBUCCFL 2018U5 IND002'                                        
         DC    C'TDBUCCFL 2021U5 LCE012'                                        
         DC    C'TDBUCCFL 2022U5 LCE002'                                        
         DC    C'TDBUCCFL 2025U5 LAN012'                                        
         DC    C'TDBUCCFL 2026U5 LAN001'                                        
         DC    C'TDBUCCFL 2029U5 MAD011'                                        
         DC    C'TDBUCCFL 2030U5 MAD001'                                        
         DC    C'TDBUCCFL 2033U5 MIL012'                                        
         DC    C'TDBUCCFL 2034U5 MIL001'                                        
         DC    C'TDBUCCFL 2037U5 RKF012'                                        
         DC    C'TDBUCCFL 2038U5 RKF001'                                        
         DC    C'TDBUCCFL 2041U5 WRH012'                                        
         DC    C'TDBUCCFL 2042U5 WRH001'                                        
         DC    C'TDBUCCFL 2045U4 BUF006'                                        
         DC    C'TDBUCCFL 2046U4 BUF001'                                        
         DC    C'TDBUCCFL 2047U4 BUF002'                                        
         DC    C'TDBUCCFL 2049U4 PHI006'                                        
         DC    C'TDBUCCFL 2050U4 PHI001'                                        
         DC    C'TDBUCCFL 2051U4 PIT001'                                        
         DC    C'TDBUCCFL 2053U4 PIT006'                                        
         DC    C'TDBUCCFL 2054U4 PIT002'                                        
         DC    C'TDBUCCFL 2057U4 ROC006'                                        
         DC    C'TDBUCCFL 2058U4 ROC001'                                        
         DC    C'TDBUCCFL 2061U4 SYR006'                                        
         DC    C'TDBUCCFL 2062U4 SYR001'                                        
         DC    C'TDBUCCFL 2065U2 AUS010'                                        
         DC    C'TDBUCCFL 2066U2 AUS001'                                        
         DC    C'TDBUCCFL 2069U2 BTR010'                                        
         DC    C'TDBUCCFL 2070U2 BTR001'                                        
         DC    C'TDBUCCFL 2073U2 DFW010'                                        
         DC    C'TDBUCCFL 2074U2 DFW001'                                        
         DC    C'TDBUCCFL 2077U2 HOU010'                                        
         DC    C'TDBUCCFL 2078U2 HOU001'                                        
         DC    C'TDBUCCFL 2081U2 JCK010'                                        
         DC    C'TDBUCCFL 2082U2 JCK001'                                        
         DC    C'TDBUCCFL 2085U2 MEM010'                                        
         DC    C'TDBUCCFL 2086U2 MEM001'                                        
         DC    C'TDBUCCFL 2089U2 NEW010'                                        
         DC    C'TDBUCCFL 2090U2 NEW001'                                        
         DC    C'TDBUCCFL 2093U2 SAN010'                                        
         DC    C'TDBUCCFL 2094U2 SAN001'                                        
         DC    C'TDBUCCFL 2097U2 STL010'                                        
         DC    C'TDBUCCFL 2098U2 STL001'                                        
         DC    C'TDBUCCFL 2101U3 ALY009'                                        
         DC    C'TDBUCCFL 2102U3 ALY001'                                        
         DC    C'TDBUCCFL 2105U3 BIR009'                                        
         DC    C'TDBUCCFL 2106U3 BIR001'                                        
         DC    C'TDBUCCFL 2109U3 CHR009'                                        
         DC    C'TDBUCCFL 2110U3 CHR001'                                        
         DC    C'TDBUCCFL 2113U3 CHN010'                                        
         DC    C'TDBUCCFL 2114U3 CHN001'                                        
         DC    C'TDBUCCFL 2117U3 CLM009'                                        
         DC    C'TDBUCCFL 2118U3 CLM001'                                        
         DC    C'TDBUCCFL 2121U3 DOT009'                                        
         DC    C'TDBUCCFL 2122U3 DOT001'                                        
         DC    C'TDBUCCFL 2125U3 EVA009'                                        
         DC    C'TDBUCCFL 2126U3 EVA001'                                        
         DC    C'TDBUCCFL 2129U3 FTM009'                                        
         DC    C'TDBUCCFL 2130U3 FTM001'                                        
         DC    C'TDBUCCFL 2133U3 JAC009'                                        
         DC    C'TDBUCCFL 2134U3 JAC001'                                        
         DC    C'TDBUCCFL 2137U3 KNO009'                                        
         DC    C'TDBUCCFL 2138U3 KNO001'                                        
         DC    C'TDBUCCFL 2141U3 LOU009'                                        
         DC    C'TDBUCCFL 2142U3 LOU001'                                        
         DC    C'TDBUCCFL 2145U3 MAC009'                                        
         DC    C'TDBUCCFL 2146U3 MAC001'                                        
         DC    C'TDBUCCFL 2149U3 MOB011'                                        
         DC    C'TDBUCCFL 2150U3 MOB001'                                        
         DC    C'TDBUCCFL 2153U3 MON009'                                        
         DC    C'TDBUCCFL 2154U3 MON001'                                        
         DC    C'TDBUCCFL 2157U3 NRF009'                                        
         DC    C'TDBUCCFL 2158U3 NRF001'                                        
         DC    C'TDBUCCFL 2161U3 RDU010'                                        
         DC    C'TDBUCCFL 2162U3 RDU001'                                        
         DC    C'TDBUCCFL 2165U3 RIC009'                                        
         DC    C'TDBUCCFL 2166U3 RIC001'                                        
         DC    C'TDBUCCFL 2169U3 SAV003'                                        
         DC    C'TDBUCCFL 2170U3 SAV002'                                        
         DC    C'TDBUCCFL 2173U3 WPB010'                                        
         DC    C'TDBUCCFL 2174U3 WPB001'                                        
         DC    C'TDBUCCFL 2177U1 BAK011'                                        
         DC    C'TDBUCCFL 2178U1 BAK001'                                        
         DC    C'TDBUCCFL 2181U1 CRE011'                                        
         DC    C'TDBUCCFL 2182U1 CRE001'                                        
         DC    C'TDBUCCFL 2185U1 COS011'                                        
         DC    C'TDBUCCFL 2186U1 COS001'                                        
         DC    C'TDBUCCFL 2189U1 DEN011'                                        
         DC    C'TDBUCCFL 2190U1 DEN001'                                        
         DC    C'TDBUCCFL 2193U1 FRV012'                                        
         DC    C'TDBUCCFL 2194U1 FRV001'                                        
         DC    C'TDBUCCFL 2196U1 LOS011'                                        
         DC    C'TDBUCCFL 2197U1 LOS001'                                        
         DC    C'TDBUCCFL 2200U1 MTS011'                                        
         DC    C'TDBUCCFL 2201U1 MTS001'                                        
         DC    C'TDBUCCFL 2204U1 PLS012'                                        
         DC    C'TDBUCCFL 2205U1 PLS001'                                        
         DC    C'TDBUCCFL 2208U1 PHO011'                                        
         DC    C'TDBUCCFL 2209U1 PHO001'                                        
         DC    C'TDBUCCFL 2212U1 POR011'                                        
         DC    C'TDBUCCFL 2213U1 POR001'                                        
         DC    C'TDBUCCFL 2216U1 SAC011'                                        
         DC    C'TDBUCCFL 2217U1 SAC001'                                        
         DC    C'TDBUCCFL 2220U1 SDI011'                                        
         DC    C'TDBUCCFL 2221U1 SDI001'                                        
         DC    C'TDBUCCFL 2224U1 SFR011'                                        
         DC    C'TDBUCCFL 2225U1 SFR001'                                        
         DC    C'TDBUCCFL 2228U1 SEA011'                                        
         DC    C'TDBUCCFL 2229U1 SEA001'                                        
         DC    C'TDBUCCFL 2232U1 SSS011'                                        
         DC    C'TDBUCCFL 2233U1 SSS001'                                        
         DC    C'TDBUCCFL 2236U1 TUC011'                                        
         DC    C'TDBUCCFL 2237U1 TUC001'                                        
         DC    C'TDBUCCFL 2240U4 BAL006'                                        
         DC    C'TDBUCCFL 2241U4 BAL001'                                        
         DC    C'TDBUCCFL 2244U4 HAR007'                                        
         DC    C'TDBUCCFL 2245U4 HAR001'                                        
         DC    C'TDBUCCFL 2248U4 WAS007'                                        
         DC    C'TDBUCCFL 2249U4 WAS001'                                        
         DC    C'TDBUCCFL 2252U2 HOU011'                                        
         DC    C'TDBUCCFL 2253U2 HOU002'                                        
         DC    C'TDBUCCFL 2256U2 DFW011'                                        
         DC    C'TDBUCCFL 2257U2 DFW002'                                        
         DC    C'TDBUCCFL 2260U2 AUS011'                                        
         DC    C'TDBUCCFL 2261U2 AUS002'                                        
         DC    C'TDBUCCFL 2264U2 HMB011'                                        
         DC    C'TDBUCCFL 2265U2 HMB001'                                        
         DC    C'TDBUCCFL 2268U2 SAN011'                                        
         DC    C'TDBUCCFL 2269U2 SAN002'                                        
         DC    C'TDBUCCFL 2272U2 BTR011'                                        
         DC    C'TDBUCCFL 2273U2 BTR002'                                        
         DC    C'TDBUCCFL 2276U2 AMA012'                                        
         DC    C'TDBUCCFL 2277U2 AMA001'                                        
         DC    C'TDBUCCFL 2280U2 OKC011'                                        
         DC    C'TDBUCCFL 2281U2 OKC001'                                        
         DC    C'TDBUCCFL 2284U2 MEM011'                                        
         DC    C'TDBUCCFL 2285U2 MEM002'                                        
         DC    C'TDBUCCFL 2288U2 JCK011'                                        
         DC    C'TDBUCCFL 2289U2 JCK002'                                        
         DC    C'TDBUCCFL 2292U2 STL013'                                        
         DC    C'TDBUCCFL 2293U2 STL002'                                        
         DC    C'TDBUCCFL 2296U2 NEW011'                                        
         DC    C'TDBUCCFL 2297U2 NEW002'                                        
         DC    C'TDBUCCFL 2341U5 CHI002'                                        
         DC    C'TDBUCCFL 2384U2 HMB010'                                        
         DC    C'TDBUCCFL 2385U2 HMB002'                                        
         DC    C'TDBUCCFL 2388U2 OKC010'                                        
         DC    C'TDBUCCFL 2389U2 OKC002'                                        
         DC    C'TDBUCCFL 2392U5 MIN012'                                        
         DC    C'TDBUCCFL 2393U5 MIN001'                                        
         DC    C'TDBUCCFL 2400U1 LOS013'                                        
         DC    C'TDBUCCFL 2402U4 BAL008'                                        
         DC    C'TDBUCCFL 2403U4 BAL002'                                        
         DC    C'TDBUCCFL 2406U4 BUF008'                                        
         DC    C'TDBUCCFL 2407U4 BUF003'                                        
         DC    C'TDBUCCFL 2410U4 HAR008'                                        
         DC    C'TDBUCCFL 2411U4 HAR002'                                        
         DC    C'TDBUCCFL 2414U4 PHI008'                                        
         DC    C'TDBUCCFL 2415U4 PHI002'                                        
         DC    C'TDBUCCFL 2418U4 PIT008'                                        
         DC    C'TDBUCCFL 2419U4 PIT003'                                        
         DC    C'TDBUCCFL 2422U4 ROC008'                                        
         DC    C'TDBUCCFL 2423U4 ROC002'                                        
         DC    C'TDBUCCFL 2426U4 SYR008'                                        
         DC    C'TDBUCCFL 2427U4 SYR002'                                        
         DC    C'TDBUCCFL 2430U4 WAS008'                                        
         DC    C'TDBUCCFL 2431U4 WAS002'                                        
         DC    C'TDBUCCFL 2435U5 CHI003'                                        
         DC    C'TDBUCCFL 2443U5 FTW002'                                        
         DC    C'TDBUCCFL 2446U5 GBA013'                                        
         DC    C'TDBUCCFL 2447U5 GBA001'                                        
         DC    C'TDBUCCFL 2454U5 LCE014'                                        
         DC    C'TDBUCCFL 2455U5 LCE001'                                        
         DC    C'TDBUCCFL 2459U5 LAN002'                                        
         DC    C'TDBUCCFL 2463U5 MAD002'                                        
         DC    C'TDBUCCFL 2474U5 RKF014'                                        
         DC    C'TDBUCCFL 2475U5 RKF002'                                        
         DC    C'TDBUCCFL 2478U5 WRH014'                                        
         DC    C'TDBUCCFL 2479U5 WRH002'                                        
         DC    C'TDBUCCFL 2482U5 CHI005'                                        
         DC    C'TDBUCCFL 2483U1 MTS014'                                        
         DC    C'TDBUCCFL 2484U1 EUG011'                                        
         DC    C'TDBUCCFL 2485U1 EUG001'                                        
         DC    C'TDBUCCFL 2489U3 PAN012'                                        
         DC    C'TDBUCCFL 2490U3 PAN001'                                        
         DC    C'TDBUCCFL 2491U3 TAL012'                                        
         DC    C'TDBUCCFL 2492U3 TAL001'                                        
         DC    C'TDBUCCFL 2493U5 IND004'                                        
         DC    C'TDBUCCFL 2493U5 IND016'                                        
         DC    C'TDBUCCFL 2494U3 FLM012'                                        
         DC    C'TDBUCCFL 2495U3 FLM001'                                        
         DC    C'TDBUCCFL 2498U3 GHP011'                                        
         DC    C'TDBUCCFL 2499U3 GHP001'                                        
         DC    C'TDBUCCFL 2502U2 AMA014'                                        
         DC    C'TDBUCCFL 2503U2 AMA002'                                        
         DC    C'TDBUCCFL 2537U5 FTW013'                                        
         DC    C'TDBUCCFL 2538U5 LAN013'                                        
         DC    C'TDBUCCFL 2543U5 CLE013'                                        
         DC    C'TDBUCCFL 2544U5 CLE001'                                        
         DC    C'TDBUCCFL 2547U5 CLE011'                                        
         DC    C'TDBUCCFL 2548U5 CLE002'                                        
         DC    C'TDBUCCFL 2583U5 CHI015'                                        
         DC    C'TDBUCCFL 2590U4 BAL007'                                        
         DC    C'TDBUCCFL 2591U1 FRV002'                                        
         DC    C'TDBUCCFL 2591U1 FRV015'                                        
         DC    C'TDBUCCFL 2594U5 CIN001'                                        
         DC    C'TDBUCCFL 2595U5 CIN002'                                        
         DC    C'TDBUCCFL 2596U2 ELP001'                                        
         DC    C'TDBUCCFL 2597U5 IND013'                                        
         DC    C'TDBUCCFL 2598U2 ELP002'                                        
         DC    C'TDBUCCFL 2599U5 MIN002'                                        
         DC    C'TDBUCCFL 2600U2 KAN001'                                        
         DC    C'TDBUCCFL 2601U2 OKC007'                                        
         DC    C'TDCADBON 7201L2 BON025'                                        
         DC    C'TDCADBON 7201L4 BON025'                                        
         DC    C'TDCADCFL 0097L4 BUF005'                                        
         DC    C'TDCADCFL 0118L3 LEX004'                                        
         DC    C'TDCADCFL 0142L2 HMB005'                                        
         DC    C'TDCADCFL 2001L5 CHI009'                                        
         DC    C'TDCADCFL 2002L5 CHI001'                                        
         DC    C'TDCADCFL 2005L5 CLE009'                                        
         DC    C'TDCADCFL 2006L5 CLE001'                                        
         DC    C'TDCADCFL 2009L5 COL009'                                        
         DC    C'TDCADCFL 2010L5 COL001'                                        
         DC    C'TDCADCFL 2014L5 FTW001'                                        
         DC    C'TDCADCFL 2017L5 IND009'                                        
         DC    C'TDCADCFL 2018L5 IND001'                                        
         DC    C'TDCADCFL 2021L5 MIL009'                                        
         DC    C'TDCADCFL 2022L5 MIL001'                                        
         DC    C'TDCADCFL 2026L5 MIN001'                                        
         DC    C'TDCADCFL 2030L5 RKF001'                                        
         DC    C'TDCADCFL 2033L4 BAL006'                                        
         DC    C'TDCADCFL 2034L4 BAL001'                                        
         DC    C'TDCADCFL 2037L4 BUF006'                                        
         DC    C'TDCADCFL 2038L4 BUF001'                                        
         DC    C'TDCADCFL 2039L5 CIN001'                                        
         DC    C'TDCADCFL 2045L4 NYK006'                                        
         DC    C'TDCADCFL 2046L4 NYK001'                                        
         DC    C'TDCADCFL 2049L4 PHI006'                                        
         DC    C'TDCADCFL 2050L4 PHI001'                                        
         DC    C'TDCADCFL 2053L4 PIT006'                                        
         DC    C'TDCADCFL 2054L4 PIT001'                                        
         DC    C'TDCADCFL 2057L4 ROC006'                                        
         DC    C'TDCADCFL 2058L4 ROC001'                                        
         DC    C'TDCADCFL 2061L4 SYR006'                                        
         DC    C'TDCADCFL 2062L4 SYR001'                                        
         DC    C'TDCADCFL 2065L4 WAS006'                                        
         DC    C'TDCADCFL 2066L4 WAS001'                                        
         DC    C'TDCADCFL 2069L4 WBS006'                                        
         DC    C'TDCADCFL 2070L4 WBS001'                                        
         DC    C'TDCADCFL 2073L2 AUS006'                                        
         DC    C'TDCADCFL 2074L1 SDI006'                                        
         DC    C'TDCADCFL 2075L1 SDI001'                                        
         DC    C'TDCADCFL 2077L2 BTR006'                                        
         DC    C'TDCADCFL 2078L2 BTR001'                                        
         DC    C'TDCADCFL 2081L2 DFW006'                                        
         DC    C'TDCADCFL 2082L2 DFW001'                                        
         DC    C'TDCADCFL 2085L2 HMB006'                                        
         DC    C'TDCADCFL 2086L2 HMB001'                                        
         DC    C'TDCADCFL 2089L2 HOU006'                                        
         DC    C'TDCADCFL 2090L2 HOU002'                                        
         DC    C'TDCADCFL 2093L2 JCK006'                                        
         DC    C'TDCADCFL 2094L2 JCK001'                                        
         DC    C'TDCADCFL 2097L2 KAN006'                                        
         DC    C'TDCADCFL 2098L2 KAN001'                                        
         DC    C'TDCADCFL 2101L2 LUB006'                                        
         DC    C'TDCADCFL 2102L2 LUB001'                                        
         DC    C'TDCADCFL 2105L2 MEM006'                                        
         DC    C'TDCADCFL 2106L2 MEM001'                                        
         DC    C'TDCADCFL 2109L2 NEW006'                                        
         DC    C'TDCADCFL 2110L2 NEW002'                                        
         DC    C'TDCADCFL 2113L2 SAN006'                                        
         DC    C'TDCADCFL 2114L2 SAN001'                                        
         DC    C'TDCADCFL 2117L2 SPR006'                                        
         DC    C'TDCADCFL 2118L2 SPR001'                                        
         DC    C'TDCADCFL 2121L3 ATL005'                                        
         DC    C'TDCADCFL 2122L3 ATL001'                                        
         DC    C'TDCADCFL 2125L3 BIR005'                                        
         DC    C'TDCADCFL 2126L3 BIR001'                                        
         DC    C'TDCADCFL 2129L3 CHA005'                                        
         DC    C'TDCADCFL 2130L3 CHA001'                                        
         DC    C'TDCADCFL 2133L3 FTM005'                                        
         DC    C'TDCADCFL 2134L3 FTM001'                                        
         DC    C'TDCADCFL 2137L3 GHP005'                                        
         DC    C'TDCADCFL 2138L3 GHP001'                                        
         DC    C'TDCADCFL 2141L3 GSP005'                                        
         DC    C'TDCADCFL 2142L3 GSP001'                                        
         DC    C'TDCADCFL 2145L3 JAC005'                                        
         DC    C'TDCADCFL 2146L3 JAC001'                                        
         DC    C'TDCADCFL 2149L3 LOU006'                                        
         DC    C'TDCADCFL 2150L3 LOU001'                                        
         DC    C'TDCADCFL 2153L3 MFL005'                                        
         DC    C'TDCADCFL 2154L3 MFL001'                                        
         DC    C'TDCADCFL 2157L3 NAS005'                                        
         DC    C'TDCADCFL 2158L3 NAS001'                                        
         DC    C'TDCADCFL 2161L3 NRF005'                                        
         DC    C'TDCADCFL 2162L3 NRF001'                                        
         DC    C'TDCADCFL 2165L3 ORL005'                                        
         DC    C'TDCADCFL 2166L3 ORL001'                                        
         DC    C'TDCADCFL 2169L3 RDU005'                                        
         DC    C'TDCADCFL 2170L3 RDU001'                                        
         DC    C'TDCADCFL 2173L3 RIC006'                                        
         DC    C'TDCADCFL 2174L3 RIC001'                                        
         DC    C'TDCADCFL 2177L3 TAL006'                                        
         DC    C'TDCADCFL 2178L3 TAL001'                                        
         DC    C'TDCADCFL 2181L3 TSP005'                                        
         DC    C'TDCADCFL 2182L3 TSP001'                                        
         DC    C'TDCADCFL 2185L3 WPB005'                                        
         DC    C'TDCADCFL 2186L3 WPB001'                                        
         DC    C'TDCADCFL 2189L1 CRE006'                                        
         DC    C'TDCADCFL 2190L1 CRE001'                                        
         DC    C'TDCADCFL 2193L1 COS006'                                        
         DC    C'TDCADCFL 2194L1 COS001'                                        
         DC    C'TDCADCFL 2197L1 DEN006'                                        
         DC    C'TDCADCFL 2198L1 DEN001'                                        
         DC    C'TDCADCFL 2201L1 FRV007'                                        
         DC    C'TDCADCFL 2202L1 FRV001'                                        
         DC    C'TDCADCFL 2205L1 LVG007'                                        
         DC    C'TDCADCFL 2206L1 LVG001'                                        
         DC    C'TDCADCFL 2213L1 MTS006'                                        
         DC    C'TDCADCFL 2214L1 MTS001'                                        
         DC    C'TDCADCFL 2218L1 PLS001'                                        
         DC    C'TDCADCFL 2221L1 PHO007'                                        
         DC    C'TDCADCFL 2222L1 PHO001'                                        
         DC    C'TDCADCFL 2229L1 SAC006'                                        
         DC    C'TDCADCFL 2230L1 SAC001'                                        
         DC    C'TDCADCFL 2233L1 SFR006'                                        
         DC    C'TDCADCFL 2234L1 SFR001'                                        
         DC    C'TDCADCFL 2237L1 SSS006'                                        
         DC    C'TDCADCFL 2238L1 SSS001'                                        
         DC    C'TDCADCFL 2241L1 TUC006'                                        
         DC    C'TDCADCFL 2242L1 TUC001'                                        
         DC    C'TDCADCFL 2360L1 SDI002'                                        
         DC    C'TDCADCFL 2363L3 MOB006'                                        
         DC    C'TDCADCFL 2364L3 MOB001'                                        
         DC    C'TDCADCFL 2367L3 GAI006'                                        
         DC    C'TDCADCFL 2375L3 PAN006'                                        
         DC    C'TDCADCFL 2379L3 LEX006'                                        
         DC    C'TDCADCFL 2380L3 LEX001'                                        
         DC    C'TDCADCFL 2383L4 BOS006'                                        
         DC    C'TDCADCFL 2385L2 STL001'                                        
         DC    C'TDCADCFL 2388L4 PRO006'                                        
         DC    C'TDCADCFL 2389L4 PRO001'                                        
         DC    C'TDCADCFL 2415L4 BOS001'                                        
         DC    C'TDCADCFL 2450L5 COL002'                                        
         DC    C'TDCADCFL 2451L4 ROC002'                                        
         DC    C'TDCADCFL 2453L4 ALB001'                                        
         DC    C'TDCADCFL 2460L1 SEA006'                                        
         DC    C'TDCADCFL 2461L1 SEA001'                                        
         DC    C'TDCADCFL 2464L2 AUS001'                                        
         DC    C'TDCADCFL 2465L4 WBS006'                                        
         DC    C'TDCHEBON 7201V1 BON245'                                        
         DC    C'TDCHEBON 7201V2 BON245'                                        
         DC    C'TDCHEBON 7201V3 BON245'                                        
         DC    C'TDCHEBON 7201V5 BON245'                                        
         DC    C'TDCHECAV 2944V1 SAC001'                                        
         DC    C'TDCHECFL 0085V2 KAN019'                                        
         DC    C'TDCHECFL 0221V4 BAL028'                                        
         DC    C'TDCHECFL 0238V4 BAN029'                                        
         DC    C'TDCHECFL 0246V2 HMB020'                                        
         DC    C'TDCHECFL 0247V2 NEW020'                                        
         DC    C'TDCHECFL 0253V2 SAN020'                                        
         DC    C'TDCHECFL 0262V2 STL020'                                        
         DC    C'TDCHECFL 0266V2 JCK020'                                        
         DC    C'TDCHECFL 0269V5 CHI037'                                        
         DC    C'TDCHECFL 0276V2 SPR020'                                        
         DC    C'TDCHECFL 0281V2 TUL020'                                        
         DC    C'TDCHECFL 0288V5 DRM037'                                        
         DC    C'TDCHECFL 0301V2 MEM022'                                        
         DC    C'TDCHECFL 0304V4 BAN030'                                        
         DC    C'TDCHECFL 0311V2 AMA023'                                        
         DC    C'TDCHECFL 0316V4 PHI002'                                        
         DC    C'TDCHECFL 0318V5 RKF037'                                        
         DC    C'TDCHECFL 0322V2 LKC024'                                        
         DC    C'TDCHECFL 2001V5 CHI038'                                        
         DC    C'TDCHECFL 2002V5 CHI001'                                        
         DC    C'TDCHECFL 2005V5 CIN038'                                        
         DC    C'TDCHECFL 2006V5 CIN001'                                        
         DC    C'TDCHECFL 2007V5 CHI005'                                        
         DC    C'TDCHECFL 2009V5 CLE038'                                        
         DC    C'TDCHECFL 2010V5 CLE001'                                        
         DC    C'TDCHECFL 2013V5 COL038'                                        
         DC    C'TDCHECFL 2014V5 COL001'                                        
         DC    C'TDCHECFL 2016V5 CLE002'                                        
         DC    C'TDCHECFL 2017V5 DRM038'                                        
         DC    C'TDCHECFL 2018V5 DRM001'                                        
         DC    C'TDCHECFL 2021V5 DAY038'                                        
         DC    C'TDCHECFL 2022V5 DAY001'                                        
         DC    C'TDCHECFL 2025V5 LCE038'                                        
         DC    C'TDCHECFL 2029V5 MIL038'                                        
         DC    C'TDCHECFL 2030V5 MIL001'                                        
         DC    C'TDCHECFL 2033V5 WRH038'                                        
         DC    C'TDCHECFL 2036V5 IND001'                                        
         DC    C'TDCHECFL 2037V4 ALB031'                                        
         DC    C'TDCHECFL 2038V4 ALB001'                                        
         DC    C'TDCHECFL 2041V4 BAL031'                                        
         DC    C'TDCHECFL 2042V4 BAL001'                                        
         DC    C'TDCHECFL 2045V4 BAN031'                                        
         DC    C'TDCHECFL 2046V4 BAN001'                                        
         DC    C'TDCHECFL 2049V4 BUF031'                                        
         DC    C'TDCHECFL 2050V4 BUF001'                                        
         DC    C'TDCHECFL 2053V4 HRG031'                                        
         DC    C'TDCHECFL 2054V4 HRG001'                                        
         DC    C'TDCHECFL 2057V4 HRB031'                                        
         DC    C'TDCHECFL 2058V4 HRB001'                                        
         DC    C'TDCHECFL 2065V4 NYK031'                                        
         DC    C'TDCHECFL 2066V4 NYK001'                                        
         DC    C'TDCHECFL 2069V4 PHI031'                                        
         DC    C'TDCHECFL 2070V4 PHI001'                                        
         DC    C'TDCHECFL 2073V4 PIT031'                                        
         DC    C'TDCHECFL 2074V4 PIT001'                                        
         DC    C'TDCHECFL 2078V4 PTA001'                                        
         DC    C'TDCHECFL 2081V4 PRO031'                                        
         DC    C'TDCHECFL 2082V4 PRO001'                                        
         DC    C'TDCHECFL 2085V4 ROC031'                                        
         DC    C'TDCHECFL 2086V4 ROC001'                                        
         DC    C'TDCHECFL 2089V4 SAL031'                                        
         DC    C'TDCHECFL 2093V4 SYR031'                                        
         DC    C'TDCHECFL 2094V4 SYR001'                                        
         DC    C'TDCHECFL 2097V4 WAS031'                                        
         DC    C'TDCHECFL 2098V4 WAS001'                                        
         DC    C'TDCHECFL 2101V2 AMA025'                                        
         DC    C'TDCHECFL 2102V2 AMA001'                                        
         DC    C'TDCHECFL 2105V2 JCK025'                                        
         DC    C'TDCHECFL 2106V2 JCK003'                                        
         DC    C'TDCHECFL 2109V2 KAN025'                                        
         DC    C'TDCHECFL 2110V2 KAN004'                                        
         DC    C'TDCHECFL 2113V2 LKC025'                                        
         DC    C'TDCHECFL 2114V2 LKC005'                                        
         DC    C'TDCHECFL 2117V2 MEM025'                                        
         DC    C'TDCHECFL 2118V2 MEM006'                                        
         DC    C'TDCHECFL 2121V2 NEW025'                                        
         DC    C'TDCHECFL 2122V2 NEW001'                                        
         DC    C'TDCHECFL 2125V2 SAN025'                                        
         DC    C'TDCHECFL 2126V2 SAN001'                                        
         DC    C'TDCHECFL 2129V2 SPR025'                                        
         DC    C'TDCHECFL 2130V2 SPR001'                                        
         DC    C'TDCHECFL 2133V2 STL025'                                        
         DC    C'TDCHECFL 2134V2 STL001'                                        
         DC    C'TDCHECFL 2137V2 TUL025'                                        
         DC    C'TDCHECFL 2138V2 TUL003'                                        
         DC    C'TDCHECFL 2141V3 ALY014'                                        
         DC    C'TDCHECFL 2142V3 ALY001'                                        
         DC    C'TDCHECFL 2145V3 ATL014'                                        
         DC    C'TDCHECFL 2146V3 ATL001'                                        
         DC    C'TDCHECFL 2150V3 AUG001'                                        
         DC    C'TDCHECFL 2153V3 CHR014'                                        
         DC    C'TDCHECFL 2154V3 CHR001'                                        
         DC    C'TDCHECFL 2157V3 CHA014'                                        
         DC    C'TDCHECFL 2158V3 CHA001'                                        
         DC    C'TDCHECFL 2161V3 CHN014'                                        
         DC    C'TDCHECFL 2162V3 CHN001'                                        
         DC    C'TDCHECFL 2165V3 CLB014'                                        
         DC    C'TDCHECFL 2166V3 CLB001'                                        
         DC    C'TDCHECFL 2169V3 CLM014'                                        
         DC    C'TDCHECFL 2170V3 CLM001'                                        
         DC    C'TDCHECFL 2173V3 EVA014'                                        
         DC    C'TDCHECFL 2174V3 EVA001'                                        
         DC    C'TDCHECFL 2177V3 FTM014'                                        
         DC    C'TDCHECFL 2178V3 FTM001'                                        
         DC    C'TDCHECFL 2181V3 GHP014'                                        
         DC    C'TDCHECFL 2182V3 GHP001'                                        
         DC    C'TDCHECFL 2185V3 GNW014'                                        
         DC    C'TDCHECFL 2186V3 GNW001'                                        
         DC    C'TDCHECFL 2189V3 GSP014'                                        
         DC    C'TDCHECFL 2190V3 GSP001'                                        
         DC    C'TDCHECFL 2193V3 HUN014'                                        
         DC    C'TDCHECFL 2194V3 HUN001'                                        
         DC    C'TDCHECFL 2197V3 JAC014'                                        
         DC    C'TDCHECFL 2198V3 JAC001'                                        
         DC    C'TDCHECFL 2201V3 KNO014'                                        
         DC    C'TDCHECFL 2202V3 KNO001'                                        
         DC    C'TDCHECFL 2205V3 LEX014'                                        
         DC    C'TDCHECFL 2206V3 LEX001'                                        
         DC    C'TDCHECFL 2209V3 LOU014'                                        
         DC    C'TDCHECFL 2210V3 LOU001'                                        
         DC    C'TDCHECFL 2213V3 MAC014'                                        
         DC    C'TDCHECFL 2214V3 MAC001'                                        
         DC    C'TDCHECFL 2217V3 MFL014'                                        
         DC    C'TDCHECFL 2218V3 MFL001'                                        
         DC    C'TDCHECFL 2221V3 NAS014'                                        
         DC    C'TDCHECFL 2222V3 NAS001'                                        
         DC    C'TDCHECFL 2225V3 NRF014'                                        
         DC    C'TDCHECFL 2226V3 NRF001'                                        
         DC    C'TDCHECFL 2229V3 ORL014'                                        
         DC    C'TDCHECFL 2230V3 ORL001'                                        
         DC    C'TDCHECFL 2233V3 PAN014'                                        
         DC    C'TDCHECFL 2234V3 PAN001'                                        
         DC    C'TDCHECFL 2235V2 HOU002'                                        
         DC    C'TDCHECFL 2237V3 RDU014'                                        
         DC    C'TDCHECFL 2238V3 RDU001'                                        
         DC    C'TDCHECFL 2241V3 RIC014'                                        
         DC    C'TDCHECFL 2242V3 RIC001'                                        
         DC    C'TDCHECFL 2245V3 ROL014'                                        
         DC    C'TDCHECFL 2246V3 ROL001'                                        
         DC    C'TDCHECFL 2249V3 SAV014'                                        
         DC    C'TDCHECFL 2250V3 SAV001'                                        
         DC    C'TDCHECFL 2253V3 TAL014'                                        
         DC    C'TDCHECFL 2254V3 TAL001'                                        
         DC    C'TDCHECFL 2257V3 TSP014'                                        
         DC    C'TDCHECFL 2258V3 TSP001'                                        
         DC    C'TDCHECFL 2261V3 WPB014'                                        
         DC    C'TDCHECFL 2262V3 WPB001'                                        
         DC    C'TDCHECFL 2265V3 WIL014'                                        
         DC    C'TDCHECFL 2266V3 WIL001'                                        
         DC    C'TDCHECFL 2273V1 CRE048'                                        
         DC    C'TDCHECFL 2274V1 CRE007'                                        
         DC    C'TDCHECFL 2277V1 COS048'                                        
         DC    C'TDCHECFL 2278V1 COS002'                                        
         DC    C'TDCHECFL 2281V1 DEN048'                                        
         DC    C'TDCHECFL 2282V1 DEN004'                                        
         DC    C'TDCHECFL 2289V1 FRV048'                                        
         DC    C'TDCHECFL 2290V1 FRV007'                                        
         DC    C'TDCHECFL 2301V1 MIB048'                                        
         DC    C'TDCHECFL 2302V1 MIB007'                                        
         DC    C'TDCHECFL 2306V1 MTS007'                                        
         DC    C'TDCHECFL 2313V1 PHO048'                                        
         DC    C'TDCHECFL 2314V1 PHO007'                                        
         DC    C'TDCHECFL 2317V1 POR048'                                        
         DC    C'TDCHECFL 2318V1 POR004'                                        
         DC    C'TDCHECFL 2325V1 SAC048'                                        
         DC    C'TDCHECFL 2326V1 SAC002'                                        
         DC    C'TDCHECFL 2333V1 SDI048'                                        
         DC    C'TDCHECFL 2341V1 SEA048'                                        
         DC    C'TDCHECFL 2342V1 SEA007'                                        
         DC    C'TDCHECFL 2349V1 TUC048'                                        
         DC    C'TDCHECFL 2397V1 LVG048'                                        
         DC    C'TDCHECFL 2458V2 TUL001'                                        
         DC    C'TDCHECFL 2558V1 PHO002'                                        
         DC    C'TDCHECFL 2563V1 ALS002'                                        
         DC    C'TDCHECFL 2565V1 ALS048'                                        
         DC    C'TDCHECFL 2569V2 HMB025'                                        
         DC    C'TDCHECFL 2570V2 HMB008'                                        
         DC    C'TDCHECFL 2590V1 DEN002'                                        
         DC    C'TDCHECFL 2626V1 PHO003'                                        
         DC    C'TDCHECFL 2653V5 CSD038'                                        
         DC    C'TDCHECFL 2654V5 CSD001'                                        
         DC    C'TDCHECFL 2661V5 FTW038'                                        
         DC    C'TDCHECFL 2662V5 FTW001'                                        
         DC    C'TDCHECFL 2665V5 GBA038'                                        
         DC    C'TDCHECFL 2666V5 GBA001'                                        
         DC    C'TDCHECFL 2669V5 GRA038'                                        
         DC    C'TDCHECFL 2670V5 GRA001'                                        
         DC    C'TDCHECFL 2673V5 MAD038'                                        
         DC    C'TDCHECFL 2677V5 MIN038'                                        
         DC    C'TDCHECFL 2678V5 MIN001'                                        
         DC    C'TDCHECFL 2681V5 PBM038'                                        
         DC    C'TDCHECFL 2682V5 PBM001'                                        
         DC    C'TDCHECFL 2685V5 DEM001'                                        
         DC    C'TDCHECFL 2685V5 RKF038'                                        
         DC    C'TDCHECFL 2689V5 IND038'                                        
         DC    C'TDCHECFL 2690V5 IND002'                                        
         DC    C'TDCHECFL 2702V5 CHI003'                                        
         DC    C'TDCHECFL 2710V5 FTW002'                                        
         DC    C'TDCHECFL 2717V3 DOT014'                                        
         DC    C'TDCHECFL 2718V3 DOT001'                                        
         DC    C'TDCHECFL 2729V3 MOB014'                                        
         DC    C'TDCHECFL 2730V3 MOB001'                                        
         DC    C'TDCHECFL 2733V3 MON014'                                        
         DC    C'TDCHECFL 2734V3 MON001'                                        
         DC    C'TDCHECFL 2802V3 MFL002'                                        
         DC    C'TDCHECFL 2823V1 BEN048'                                        
         DC    C'TDCHECFL 2824V1 BEN002'                                        
         DC    C'TDCHECFL 2825V1 CRE049'                                        
         DC    C'TDCHECFL 2826V1 CRE003'                                        
         DC    C'TDCHECFL 2827V1 COS049'                                        
         DC    C'TDCHECFL 2828V1 COS003'                                        
         DC    C'TDCHECFL 2830V1 DEN003'                                        
         DC    C'TDCHECFL 2831V1 EUG049'                                        
         DC    C'TDCHECFL 2832V1 EUG004'                                        
         DC    C'TDCHECFL 2833V1 FRV049'                                        
         DC    C'TDCHECFL 2834V1 FRV003'                                        
         DC    C'TDCHECFL 2835V1 HON048'                                        
         DC    C'TDCHECFL 2836V1 HON002'                                        
         DC    C'TDCHECFL 2839V1 MDK048'                                        
         DC    C'TDCHECFL 2840V1 MDK002'                                        
         DC    C'TDCHECFL 2842V1 MIB003'                                        
         DC    C'TDCHECFL 2844V1 POR053'                                        
         DC    C'TDCHECFL 2846V1 PHO004'                                        
         DC    C'TDCHECFL 2847V1 POR052'                                        
         DC    C'TDCHECFL 2848V1 POR003'                                        
         DC    C'TDCHECFL 2849V1 SAC052'                                        
         DC    C'TDCHECFL 2850V1 SAC004'                                        
         DC    C'TDCHECFL 2851V1 SDI052'                                        
         DC    C'TDCHECFL 2853V1 SFR052'                                        
         DC    C'TDCHECFL 2855V1 SEA052'                                        
         DC    C'TDCHECFL 2856V1 SEA002'                                        
         DC    C'TDCHECFL 2858V1 TUC002'                                        
         DC    C'TDCHECFL 2859V3 CHU014'                                        
         DC    C'TDCHECFL 2860V3 CHU001'                                        
         DC    C'TDCHECFL 2863V4 SYR032'                                        
         DC    C'TDCHECFL 2864V5 DEM038'                                        
         DC    C'TDCHECFL 2865V5 DEM001'                                        
         DC    C'TDCHECFL 2872V4 PRO032'                                        
         DC    C'TDCHECFL 2873V1 SEA003'                                        
         DC    C'TDCHECFL 2874V1 MDK049'                                        
         DC    C'TDCHECFL 2875V1 MDK003'                                        
         DC    C'TDCHECFL 2878V3 FLM014'                                        
         DC    C'TDCHECFL 2879V3 FLM001'                                        
         DC    C'TDCHECFL 2920V5 LAF038'                                        
         DC    C'TDCHECFL 2921V5 LAF001'                                        
         DC    C'TDCHECFL 2928V5 DRM040'                                        
         DC    C'TDCHECFL 2929V4 SAL032'                                        
         DC    C'TDCHECFL 2930V4 WAS032'                                        
         DC    C'TDCHECFL 2931V1 SEA051'                                        
         DC    C'TDCHECFL 2932V1 POR051'                                        
         DC    C'TDCHECFL 2934V4 BAN005'                                        
         DC    C'TDCHECFL 2935V4 BAN003'                                        
         DC    C'TDCHECFL 2945V1 SDI051'                                        
         DC    C'TDCHECFL 2948V1 PHO005'                                        
         DC    C'TDCHECFL 2949V1 PHO006'                                        
         DC    C'TDCHECFL 2950V1 DEN051'                                        
         DC    C'TDCHECFL 2951V2 SPR007'                                        
         DC    C'TDCHECFL 2951V2 SPR027'                                        
         DC    C'TDCHECFL 2960V4 BIN031'                                        
         DC    C'TDCHECFL 2961V4 BIN001'                                        
         DC    C'TDCHECFL 2971V4 ALB032'                                        
         DC    C'TDCHECFL 2972V5 LCE039'                                        
         DC    C'TDCHECFL 2973V5 WRH039'                                        
         DC    C'TDCHECFL 2978V3 KNO015'                                        
         DC    C'TDCHECFL 2979V3 LEX015'                                        
         DC    C'TDCHECFL 2986V4 PRO033'                                        
         DC    C'TDCHECFL 2987V4 PHI032'                                        
         DC    C'TDCHECFL 2988V3 CHN015'                                        
         DC    C'TDCHECFL 2989V4 BAN032'                                        
         DC    C'TDCHECFL 2990V3 LOU015'                                        
         DC    C'TDCHECFL 2991V4 PIT032'                                        
         DC    C'TDCHECFL 2994V5 DEM004'                                        
         DC    C'TDCHECFL 2995V4 WAS033'                                        
         DC    C'TDCHECFL 2996V4 PRO007'                                        
         DC    C'TDCHECFL 3002V4 ALB007'                                        
         DC    C'TDCHECFL 3006V4 ALB033'                                        
         DC    C'TDCHECFL 3007V4 PRO034'                                        
         DC    C'TDCHECFL 3012V4 ALB034'                                        
         DC    C'TDCHECFL 3016V4 BAN035'                                        
         DC    C'TDCHECFL 3024V2 MEM009'                                        
         DC    C'TDCHECFL 3026V4 PHI009'                                        
         DC    C'TDCHECFL 3027V3 CHR015'                                        
         DC    C'TDCHETFL 0252V2 NEW021'                                        
         DC    C'TDCHETFL 2561V1 ALS050'                                        
         DC    C'TDCHETFL 2563V1 ALS007'                                        
         DC    C'TDCHETFL 2577V1 BEN050'                                        
         DC    C'TDCHETFL 2578V1 BEN007'                                        
         DC    C'TDCHETFL 2581V1 CRE050'                                        
         DC    C'TDCHETFL 2582V1 CRE002'                                        
         DC    C'TDCHETFL 2585V1 COS050'                                        
         DC    C'TDCHETFL 2586V1 COS007'                                        
         DC    C'TDCHETFL 2589V1 DEN050'                                        
         DC    C'TDCHETFL 2590V1 DEN007'                                        
         DC    C'TDCHETFL 2593V1 EUG050'                                        
         DC    C'TDCHETFL 2594V1 EUG002'                                        
         DC    C'TDCHETFL 2597V1 FRV050'                                        
         DC    C'TDCHETFL 2598V1 FRV002'                                        
         DC    C'TDCHETFL 2601V1 HON050'                                        
         DC    C'TDCHETFL 2602V1 HON007'                                        
         DC    C'TDCHETFL 2609V1 LVG050'                                        
         DC    C'TDCHETFL 2613V1 MDK050'                                        
         DC    C'TDCHETFL 2614V1 MDK007'                                        
         DC    C'TDCHETFL 2617V1 MIB050'                                        
         DC    C'TDCHETFL 2618V1 MIB007'                                        
         DC    C'TDCHETFL 2625V1 PHO050'                                        
         DC    C'TDCHETFL 2626V1 PHO002'                                        
         DC    C'TDCHETFL 2629V1 POR050'                                        
         DC    C'TDCHETFL 2630V1 POR002'                                        
         DC    C'TDCHETFL 2633V1 SAC050'                                        
         DC    C'TDCHETFL 2634V1 SAC003'                                        
         DC    C'TDCHETFL 2641V1 SEA050'                                        
         DC    C'TDCHETFL 2642V1 EUG007'                                        
         DC    C'TDCHETFL 2645V1 SFR050'                                        
         DC    C'TDCHETFL 2646V1 SFR007'                                        
         DC    C'TDCHETFL 2649V1 TUC050'                                        
         DC    C'TDCHETFL 2650V1 TUC007'                                        
         DC    C'TDCHETFL 2954V2 NEW026'                                        
         DC    C'TDCHETFL 2955V2 NEW002'                                        
         DC    C'TDCHETFL 3024V2 MEM010'                                        
         DC    C'TDPONBON 7201N1 BON245'                                        
         DC    C'TDPONBON 7201N3 BON245'                                        
         DC    C'TDPONCFL 0019N1 MTS007'                                        
         DC    C'TDPONCFL 0035N3 BIR006'                                        
         DC    C'TDPONCFL 0037N3 RDU006'                                        
         DC    C'TDPONCFL 0038N3 CHA006'                                        
         DC    C'TDPONCFL 0078N1 DEN008'                                        
         DC    C'TDPONCFL 0087N2 HMB010'                                        
         DC    C'TDPONCFL 0089N1 CRE008'                                        
         DC    C'TDPONCFL 0094N1 HON008'                                        
         DC    C'TDPONCFL 0100N1 FRV008'                                        
         DC    C'TDPONCFL 0101N1 SDI007'                                        
         DC    C'TDPONCFL 0102N1 BIL007'                                        
         DC    C'TDPONCFL 0103N1 BBZ007'                                        
         DC    C'TDPONCFL 2001N5 CHI008'                                        
         DC    C'TDPONCFL 2002N5 CHI001'                                        
         DC    C'TDPONCFL 2005N5 DAY008'                                        
         DC    C'TDPONCFL 2006N5 DAY001'                                        
         DC    C'TDPONCFL 2009N5 FTW008'                                        
         DC    C'TDPONCFL 2010N5 FTW001'                                        
         DC    C'TDPONCFL 2013N5 LCE008'                                        
         DC    C'TDPONCFL 2014N5 LCE001'                                        
         DC    C'TDPONCFL 2017N5 LAF008'                                        
         DC    C'TDPONCFL 2018N5 LAF001'                                        
         DC    C'TDPONCFL 2021N5 MAD008'                                        
         DC    C'TDPONCFL 2022N5 MAD001'                                        
         DC    C'TDPONCFL 2029N5 MIN008'                                        
         DC    C'TDPONCFL 2030N5 MIN001'                                        
         DC    C'TDPONCFL 2033N5 RKF008'                                        
         DC    C'TDPONCFL 2034N5 RKF001'                                        
         DC    C'TDPONCFL 2037N5 WRH008'                                        
         DC    C'TDPONCFL 2038N5 WRH001'                                        
         DC    C'TDPONCFL 2041N4 BAL014'                                        
         DC    C'TDPONCFL 2042N4 BAL001'                                        
         DC    C'TDPONCFL 2045N4 BUF014'                                        
         DC    C'TDPONCFL 2046N4 BUF001'                                        
         DC    C'TDPONCFL 2053N4 PHI014'                                        
         DC    C'TDPONCFL 2054N4 PHI001'                                        
         DC    C'TDPONCFL 2057N4 PRO014'                                        
         DC    C'TDPONCFL 2058N4 PRO001'                                        
         DC    C'TDPONCFL 2061N4 ROC014'                                        
         DC    C'TDPONCFL 2062N4 ROC001'                                        
         DC    C'TDPONCFL 2065N4 SYR014'                                        
         DC    C'TDPONCFL 2066N4 SYR001'                                        
         DC    C'TDPONCFL 2069N4 WAS014'                                        
         DC    C'TDPONCFL 2070N4 WAS001'                                        
         DC    C'TDPONCFL 2073N2 BTR009'                                        
         DC    C'TDPONCFL 2074N2 BTR001'                                        
         DC    C'TDPONCFL 2077N2 HMB009'                                        
         DC    C'TDPONCFL 2078N2 HMB001'                                        
         DC    C'TDPONCFL 2081N2 JCK009'                                        
         DC    C'TDPONCFL 2082N2 JCK001'                                        
         DC    C'TDPONCFL 2085N2 KAN009'                                        
         DC    C'TDPONCFL 2086N2 KAN001'                                        
         DC    C'TDPONCFL 2089N2 MEM009'                                        
         DC    C'TDPONCFL 2090N2 MEM001'                                        
         DC    C'TDPONCFL 2093N2 NEW009'                                        
         DC    C'TDPONCFL 2094N2 NEW001'                                        
         DC    C'TDPONCFL 2097N2 SAN009'                                        
         DC    C'TDPONCFL 2098N2 SAN001'                                        
         DC    C'TDPONCFL 2101N2 SPR009'                                        
         DC    C'TDPONCFL 2102N2 SPR001'                                        
         DC    C'TDPONCFL 2105N3 ALY001'                                        
         DC    C'TDPONCFL 2108N3 BIR001'                                        
         DC    C'TDPONCFL 2111N3 CHR001'                                        
         DC    C'TDPONCFL 2114N3 CHA001'                                        
         DC    C'TDPONCFL 2117N3 CHN001'                                        
         DC    C'TDPONCFL 2120N3 CLM001'                                        
         DC    C'TDPONCFL 2123N3 DOT001'                                        
         DC    C'TDPONCFL 2126N3 EVA001'                                        
         DC    C'TDPONCFL 2129N3 FTM001'                                        
         DC    C'TDPONCFL 2132N3 GAI001'                                        
         DC    C'TDPONCFL 2135N3 HUN001'                                        
         DC    C'TDPONCFL 2138N3 JAC001'                                        
         DC    C'TDPONCFL 2141N3 LOU001'                                        
         DC    C'TDPONCFL 2144N3 MAC001'                                        
         DC    C'TDPONCFL 2150N3 MOB001'                                        
         DC    C'TDPONCFL 2153N3 MON001'                                        
         DC    C'TDPONCFL 2156N3 NRF001'                                        
         DC    C'TDPONCFL 2159N3 ORL001'                                        
         DC    C'TDPONCFL 2162N3 PAN001'                                        
         DC    C'TDPONCFL 2165N3 RDU001'                                        
         DC    C'TDPONCFL 2168N3 TAL001'                                        
         DC    C'TDPONCFL 2171N3 WPB001'                                        
         DC    C'TDPONCFL 2174N1 BAK010'                                        
         DC    C'TDPONCFL 2175N1 BAK001'                                        
         DC    C'TDPONCFL 2178N1 BIL009'                                        
         DC    C'TDPONCFL 2179N1 BIL001'                                        
         DC    C'TDPONCFL 2182N1 CRE010'                                        
         DC    C'TDPONCFL 2183N1 CRE001'                                        
         DC    C'TDPONCFL 2186N1 COS010'                                        
         DC    C'TDPONCFL 2187N1 COS001'                                        
         DC    C'TDPONCFL 2190N1 DEN010'                                        
         DC    C'TDPONCFL 2191N1 DEN001'                                        
         DC    C'TDPONCFL 2194N1 EUG010'                                        
         DC    C'TDPONCFL 2195N1 EUG001'                                        
         DC    C'TDPONCFL 2198N1 FRV010'                                        
         DC    C'TDPONCFL 2199N1 FRV001'                                        
         DC    C'TDPONCFL 2206N1 HON010'                                        
         DC    C'TDPONCFL 2207N1 HON001'                                        
         DC    C'TDPONCFL 2210N1 MTS010'                                        
         DC    C'TDPONCFL 2211N1 MTS001'                                        
         DC    C'TDPONCFL 2214N1 POR010'                                        
         DC    C'TDPONCFL 2215N1 POR001'                                        
         DC    C'TDPONCFL 2218N1 SAC010'                                        
         DC    C'TDPONCFL 2219N1 SAC001'                                        
         DC    C'TDPONCFL 2222N1 SDI010'                                        
         DC    C'TDPONCFL 2223N1 SDI001'                                        
         DC    C'TDPONCFL 2226N1 SFR010'                                        
         DC    C'TDPONCFL 2227N1 SFR001'                                        
         DC    C'TDPONCFL 2230N1 TUC010'                                        
         DC    C'TDPONCFL 2231N1 TUC001'                                        
         DC    C'TDPONCFL 2234N2 HOU009'                                        
         DC    C'TDPONCFL 2235N2 HOU001'                                        
         DC    C'TDPONCFL 2238N2 AMA009'                                        
         DC    C'TDPONCFL 2239N2 AMA001'                                        
         DC    C'TDPONCFL 2242N4 HRG014'                                        
         DC    C'TDPONCFL 2243N4 HRG001'                                        
         DC    C'TDPONCFL 2250N1 BBZ009'                                        
         DC    C'TDPONCFL 2251N1 BBZ001'                                        
         DC    C'TDPONCFL 2262N1 PHO010'                                        
         DC    C'TDPONCFL 2263N1 PHO001'                                        
         DC    C'TDPONCFL 2274N3 ALY007'                                        
         DC    C'TDPONCFL 2275N3 BIR007'                                        
         DC    C'TDPONCFL 2277N3 CHA007'                                        
         DC    C'TDPONCFL 2278N3 CHN007'                                        
         DC    C'TDPONCFL 2279N3 CLM007'                                        
         DC    C'TDPONCFL 2280N3 DOT007'                                        
         DC    C'TDPONCFL 2281N3 EVA007'                                        
         DC    C'TDPONCFL 2283N3 FTM007'                                        
         DC    C'TDPONCFL 2285N3 HUN007'                                        
         DC    C'TDPONCFL 2286N3 JAC007'                                        
         DC    C'TDPONCFL 2287N3 LOU007'                                        
         DC    C'TDPONCFL 2288N3 MAC007'                                        
         DC    C'TDPONCFL 2290N3 MOB007'                                        
         DC    C'TDPONCFL 2291N3 MON007'                                        
         DC    C'TDPONCFL 2292N3 NRF007'                                        
         DC    C'TDPONCFL 2293N3 ORL007'                                        
         DC    C'TDPONCFL 2294N3 PAN007'                                        
         DC    C'TDPONCFL 2295N3 RDU007'                                        
         DC    C'TDPONCFL 2296N3 TAL007'                                        
         DC    C'TDPONCFL 2297N3 WPB007'                                        
         DC    C'TDPONCFL 2302N5 MIN003'                                        
         DC    C'TDPONCFL 2303N5 CIN008'                                        
         DC    C'TDPONCFL 2304N5 CIN001'                                        
         DC    C'TDPONCFL 2305N5 COL008'                                        
         DC    C'TDPONCFL 2306N5 COL001'                                        
         DC    C'TDPONCFL 2307N5 CLE008'                                        
         DC    C'TDPONCFL 2308N5 CLE001'                                        
         DC    C'TDPONCFL 2312N5 GRA008'                                        
         DC    C'TDPONCFL 2313N5 GRA001'                                        
         DC    C'TDPONCFL 2319N3 GHP007'                                        
         DC    C'TDPONCFL 2321N4 BAL002'                                        
         DC    C'TDPONCFL 2324N3 FLM002'                                        
         DC    C'TDPONCFL 2326N2 WTB009'                                        
         DC    C'TDPONCFL 2327N2 WTB002'                                        
         DC    C'TDPONPFL 0025N1 DEN007'                                        
         DC    C'TDPONPFL 2320N3 MFL001'                                        
         DC    C'TDTGMBON 7201M1 BON245'                                        
         DC    C'TDTGMBON 7201M2 BON245'                                        
         DC    C'TDTGMBON 7201M3 BON245'                                        
         DC    C'TDTGMBON 7201M5 BON245'                                        
         DC    C'TDTGMTFL 0036M5 COL012'                                        
         DC    C'TDTGMTFL 0045M1 DEN007'                                        
         DC    C'TDTGMTFL 0087M5 LCE012'                                        
         DC    C'TDTGMTFL 0088M5 WRH012'                                        
         DC    C'TDTGMTFL 0093M2 NEW009'                                        
         DC    C'TDTGMTFL 0096M5 CIN012'                                        
         DC    C'TDTGMTFL 0116M5 CHI012'                                        
         DC    C'TDTGMTFL 0117M5 MIN012'                                        
         DC    C'TDTGMTFL 0122M2 AMA010'                                        
         DC    C'TDTGMTFL 0124M5 GBA012'                                        
         DC    C'TDTGMTFL 2001M5 CIN013'                                        
         DC    C'TDTGMTFL 2002M5 CIN001'                                        
         DC    C'TDTGMTFL 2005M5 COL013'                                        
         DC    C'TDTGMTFL 2006M5 COL001'                                        
         DC    C'TDTGMTFL 2009M5 FTW013'                                        
         DC    C'TDTGMTFL 2010M5 FTW001'                                        
         DC    C'TDTGMTFL 2013M5 GBA013'                                        
         DC    C'TDTGMTFL 2014M5 GBA001'                                        
         DC    C'TDTGMTFL 2017M5 MAD013'                                        
         DC    C'TDTGMTFL 2018M5 MAD001'                                        
         DC    C'TDTGMTFL 2021M5 RKF013'                                        
         DC    C'TDTGMTFL 2022M5 RKF001'                                        
         DC    C'TDTGMTFL 2025M5 WRH013'                                        
         DC    C'TDTGMTFL 2026M5 WRH001'                                        
         DC    C'TDTGMTFL 2029M4 BAL015'                                        
         DC    C'TDTGMTFL 2030M4 BAL001'                                        
         DC    C'TDTGMTFL 2033M4 BUF015'                                        
         DC    C'TDTGMTFL 2034M4 BUF001'                                        
         DC    C'TDTGMTFL 2037M4 HRG015'                                        
         DC    C'TDTGMTFL 2038M4 HRG001'                                        
         DC    C'TDTGMTFL 2041M4 PHI015'                                        
         DC    C'TDTGMTFL 2042M4 PHI001'                                        
         DC    C'TDTGMTFL 2045M4 PRO015'                                        
         DC    C'TDTGMTFL 2046M4 PRO001'                                        
         DC    C'TDTGMTFL 2049M4 ROC015'                                        
         DC    C'TDTGMTFL 2050M4 ROC001'                                        
         DC    C'TDTGMTFL 2053M4 SYR015'                                        
         DC    C'TDTGMTFL 2054M4 SYR001'                                        
         DC    C'TDTGMTFL 2057M4 WAS015'                                        
         DC    C'TDTGMTFL 2058M4 WAS001'                                        
         DC    C'TDTGMTFL 2061M2 HMB011'                                        
         DC    C'TDTGMTFL 2062M2 HMB001'                                        
         DC    C'TDTGMTFL 2064M2 HMB003'                                        
         DC    C'TDTGMTFL 2065M2 MEM011'                                        
         DC    C'TDTGMTFL 2066M2 MEM001'                                        
         DC    C'TDTGMTFL 2069M2 NEW011'                                        
         DC    C'TDTGMTFL 2070M2 NEW001'                                        
         DC    C'TDTGMTFL 2073M2 SAN011'                                        
         DC    C'TDTGMTFL 2074M2 SAN001'                                        
         DC    C'TDTGMTFL 2077M2 SPR011'                                        
         DC    C'TDTGMTFL 2078M2 SPR002'                                        
         DC    C'TDTGMTFL 2081M2 STL011'                                        
         DC    C'TDTGMTFL 2082M2 STL001'                                        
         DC    C'TDTGMTFL 2085M3 ALY008'                                        
         DC    C'TDTGMTFL 2086M3 ALY001'                                        
         DC    C'TDTGMTFL 2090M3 AUG001'                                        
         DC    C'TDTGMTFL 2093M3 BIR008'                                        
         DC    C'TDTGMTFL 2094M3 BIR001'                                        
         DC    C'TDTGMTFL 2097M3 CLB008'                                        
         DC    C'TDTGMTFL 2098M3 CLB001'                                        
         DC    C'TDTGMTFL 2101M3 CLM008'                                        
         DC    C'TDTGMTFL 2102M3 CLM001'                                        
         DC    C'TDTGMTFL 2105M3 DOT008'                                        
         DC    C'TDTGMTFL 2106M3 DOT001'                                        
         DC    C'TDTGMTFL 2109M3 EVA008'                                        
         DC    C'TDTGMTFL 2110M3 EVA001'                                        
         DC    C'TDTGMTFL 2113M3 FLM008'                                        
         DC    C'TDTGMTFL 2114M3 FLM001'                                        
         DC    C'TDTGMTFL 2117M3 FTM008'                                        
         DC    C'TDTGMTFL 2118M3 FTM002'                                        
         DC    C'TDTGMTFL 2121M3 GAI008'                                        
         DC    C'TDTGMTFL 2122M3 GAI001'                                        
         DC    C'TDTGMTFL 2125M3 JAC008'                                        
         DC    C'TDTGMTFL 2126M3 JAC001'                                        
         DC    C'TDTGMTFL 2129M3 LOU008'                                        
         DC    C'TDTGMTFL 2130M3 LOU001'                                        
         DC    C'TDTGMTFL 2133M3 MAC008'                                        
         DC    C'TDTGMTFL 2134M3 MAC001'                                        
         DC    C'TDTGMTFL 2137M3 MOB008'                                        
         DC    C'TDTGMTFL 2138M3 MOB001'                                        
         DC    C'TDTGMTFL 2141M3 MON008'                                        
         DC    C'TDTGMTFL 2142M3 MON001'                                        
         DC    C'TDTGMTFL 2145M3 NRF008'                                        
         DC    C'TDTGMTFL 2146M3 NRF001'                                        
         DC    C'TDTGMTFL 2149M3 PAN008'                                        
         DC    C'TDTGMTFL 2150M3 PAN001'                                        
         DC    C'TDTGMTFL 2153M3 RDU008'                                        
         DC    C'TDTGMTFL 2154M3 RDU001'                                        
         DC    C'TDTGMTFL 2157M3 SAV008'                                        
         DC    C'TDTGMTFL 2158M3 SAV001'                                        
         DC    C'TDTGMTFL 2161M3 TAL008'                                        
         DC    C'TDTGMTFL 2162M3 TAL001'                                        
         DC    C'TDTGMTFL 2165M3 WPB008'                                        
         DC    C'TDTGMTFL 2166M3 WPB001'                                        
         DC    C'TDTGMTFL 2173M1 BIL008'                                        
         DC    C'TDTGMTFL 2174M1 BIL001'                                        
         DC    C'TDTGMTFL 2177M1 BBZ008'                                        
         DC    C'TDTGMTFL 2178M1 BBZ001'                                        
         DC    C'TDTGMTFL 2181M1 CRE008'                                        
         DC    C'TDTGMTFL 2182M1 CRE001'                                        
         DC    C'TDTGMTFL 2185M1 COS008'                                        
         DC    C'TDTGMTFL 2186M1 COS001'                                        
         DC    C'TDTGMTFL 2189M1 DEN008'                                        
         DC    C'TDTGMTFL 2190M1 DEN001'                                        
         DC    C'TDTGMTFL 2193M1 EUG008'                                        
         DC    C'TDTGMTFL 2194M1 EUG001'                                        
         DC    C'TDTGMTFL 2197M1 FRV008'                                        
         DC    C'TDTGMTFL 2198M1 FRV001'                                        
         DC    C'TDTGMTFL 2201M1 GRF008'                                        
         DC    C'TDTGMTFL 2202M1 GRF001'                                        
         DC    C'TDTGMTFL 2205M1 HON008'                                        
         DC    C'TDTGMTFL 2206M1 HON001'                                        
         DC    C'TDTGMTFL 2213M1 MTS008'                                        
         DC    C'TDTGMTFL 2214M1 MTS001'                                        
         DC    C'TDTGMTFL 2221M1 PHO008'                                        
         DC    C'TDTGMTFL 2222M1 PHO002'                                        
         DC    C'TDTGMTFL 2225M1 POR008'                                        
         DC    C'TDTGMTFL 2226M1 POR001'                                        
         DC    C'TDTGMTFL 2229M1 SAC008'                                        
         DC    C'TDTGMTFL 2230M1 SAC001'                                        
         DC    C'TDTGMTFL 2233M1 SDI008'                                        
         DC    C'TDTGMTFL 2234M1 SDI001'                                        
         DC    C'TDTGMTFL 2237M1 SFR008'                                        
         DC    C'TDTGMTFL 2238M1 SFR001'                                        
         DC    C'TDTGMTFL 2241M1 TUC008'                                        
         DC    C'TDTGMTFL 2242M1 TUC001'                                        
         DC    C'TDTGMTFL 2261M5 CHI013'                                        
         DC    C'TDTGMTFL 2262M5 CHI001'                                        
         DC    C'TDTGMTFL 2269M5 MIN013'                                        
         DC    C'TDTGMTFL 2270M5 MIN001'                                        
         DC    C'TDTGMTFL 2282M2 AMA001'                                        
         DC    C'TDTGMTFL 2285M5 LCE013'                                        
         DC    C'TDTGMTFL 2286M5 LCE001'                                        
         DC    C'TDTGMTFL 2289M4 BPG015'                                        
         DC    C'TDTGMTFL 2290M4 BPG002'                                        
         DC    C'TDTGMTFL 2294M5 MIN002'                                        
         DC    C'TDTGMTFL 2297M3 CHR008'                                        
         DC    C'TDTGMTFL 2298M3 CHR001'                                        
         DC    C'TDTGMTFL 2301M4 PHI004'                                        
         DC    C'TDTGMTFL 2308M4 HAR015'                                        
         DC    C'TDTGMTFL 2314M2 AMA011'                                        
         DC    C'TDTGMTFL 2315M2 AMA002'                                        
         DC    C'TDTGMTFL 2318M5 GRA003'                                        
         DC    C'TDTGMTFL 2319M5 GRA001'                                        
         DC    C'TDTGMTFL 2320M2 STL004'                                        
         DC    C'TDTGMTFL 2321M3 GHP008'                                        
         DC    C'TDTGMTFL 2322M3 GHP001'                                        
         DC    C'TDTGMTFL 2323M5 CLE013'                                        
         DC    C'TDTGMTFL 2324M5 CLE001'                                        
         DC    C'TDTGMTFL 2326M5 TOL014'                                        
         DC    C'TDTGMTFL 2327M3 JAC009'                                        
         DC    C'TDTGMTFL 2328M3 JAC002'                                        
         DC    C'TDTGMTFL 2332M4 HAR001'                                        
         DC    C'TDTGMTFL 2336M5 TOL001'                                        
         DC    C'TDTGMTFL 2346M3 MFL003'                                        
         DC    C'TDTGMTFL 2348M2 WTB011'                                        
         DC    C'TDTGMTFL 2349M2 WTB001'                                        
         DC    C'TDTGMTFL 2352M4 BAL003'                                        
         DC    C'TDTGMTFL 2355M3 CHA001'                                        
         DC    C'TDTGMTFL 2362M1 SFR003'                                        
         DC    C'TGACVPROM0018AC PRO001'                                        
         DC    C'TGBMDCFL 2001BU BB 012'                                        
         DC    C'TGBMDCFL 2002BU BB 013'                                        
         DC    C'TGBMDCFL 2005BU BB 001'                                        
         DC    C'TGBMDCFL 2006BU BB 002'                                        
         DC    C'TGBMDCFL 2007BU BB 014'                                        
         DC    C'TGBMDCFL 2008BU BB 014'                                        
         DC    C'TGBMDCFL 2009BU BB 015'                                        
         DC    C'TGBMDCFL 7201BU BB 245'                                        
         DC    C'TGBMDCFL 8012BU POD022'                                        
         DC    C'TGBMDCFL 8022BU POD023'                                        
         DC    C'TGBMDCFL 8992BU POD026'                                        
         DC    C'TGBMDCFL 9022BU BB 025'                                        
         DC    C'TGBMDCFL 9203BU BB 024'                                        
         DC    C'TGBMDPOD 8012BU POD022'                                        
         DC    C'TGBMDPOD 8022BU POD023'                                        
         DC    C'TGBMDPOD 8992BU POD026'                                        
         DC    C'TGCDLCFL 2002CA MB 002'                                        
         DC    C'TGCDLCFL 9203CA MB 024'                                        
         DC    C'TGCDLESC 2004CA ESC003'                                        
         DC    C'TGCDLEXT 2001CA EXT001'                                        
         DC    C'TGCDLPOD 8012CA POD022'                                        
         DC    C'TGCDLPOD 8202CA POD248'                                        
         DC    C'TGCDLPOD 8992CA POD026'                                        
         DC    C'TGCHNPOD 8992CH POD030'                                        
         DC    C'TGCHVAVA 0117CH TA 009'                                        
         DC    C'TGCHVAVA 2001CH TA 010'                                        
         DC    C'TGCHVAVA 2002CH TA 002'                                        
         DC    C'TGCHVAVA 2018CH TA 012'                                        
         DC    C'TGCHVAVA 9012CH TA 025'                                        
         DC    C'TGCHVAVA 9203CH TA 024'                                        
         DC    C'TGCHVBON 7022CH BON028'                                        
         DC    C'TGCHVBON 7022CH BON244'                                        
         DC    C'TGCHVBON 7201CH BON245'                                        
         DC    C'TGCHVCFL 2031CH CF 012'                                        
         DC    C'TGCHVCFL 9203CH CF 024'                                        
         DC    C'TGCHVIMP 2030CH CI 012'                                        
         DC    C'TGCHVIMP 9203CH CI 024'                                        
         DC    C'TGCHVPOD 8012CH POD022'                                        
         DC    C'TGCHVPOD 8022CH POD023'                                        
         DC    C'TGCHVPOD 8202CH POD248'                                        
         DC    C'TGCHVPOD 8992CH POD026'                                        
         DC    C'TGCHVSIL 0116CH TS 006'                                        
         DC    C'TGCHVSIL 9012CH TS 025'                                        
         DC    C'TGCHVSIL 9203CH TS 024'                                        
         DC    C'TGCHVSUB 0113CH TU 008'                                        
         DC    C'TGCHVSUB 2022CH TU 012'                                        
         DC    C'TGCHVSUB 9012CH TU 025'                                        
         DC    C'TGCHVSUB 9203CH TU 024'                                        
         DC    C'TGCHVTAH 0114CH TH 004'                                        
         DC    C'TGCHVTAH 0115CH TH 005'                                        
         DC    C'TGCHVTAH 2023CH TH 012'                                        
         DC    C'TGCHVTAH 9203CH TH 024'                                        
         DC    C'TGCHVTBL 0115CH TZ 005'                                        
         DC    C'TGCHVTBL 2008CH TZ 011'                                        
         DC    C'TGCHVTBL 2009CH TZ 007'                                        
         DC    C'TGCHVTBL 2019CH TZ 012'                                        
         DC    C'TGCHVTBL 9012CH TZ 025'                                        
         DC    C'TGCHVTBL 9022CH TZ 027'                                        
         DC    C'TGCHVTBL 9203CH TZ 024'                                        
         DC    C'TGCHVTFL 2020CH TF 012'                                        
         DC    C'TGCHVTFL 9012CH TF 025'                                        
         DC    C'TGCHVTFL 9203CH TF 024'                                        
         DC    C'TGCHVTSO 2021CH TS 012'                                        
         DC    C'TGCUVPOD 8982CU POD029'                                        
         DC    C'TGDITGEN 0001DI GEN004'                                        
         DC    C'TGDITGEN 0002DI GEN002'                                        
         DC    C'TGDITGEN 9203DI GEN024'                                        
         DC    C'TGGM (NA)0020LC NA 002'                                        
         DC    C'TGGM (NA)0022LC NA 005'                                        
         DC    C'TGGM (NA)0023LC NA 003'                                        
         DC    C'TGGM (NA)0027LC NA 004'                                        
         DC    C'TGGM (NA)0028LC NA 008'                                        
         DC    C'TGGM KAR 0026LC NA 006'                                        
         DC    C'TGGMFCMI 9702CM CMI025'                                        
         DC    C'TGGMFPOD 8022CM POD027'                                        
         DC    C'TGGMFPOD 8973CM POD026'                                        
         DC    C'TGGMFPOD 8992CM POD028'                                        
         DC    C'TGISUGEN 0001IZ GEN008'                                        
         DC    C'TGISUGEN 0003IZ GEN001'                                        
         DC    C'TGISUGEN 9203IZ GEN024'                                        
         DC    C'TGMAMBON 7002GT BON026'                                        
         DC    C'TGMAMBON 7022GT BON022'                                        
         DC    C'TGMAMBON 7201GT BON025'                                        
         DC    C'TGMAMENV 2004GT GE 003'                                        
         DC    C'TGMAMENV 9203GT GE 024'                                        
         DC    C'TGMAMPOD 8012GT POD027'                                        
         DC    C'TGMAMPOD 8202GT POD248'                                        
         DC    C'TGMAMPOD 8973GT POD023'                                        
         DC    C'TGMAMPOD 8982GT POD029'                                        
         DC    C'TGMAMPOD 8992GT POD030'                                        
         DC    C'TGMAMSIE 2003GT GS 003'                                        
         DC    C'TGMAMSIE 9203GT GS 024'                                        
         DC    C'TGMAMSON 9012GT GN 028'                                        
         DC    C'TGMAMTFL 2001GT GB 001'                                        
         DC    C'TGMAMTFL 9203GT GB 024'                                        
         DC    C'TGMAMYUK 2002GT GY 002'                                        
         DC    C'TGMAMYUK 9203GT GY 024'                                        
         DC    C'TGMCOBRN 9996GM FM 024'                                        
         DC    C'TGMCOPOD 8012GM POD025'                                        
         DC    C'TGMCOPOD 8022GM POD023'                                        
         DC    C'TGMCOPOD 8992GM POD026'                                        
         DC    C'TGMONONST9996ON GEN024'                                        
         DC    C'TGMPWGEN 0002GMPGEN001'                                        
         DC    C'TGMTGPOD 8992GT POD030'                                        
         DC    C'TGOLDBON 7201OL BON245'                                        
         DC    C'TGOLDPOD 8022OL POD031'                                        
         DC    C'TGOLDPOD 8992OL POD030'                                        
         DC    C'TGPONBON 7201PO BON025'                                        
         DC    C'TGPONGP  9203PO PP 024'                                        
         DC    C'TGPONNAA 2003PO PA 005'                                        
         DC    C'TGPONNAG 2002PO PG 004'                                        
         DC    C'TGPONNAG 9203PO PG 024'                                        
         DC    C'TGPONNAP 2001PO PP 003'                                        
         DC    C'TGPONNAP 9203PO PP 029'                                        
         DC    C'TGPONPOD 8012PO POD022'                                        
         DC    C'TGPONPOD 8022PO POD028'                                        
         DC    C'TGPONPOD 8202PO POD248'                                        
         DC    C'TGPONPOD 8992PO POD026'                                        
         DC    C'TGPUAPC  0001UA PC 001'                                        
         DC    C'TGPUAPC  0002UA PC 002'                                        
         DC    C'TGPUAPC  9012UA PC 022'                                        
         DC    C'TGPUAPC  9203UA PC 024'                                        
         DC    C'TGSADCFL 0002SD ATL003'                                        
         DC    C'TGSADCFL 0002SD DTW003'                                        
         DC    C'TGSADCFL 0002SD MFL003'                                        
         DC    C'TGSADCFL 0002SD WPB003'                                        
         DC    C'TGSAMBON 7201SM BON245'                                        
         DC    C'TGSAMCFL 0076SM FL 010'                                        
         DC    C'TGSAMCFL 0077SM FL 011'                                        
         DC    C'TGSAMCFL 0078SM FL 012'                                        
         DC    C'TGSAMCFL 0079SM FL 001'                                        
         DC    C'TGSAMCFL 9012SM FL 022'                                        
         DC    C'TGSAMCFL 9203SM FL 024'                                        
         DC    C'TGSAMCFL 9702SM FL 025'                                        
         DC    C'TGSAMCFL 9802SM FL 026'                                        
         DC    C'TGSAMCFL 9996SM FL 027'                                        
         DC    C'TGSAMPOD 8012SM POD028'                                        
         DC    C'TGSAMPOD 8022SM POD023'                                        
         DC    C'TGSAMPOD 8202SM POD029'                                        
         DC    C'TGSAMPOD 8992SM POD030'                                        
         DC    C'TGSATAGR 9996SA SB 027'                                        
         DC    C'TGSATCOR 2001SA SB 001'                                        
         DC    C'TGSATCOR 2002SA SB 002'                                        
         DC    C'TGSATCOR 2003SA SB 003'                                        
         DC    C'TGSATCOR 8202SA POD248'                                        
         DC    C'TGSATCOR 9203SA SB 024'                                        
         DC    C'TGSATPOD 8022SA POD023'                                        
         DC    C'TGSATPOD 8982SA POD025'                                        
         DC    C'TGSATPOD 8992SA POD026'                                        
         DC    C'THCHVAVA 2005CHHTAH000'                                        
         DC    C'THCHVAVA 2005CHHTAH005'                                        
         DC    C'THCHVAVA 2006CHHTAH001'                                        
         DC    C'THCHVBON 7201CHHBON000'                                        
         DC    C'THCHVBON 7201CHHBON025'                                        
         DC    C'THCHVTBL 2002CHHTZH002'                                        
         DC    C'THGMCBON 7201GTHBON000'                                        
         DC    C'THGMCBON 7201GTHBON025'                                        
         DC    C'THMCMTFL 0005COHTFH000'                                        
         DC    C'THPONBON 7201POHBON000'                                        
         DC    C'THPONBON 7201POHBON025'                                        
         DC    C'TH1CVBON 7201C1HBON000'                                        
         DC    C'TH1CVBON 7201C1HBON025'                                        
         DC    C'TH1CVCFL 2033C1HCFH000'                                        
         DC    C'TH1CVCFL 2033C1HCFH012'                                        
         DC    C'TH1CVCFL 2034C1HCFH001'                                        
         DC    C'TH1CVTFL 0007C1HTFH000'                                        
         DC    C'TH1GCTFL 2002G1HGF 001'                                        
         DC    C'TH1POPFL 2001P1HPD 000'                                        
         DC    C'TH1POPFL 2001P1HPD 007'                                        
         DC    C'TH2CVBON 7201C2HBON025'                                        
         DC    C'TH2CVBON 7201C2HTFH000'                                        
         DC    C'TH2CVCAV 2001C2HCA 002'                                        
         DC    C'TH2CVCAV 2010C2HCA 000'                                        
         DC    C'TH2CVCAV 2010C2HCA 009'                                        
         DC    C'TH2CVMAL 2004C2HCBH000'                                        
         DC    C'TH2CVMAL 2004C2HCBH008'                                        
         DC    C'TH2CVMAL 2005C2HCBH002'                                        
         DC    C'TH2CVSIL 2011C2HTSH000'                                        
         DC    C'TH2CVSIL 2011C2HTSH008'                                        
         DC    C'TH2CVSIL 2012C2HTSH002'                                        
         DC    C'TH2CVTBL 2019C2HTZH000'                                        
         DC    C'TH2CVTBL 2019C2HTZH008'                                        
         DC    C'TH2CVTBL 2020C2HTZH002'                                        
         DC    C'TH2CVTFL 2008C2HTFH000'                                        
         DC    C'TH2CVTFL 2008C2HTFH008'                                        
         DC    C'TH2CVTFL 2009C2HTFH003'                                        
         DC    C'TH2CVTFL 2019C2HTFH000'                                        
         DC    C'TH2CVTFL 2023C2HTFH000'                                        
         DC    C'TH2CVTFL 2023C2HTFH009'                                        
         DC    C'TH2CVTFL 2024C2HTFH002'                                        
         DC    C'TH2POBON 7201P2HBON025'                                        
         DC    C'TH3CVCFL 2001C3HTFH000'                                        
         DC    C'TH3CVCFL 2001C3HTFH006'                                        
         DC    C'TH3CVCFL 2002C3HCFH001'                                        
         DC    C'TH3GCTFL 2001G3HGF 000'                                        
         DC    C'TH3GCTFL 2001G3HGF 002'                                        
         DC    C'TH3GCTFL 2002G3HGF 001'                                        
         DC    C'TH3POPFL 2001P3HPD 000'                                        
         DC    C'TH3POPFL 2001P3HPD 005'                                        
         DC    C'TH3POPFL 2002P3HPD 001'                                        
         DC    C'TH4CVBON 7201C4HBON000'                                        
         DC    C'TH4CVBON 7201C4HBON025'                                        
         DC    C'TH4CVCFL 2001C4HCFH000'                                        
         DC    C'TH4CVCFL 2001C4HCFH004'                                        
         DC    C'TH4CVCFL 2002C4HCFH001'                                        
         DC    C'TH4POPFL 2001P4HPD 000'                                        
         DC    C'TH4POPFL 2001P4HPD 001'                                        
         DC    C'TH4POPFL 2002P4HPD 001'                                        
         DC    C'TR1BUBON 7201B1 BON245'                                        
         DC    C'TR1BUBON 7992B1 BON026'                                        
         DC    C'TR1BUCFLR0040B1 BB 033'                                        
         DC    C'TR1BUCFLR0048B1 BB 034'                                        
         DC    C'TR1BUCFLR0049B1 BB 034'                                        
         DC    C'TR1BUCFLR2001B1 BB 035'                                        
         DC    C'TR1BUCFLR2002B1 BB 002'                                        
         DC    C'TR1BUCFLR2005B1 BB 035'                                        
         DC    C'TR1BUCFLR2006B1 BB 002'                                        
         DC    C'TR1BUCFLR2009B1 BB 035'                                        
         DC    C'TR1BUCFLR2010B1 BB 002'                                        
         DC    C'TR1BUCFLR2013B1 BB 035'                                        
         DC    C'TR1BUCFLR2014B1 BB 002'                                        
         DC    C'TR1BUCFLR2017B1 BB 035'                                        
         DC    C'TR1BUCFLR2018B1 BB 002'                                        
         DC    C'TR1BUCFLR2021B1 BB 035'                                        
         DC    C'TR1BUCFLR2022B1 BB 002'                                        
         DC    C'TR1BUCFLR2025B1 BB 035'                                        
         DC    C'TR1BUCFLR2026B1 BB 002'                                        
         DC    C'TR1BUCFLR2029B1 BB 035'                                        
         DC    C'TR1BUCFLR2030B1 BB 002'                                        
         DC    C'TR1BUCFLR2033B1 BB 035'                                        
         DC    C'TR1BUCFLR2034B1 BB 002'                                        
         DC    C'TR1BUCFLR2037B1 BB 003'                                        
         DC    C'TR1BUCFLR2038B1 BB 002'                                        
         DC    C'TR1BUCFLR2041B1 BB 035'                                        
         DC    C'TR1BUCFLR2042B1 BB 002'                                        
         DC    C'TR1BUCFLR2045B1 BB 035'                                        
         DC    C'TR1BUCFLR2046B1 BB 004'                                        
         DC    C'TR1BUCFLR2049B1 BB 035'                                        
         DC    C'TR1BUCFLR2050B1 BB 004'                                        
         DC    C'TR1BUCFLR2054B1 BB 004'                                        
         DC    C'TR1BUCFLR2057B1 BB 036'                                        
         DC    C'TR1BUCFLR2058B1 BB 004'                                        
         DC    C'TR1BUCFLR2061B1 BB 035'                                        
         DC    C'TR1BUCFLR2062B1 BB 004'                                        
         DC    C'TR1BUCFLR2065B1 BB 035'                                        
         DC    C'TR1BUCFLR2066B1 BB 004'                                        
         DC    C'TR1BUCFLR2069B1 BB 037'                                        
         DC    C'TR1BUCFLR2070B1 BB 005'                                        
         DC    C'TR1BUCFLR2074B1 BB 038'                                        
         DC    C'TR1BUCFLR2075B1 BB 039'                                        
         DC    C'TR1BUCFLR2076B1 BB 038'                                        
         DC    C'TR1BUCFLR2082B1 BB 040'                                        
         DC    C'TR1BUCFLR2085B1 BB 040'                                        
         DC    C'TR1BUCFLR2086B1 BB 040'                                        
         DC    C'TR1BUCFLR2087B1 BB 036'                                        
         DC    C'TR1BUCFLR2090B1 BB 041'                                        
         DC    C'TR1BUCFLR9012B1 BB 030'                                        
         DC    C'TR1BUCFLR9203B1 BB 024'                                        
         DC    C'TR1BUPOD 8012B1 POD022'                                        
         DC    C'TR1BUPOD 8022B1 POD021'                                        
         DC    C'TR1BUPOD 8202B1 POD248'                                        
         DC    C'TR1BUPOD 8973B1 POD029'                                        
         DC    C'TR1BUPOD 8982B1 POD023'                                        
         DC    C'TR1BUPOD 8992B1 POD028'                                        
         DC    C'TR1CABON 7022D1 BON024'                                        
         DC    C'TR1CABON 7201D1 BON245'                                        
         DC    C'TR1CACFL 2001D1 MB 005'                                        
         DC    C'TR1CACFL 2009D1 MB 005'                                        
         DC    C'TR1CACFL 2010D1 MB 001'                                        
         DC    C'TR1CACFL 2013D1 MB 005'                                        
         DC    C'TR1CACFL 2014D1 MB 001'                                        
         DC    C'TR1CACFL 2017D1 MB 006'                                        
         DC    C'TR1CACFL 2018D1 MB 001'                                        
         DC    C'TR1CACFL 2021D1 MB 005'                                        
         DC    C'TR1CACFL 2022D1 MB 001'                                        
         DC    C'TR1CACFL 2033D1 MB 006'                                        
         DC    C'TR1CACFL 2034D1 MB 001'                                        
         DC    C'TR1CACFL 2037D1 MB 005'                                        
         DC    C'TR1CACFL 2038D1 MB 001'                                        
         DC    C'TR1CACFL 2041D1 MB 005'                                        
         DC    C'TR1CACFL 2042D1 MB 001'                                        
         DC    C'TR1CACFL 2045D1 MB 005'                                        
         DC    C'TR1CACFL 2046D1 MB 001'                                        
         DC    C'TR1CACFL 2058D1 MB 001'                                        
         DC    C'TR1CACFL 2061D1 MB 005'                                        
         DC    C'TR1CACFL 2062D1 MB 001'                                        
         DC    C'TR1CACFL 2070D1 MB 001'                                        
         DC    C'TR1CACFL 2073D1 MB 007'                                        
         DC    C'TR1CACFL 2074D1 MB 005'                                        
         DC    C'TR1CACFL 2079D1 MB 005'                                        
         DC    C'TR1CACFL 9203D1 MB 028'                                        
         DC    C'TR1CAPOD 8012D1 POD022'                                        
         DC    C'TR1CAPOD 8022D1 POD023'                                        
         DC    C'TR1CAPOD 8973D1 POD025'                                        
         DC    C'TR1CAPOD 8982D1 POD026'                                        
         DC    C'TR1CAPOD 8992D1 POD027'                                        
         DC    C'TR1CVBON 7201C1 BON245'                                        
         DC    C'TR1CVCFL 0209C1 CF 001'                                        
         DC    C'TR1CVCFL 0211C1 CF 002'                                        
         DC    C'TR1CVCFL 2001C1 CF 090'                                        
         DC    C'TR1CVCFL 2002C1 CF 003'                                        
         DC    C'TR1CVCFL 2003C1 CF 090'                                        
         DC    C'TR1CVCFL 2004C1 CF 004'                                        
         DC    C'TR1CVCFL 2005C1 CF 090'                                        
         DC    C'TR1CVCFL 2006C1 CF 005'                                        
         DC    C'TR1CVCFL 2007C1 CF 090'                                        
         DC    C'TR1CVCFL 2008C1 CF 004'                                        
         DC    C'TR1CVCFL 2009C1 CF 090'                                        
         DC    C'TR1CVCFL 2010C1 CF 005'                                        
         DC    C'TR1CVCFL 2011C1 CF 090'                                        
         DC    C'TR1CVCFL 2012C1 CF 005'                                        
         DC    C'TR1CVCFL 2013C1 CF 090'                                        
         DC    C'TR1CVCFL 2014C1 CF 005'                                        
         DC    C'TR1CVCFL 2015C1 CF 090'                                        
         DC    C'TR1CVCFL 2016C1 CF 005'                                        
         DC    C'TR1CVCFL 2017C1 CF 090'                                        
         DC    C'TR1CVCFL 2018C1 CF 005'                                        
         DC    C'TR1CVCFL 2019C1 CF 090'                                        
         DC    C'TR1CVCFL 2020C1 CF 006'                                        
         DC    C'TR1CVCFL 2021C1 CF 090'                                        
         DC    C'TR1CVCFL 2022C1 CF 007'                                        
         DC    C'TR1CVCFL 2023C1 CF 090'                                        
         DC    C'TR1CVCFL 2024C1 CF 007'                                        
         DC    C'TR1CVCFL 2025C1 CF 090'                                        
         DC    C'TR1CVCFL 2026C1 CF 004'                                        
         DC    C'TR1CVCFL 2027C1 CF 090'                                        
         DC    C'TR1CVCFL 2028C1 CF 006'                                        
         DC    C'TR1CVCFL 2029C1 CF 090'                                        
         DC    C'TR1CVCFL 2030C1 CF 004'                                        
         DC    C'TR1CVCFL 2031C1 CF 090'                                        
         DC    C'TR1CVCFL 2032C1 CF 006'                                        
         DC    C'TR1CVCFL 2033C1 CF 090'                                        
         DC    C'TR1CVCFL 2034C1 CF 005'                                        
         DC    C'TR1CVCFL 2035C1 CF 090'                                        
         DC    C'TR1CVCFL 2036C1 CF 005'                                        
         DC    C'TR1CVCFL 2037C1 CF 090'                                        
         DC    C'TR1CVCFL 2038C1 CF 006'                                        
         DC    C'TR1CVCFL 2039C1 CF 090'                                        
         DC    C'TR1CVCFL 2040C1 CF 005'                                        
         DC    C'TR1CVCFL 2041C1 CF 090'                                        
         DC    C'TR1CVCFL 2042C1 CF 006'                                        
         DC    C'TR1CVCFL 2043C1 CF 090'                                        
         DC    C'TR1CVCFL 2044C1 CF 005'                                        
         DC    C'TR1CVCFL 2045C1 CF 090'                                        
         DC    C'TR1CVCFL 2046C1 CF 004'                                        
         DC    C'TR1CVCFL 2047C1 CF 090'                                        
         DC    C'TR1CVCFL 2048C1 CF 003'                                        
         DC    C'TR1CVCFL 2049C1 CF 090'                                        
         DC    C'TR1CVCFL 2050C1 CF 008'                                        
         DC    C'TR1CVCFL 2051C1 CF 090'                                        
         DC    C'TR1CVCFL 2052C1 CF 004'                                        
         DC    C'TR1CVCFL 2053C1 CF 090'                                        
         DC    C'TR1CVCFL 2054C1 CF 007'                                        
         DC    C'TR1CVCFL 2055C1 CF 090'                                        
         DC    C'TR1CVCFL 2056C1 CF 005'                                        
         DC    C'TR1CVCFL 2057C1 CF 090'                                        
         DC    C'TR1CVCFL 2058C1 CF 006'                                        
         DC    C'TR1CVCFL 2059C1 CF 090'                                        
         DC    C'TR1CVCFL 2060C1 CF 005'                                        
         DC    C'TR1CVCFL 2061C1 CF 090'                                        
         DC    C'TR1CVCFL 2062C1 CF 003'                                        
         DC    C'TR1CVCFL 2063C1 CF 090'                                        
         DC    C'TR1CVCFL 2064C1 CF 004'                                        
         DC    C'TR1CVCFL 2065C1 CF 090'                                        
         DC    C'TR1CVCFL 2066C1 CF 005'                                        
         DC    C'TR1CVCFL 2067C1 CF 090'                                        
         DC    C'TR1CVCFL 2068C1 CF 006'                                        
         DC    C'TR1CVCFL 2069C1 CF 081'                                        
         DC    C'TR1CVCFL 2070C1 CF 006'                                        
         DC    C'TR1CVCFL 2071C1 CF 090'                                        
         DC    C'TR1CVCFL 2072C1 CF 006'                                        
         DC    C'TR1CVCFL 2073C1 CF 090'                                        
         DC    C'TR1CVCFL 2074C1 CF 008'                                        
         DC    C'TR1CVCFL 2075C1 CF 090'                                        
         DC    C'TR1CVCFL 2076C1 CF 006'                                        
         DC    C'TR1CVCFL 2077C1 CF 090'                                        
         DC    C'TR1CVCFL 2078C1 CF 005'                                        
         DC    C'TR1CVCFL 2079C1 CF 090'                                        
         DC    C'TR1CVCFL 2080C1 CF 004'                                        
         DC    C'TR1CVCFL 2081C1 CF 090'                                        
         DC    C'TR1CVCFL 2082C1 CF 004'                                        
         DC    C'TR1CVCFL 2083C1 CF 090'                                        
         DC    C'TR1CVCFL 2084C1 CF 019'                                        
         DC    C'TR1CVCFL 2085C1 CF 011'                                        
         DC    C'TR1CVCFL 2085C1 CF 090'                                        
         DC    C'TR1CVCFL 2086C1 CF 005'                                        
         DC    C'TR1CVCFL 2087C1 CF 090'                                        
         DC    C'TR1CVCFL 2088C1 CF 004'                                        
         DC    C'TR1CVCFL 2089C1 CF 090'                                        
         DC    C'TR1CVCFL 2090C1 CF 005'                                        
         DC    C'TR1CVCFL 2093C1 CF 080'                                        
         DC    C'TR1CVCFL 2372C1 CF 080'                                        
         DC    C'TR1CVCFL 2373C1 CF 080'                                        
         DC    C'TR1CVCFL 2374C1 CF 080'                                        
         DC    C'TR1CVCFL 2375C1 CF 080'                                        
         DC    C'TR1CVCFL 2376C1 CF 080'                                        
         DC    C'TR1CVCFL 2377C1 CF 080'                                        
         DC    C'TR1CVCFL 2378C1 CF 088'                                        
         DC    C'TR1CVCFL 9203C1 CF 024'                                        
         DC    C'TR1CVCFL 9702C1 CF 033'                                        
         DC    C'TR1CVIMP 9012C1 CI 022'                                        
         DC    C'TR1CVPOD 8012C1 POD032'                                        
         DC    C'TR1CVPOD 8022C1 POD023'                                        
         DC    C'TR1CVPOD 8202C1 POD029'                                        
         DC    C'TR1CVPOD 8973C1 POD030'                                        
         DC    C'TR1CVPOD 8982C1 POD026'                                        
         DC    C'TR1CVPOD 8992C1 POD031'                                        
         DC    C'TR1CVSIL 2146C1 TS 004'                                        
         DC    C'TR1CVSIL 2152C1 TS 004'                                        
         DC    C'TR1CVTFL 2091C1 TF 083'                                        
         DC    C'TR1CVTFL 2095C1 TF 082'                                        
         DC    C'TR1CVTFL 2096C1 TF 085'                                        
         DC    C'TR1CVTFL 2099C1 TF 084'                                        
         DC    C'TR1CVTFL 2101C1 TF 086'                                        
         DC    C'TR1CVTFL 2102C1 TF 003'                                        
         DC    C'TR1CVTFL 2103C1 TF 086'                                        
         DC    C'TR1CVTFL 2104C1 TF 004'                                        
         DC    C'TR1CVTFL 2105C1 TF 086'                                        
         DC    C'TR1CVTFL 2106C1 TF 005'                                        
         DC    C'TR1CVTFL 2107C1 TF 086'                                        
         DC    C'TR1CVTFL 2108C1 TF 004'                                        
         DC    C'TR1CVTFL 2109C1 TF 086'                                        
         DC    C'TR1CVTFL 2110C1 TF 005'                                        
         DC    C'TR1CVTFL 2111C1 TF 086'                                        
         DC    C'TR1CVTFL 2112C1 TF 005'                                        
         DC    C'TR1CVTFL 2113C1 TF 086'                                        
         DC    C'TR1CVTFL 2114C1 TF 005'                                        
         DC    C'TR1CVTFL 2115C1 TF 086'                                        
         DC    C'TR1CVTFL 2116C1 TF 005'                                        
         DC    C'TR1CVTFL 2117C1 TF 086'                                        
         DC    C'TR1CVTFL 2118C1 TF 005'                                        
         DC    C'TR1CVTFL 2119C1 TF 086'                                        
         DC    C'TR1CVTFL 2120C1 TF 006'                                        
         DC    C'TR1CVTFL 2121C1 TF 086'                                        
         DC    C'TR1CVTFL 2122C1 TF 007'                                        
         DC    C'TR1CVTFL 2123C1 TF 091'                                        
         DC    C'TR1CVTFL 2124C1 TF 006'                                        
         DC    C'TR1CVTFL 2125C1 TF 086'                                        
         DC    C'TR1CVTFL 2126C1 TF 004'                                        
         DC    C'TR1CVTFL 2127C1 TF 086'                                        
         DC    C'TR1CVTFL 2128C1 TF 004'                                        
         DC    C'TR1CVTFL 2129C1 TF 086'                                        
         DC    C'TR1CVTFL 2130C1 TF 004'                                        
         DC    C'TR1CVTFL 2131C1 TF 086'                                        
         DC    C'TR1CVTFL 2132C1 TF 006'                                        
         DC    C'TR1CVTFL 2133C1 TF 086'                                        
         DC    C'TR1CVTFL 2134C1 TF 005'                                        
         DC    C'TR1CVTFL 2135C1 TF 086'                                        
         DC    C'TR1CVTFL 2136C1 TF 005'                                        
         DC    C'TR1CVTFL 2137C1 TF 086'                                        
         DC    C'TR1CVTFL 2138C1 TF 005'                                        
         DC    C'TR1CVTFL 2139C1 TF 086'                                        
         DC    C'TR1CVTFL 2140C1 TF 005'                                        
         DC    C'TR1CVTFL 2141C1 TF 086'                                        
         DC    C'TR1CVTFL 2142C1 TF 006'                                        
         DC    C'TR1CVTFL 2143C1 TF 086'                                        
         DC    C'TR1CVTFL 2144C1 TF 005'                                        
         DC    C'TR1CVTFL 2145C1 TF 086'                                        
         DC    C'TR1CVTFL 2146C1 TF 003'                                        
         DC    C'TR1CVTFL 2147C1 TF 086'                                        
         DC    C'TR1CVTFL 2148C1 TF 003'                                        
         DC    C'TR1CVTFL 2149C1 TF 086'                                        
         DC    C'TR1CVTFL 2150C1 TF 008'                                        
         DC    C'TR1CVTFL 2151C1 TF 086'                                        
         DC    C'TR1CVTFL 2152C1 TF 003'                                        
         DC    C'TR1CVTFL 2153C1 TF 091'                                        
         DC    C'TR1CVTFL 2154C1 TF 015'                                        
         DC    C'TR1CVTFL 2155C1 TF 086'                                        
         DC    C'TR1CVTFL 2156C1 TF 005'                                        
         DC    C'TR1CVTFL 2157C1 TF 086'                                        
         DC    C'TR1CVTFL 2158C1 TF 006'                                        
         DC    C'TR1CVTFL 2159C1 TF 086'                                        
         DC    C'TR1CVTFL 2160C1 TF 005'                                        
         DC    C'TR1CVTFL 2161C1 TF 086'                                        
         DC    C'TR1CVTFL 2162C1 TF 003'                                        
         DC    C'TR1CVTFL 2163C1 TF 086'                                        
         DC    C'TR1CVTFL 2164C1 TF 004'                                        
         DC    C'TR1CVTFL 2165C1 TF 086'                                        
         DC    C'TR1CVTFL 2166C1 TF 005'                                        
         DC    C'TR1CVTFL 2167C1 TF 086'                                        
         DC    C'TR1CVTFL 2168C1 TF 005'                                        
         DC    C'TR1CVTFL 2169C1 TF 086'                                        
         DC    C'TR1CVTFL 2170C1 TF 006'                                        
         DC    C'TR1CVTFL 2171C1 TF 086'                                        
         DC    C'TR1CVTFL 2172C1 TF 007'                                        
         DC    C'TR1CVTFL 2173C1 TF 086'                                        
         DC    C'TR1CVTFL 2174C1 TF 006'                                        
         DC    C'TR1CVTFL 2175C1 TF 086'                                        
         DC    C'TR1CVTFL 2176C1 TF 006'                                        
         DC    C'TR1CVTFL 2177C1 TF 086'                                        
         DC    C'TR1CVTFL 2178C1 TF 005'                                        
         DC    C'TR1CVTFL 2179C1 TF 086'                                        
         DC    C'TR1CVTFL 2180C1 TF 004'                                        
         DC    C'TR1CVTFL 2181C1 TF 086'                                        
         DC    C'TR1CVTFL 2182C1 TF 004'                                        
         DC    C'TR1CVTFL 2183C1 TF 091'                                        
         DC    C'TR1CVTFL 2184C1 TF 015'                                        
         DC    C'TR1CVTFL 2185C1 TF 086'                                        
         DC    C'TR1CVTFL 2186C1 TF 005'                                        
         DC    C'TR1CVTFL 2187C1 TF 086'                                        
         DC    C'TR1CVTFL 2188C1 TF 004'                                        
         DC    C'TR1CVTFL 2189C1 TF 086'                                        
         DC    C'TR1CVTFL 2190C1 TF 005'                                        
         DC    C'TR1CVTFL 2283C1 TF 087'                                        
         DC    C'TR1CVTFL 9012C1 CI 022'                                        
         DC    C'TR1CVTFL 9012C1 TF 022'                                        
         DC    C'TR1CVTFL 9203C1 CF 024'                                        
         DC    C'TR1GCBON 7201G1 BON245'                                        
         DC    C'TR1GCBON 7202G1 BON029'                                        
         DC    C'TR1GCPOD 8012G1 POD022'                                        
         DC    C'TR1GCPOD 8022G1 POD023'                                        
         DC    C'TR1GCPOD 8202G1 POD024'                                        
         DC    C'TR1GCPOD 8973G1 POD027'                                        
         DC    C'TR1GCPOD 8982G1 POD026'                                        
         DC    C'TR1GCPOD 8992G1 POD021'                                        
         DC    C'TR1GCTFL 2001G1 GF 005'                                        
         DC    C'TR1GCTFL 2002G1 GF 001'                                        
         DC    C'TR1GCTFL 2004G1 GF 001'                                        
         DC    C'TR1GCTFL 2005G1 GF 005'                                        
         DC    C'TR1GCTFL 2006G1 GF 001'                                        
         DC    C'TR1GCTFL 2007G1 GF 005'                                        
         DC    C'TR1GCTFL 2008G1 GF 001'                                        
         DC    C'TR1GCTFL 2009G1 GF 005'                                        
         DC    C'TR1GCTFL 2010G1 GF 001'                                        
         DC    C'TR1GCTFL 2011G1 GF 005'                                        
         DC    C'TR1GCTFL 2012G1 GF 001'                                        
         DC    C'TR1GCTFL 2013G1 GF 005'                                        
         DC    C'TR1GCTFL 2014G1 GF 001'                                        
         DC    C'TR1GCTFL 2015G1 GF 002'                                        
         DC    C'TR1GCTFL 2016G1 GF 001'                                        
         DC    C'TR1GCTFL 2017G1 GF 005'                                        
         DC    C'TR1GCTFL 2018G1 GF 001'                                        
         DC    C'TR1GCTFL 2019G1 GF 005'                                        
         DC    C'TR1GCTFL 2020G1 GF 001'                                        
         DC    C'TR1GCTFL 2021G1 GF 005'                                        
         DC    C'TR1GCTFL 2022G1 GF 001'                                        
         DC    C'TR1GCTFL 2023G1 GF 005'                                        
         DC    C'TR1GCTFL 2024G1 GF 001'                                        
         DC    C'TR1GCTFL 2025G1 GF 005'                                        
         DC    C'TR1GCTFL 2026G1 GF 001'                                        
         DC    C'TR1GCTFL 2027G1 GF 005'                                        
         DC    C'TR1GCTFL 2028G1 GF 001'                                        
         DC    C'TR1GCTFL 2029G1 GF 005'                                        
         DC    C'TR1GCTFL 2030G1 GF 001'                                        
         DC    C'TR1GCTFL 2031G1 GF 005'                                        
         DC    C'TR1GCTFL 2032G1 GF 001'                                        
         DC    C'TR1GCTFL 2033G1 GF 005'                                        
         DC    C'TR1GCTFL 2034G1 GF 001'                                        
         DC    C'TR1GCTFL 2035G1 GF 006'                                        
         DC    C'TR1GCTFL 2036G1 GF 005'                                        
         DC    C'TR1GCTFL 2037G1 GF 001'                                        
         DC    C'TR1GCTFL 2038G1 GF 001'                                        
         DC    C'TR1GCTFL 9012G1 GF 030'                                        
         DC    C'TR1GCTFL 9203G1 GF 028'                                        
         DC    C'TR1OLBON 7022O1 BON027'                                        
         DC    C'TR1OLBON 7201O1 BON245'                                        
         DC    C'TR1OLPOD 8012O1 POD022'                                        
         DC    C'TR1OLPOD 8022O1 POD023'                                        
         DC    C'TR1OLPOD 8973O1 POD029'                                        
         DC    C'TR1OLPOD 8982O1 POD028'                                        
         DC    C'TR1OLPOD 8992O1 POD025'                                        
         DC    C'TR1POBON 7201P1 BON245'                                        
         DC    C'TR1POBON 7202P1 BON030'                                        
         DC    C'TR1POBON 7992P1 BON026'                                        
         DC    C'TR1POPFL 0031P1 PD 007'                                        
         DC    C'TR1POPFL 0039P1 PD 008'                                        
         DC    C'TR1POPFL 0044P1 PD 009'                                        
         DC    C'TR1POPFL 0046P1 PD 010'                                        
         DC    C'TR1POPFL 0048P1 PD 010'                                        
         DC    C'TR1POPFL 0049P1 PD 010'                                        
         DC    C'TR1POPFL 2001P1 PD 011'                                        
         DC    C'TR1POPFL 2002P1 PD 001'                                        
         DC    C'TR1POPFL 2003P1 PD 012'                                        
         DC    C'TR1POPFL 2004P1 PD 001'                                        
         DC    C'TR1POPFL 2007P1 PD 013'                                        
         DC    C'TR1POPFL 2008P1 PD 011'                                        
         DC    C'TR1POPFL 2009P1 PD 001'                                        
         DC    C'TR1POPFL 2010P1 PD 011'                                        
         DC    C'TR1POPFL 2011P1 PD 001'                                        
         DC    C'TR1POPFL 2012P1 PD 011'                                        
         DC    C'TR1POPFL 2013P1 PD 001'                                        
         DC    C'TR1POPFL 2014P1 PD 011'                                        
         DC    C'TR1POPFL 2015P1 PD 001'                                        
         DC    C'TR1POPFL 2016P1 PD 011'                                        
         DC    C'TR1POPFL 2017P1 PD 001'                                        
         DC    C'TR1POPFL 2018P1 PD 011'                                        
         DC    C'TR1POPFL 2019P1 PD 001'                                        
         DC    C'TR1POPFL 2020P1 PD 011'                                        
         DC    C'TR1POPFL 2021P1 PD 002'                                        
         DC    C'TR1POPFL 2022P1 PD 011'                                        
         DC    C'TR1POPFL 2023P1 PD 002'                                        
         DC    C'TR1POPFL 2025P1 PD 002'                                        
         DC    C'TR1POPFL 2026P1 PD 011'                                        
         DC    C'TR1POPFL 2028P1 PD 011'                                        
         DC    C'TR1POPFL 2030P1 PD 011'                                        
         DC    C'TR1POPFL 2034P1 PD 011'                                        
         DC    C'TR1POPFL 2035P1 PD 002'                                        
         DC    C'TR1POPFL 2036P1 PD 014'                                        
         DC    C'TR1POPFL 2191P1 PD 004'                                        
         DC    C'TR1POPFL 2230P1 PD 003'                                        
         DC    C'TR1POPFL 9012P1 PD 031'                                        
         DC    C'TR1POPFL 9203P1 PD 024'                                        
         DC    C'TR1POPOD 8012P1 POD022'                                        
         DC    C'TR1POPOD 8022P1 POD023'                                        
         DC    C'TR1POPOD 8202P1 POD021'                                        
         DC    C'TR1POPOD 8973P1 POD027'                                        
         DC    C'TR1POPOD 8982P1 POD028'                                        
         DC    C'TR1POPOD 8992P1 POD029'                                        
         DC    C'TR2BUBON 7201B2 BON245'                                        
         DC    C'TR2BUCFLR0054B2 BB 011'                                        
         DC    C'TR2BUCFLR2001B2 BB 012'                                        
         DC    C'TR2BUCFLR2002B2 BB 002'                                        
         DC    C'TR2BUCFLR2003B2 BB 002'                                        
         DC    C'TR2BUCFLR2003B2 BB 015'                                        
         DC    C'TR2BUCFLR2004B2 BB 002'                                        
         DC    C'TR2BUCFLR2005B2 BB 012'                                        
         DC    C'TR2BUCFLR2006B2 BB 002'                                        
         DC    C'TR2BUCFLR2007B2 BB 012'                                        
         DC    C'TR2BUCFLR2008B2 BB 002'                                        
         DC    C'TR2BUCFLR2009B2 BB 012'                                        
         DC    C'TR2BUCFLR2010B2 BB 002'                                        
         DC    C'TR2BUCFLR2011B2 BB 012'                                        
         DC    C'TR2BUCFLR2012B2 BB 002'                                        
         DC    C'TR2BUCFLR2013B2 BB 012'                                        
         DC    C'TR2BUCFLR2014B2 BB 002'                                        
         DC    C'TR2BUCFLR2015B2 BB 012'                                        
         DC    C'TR2BUCFLR2016B2 BB 002'                                        
         DC    C'TR2BUCFLR2017B2 BB 013'                                        
         DC    C'TR2BUCFLR2018B2 BB 003'                                        
         DC    C'TR2BUCFLR2019B2 BB 013'                                        
         DC    C'TR2BUCFLR2020B2 BB 003'                                        
         DC    C'TR2BUCFLR2021B2 BB 013'                                        
         DC    C'TR2BUCFLR2022B2 BB 003'                                        
         DC    C'TR2BUCFLR2023B2 BB 013'                                        
         DC    C'TR2BUCFLR2024B2 BB 003'                                        
         DC    C'TR2BUCFLR2025B2 BB 013'                                        
         DC    C'TR2BUCFLR2026B2 BB 003'                                        
         DC    C'TR2BUCFLR2027B2 BB 013'                                        
         DC    C'TR2BUCFLR2028B2 BB 003'                                        
         DC    C'TR2BUCFLR2029B2 BB 013'                                        
         DC    C'TR2BUCFLR2030B2 BB 003'                                        
         DC    C'TR2BUCFLR2031B2 BB 013'                                        
         DC    C'TR2BUCFLR2032B2 BB 003'                                        
         DC    C'TR2BUCFLR2038B2 BB 002'                                        
         DC    C'TR2BUCFLR2042B2 BB 002'                                        
         DC    C'TR2BUCFLR2046B2 BB 002'                                        
         DC    C'TR2BUCFLR2050B2 BB 002'                                        
         DC    C'TR2BUCFLR2081B2 BB 013'                                        
         DC    C'TR2BUCFLR2082B2 BB 003'                                        
         DC    C'TR2BUCFLR2085B2 BB 013'                                        
         DC    C'TR2BUCFLR2086B2 BB 003'                                        
         DC    C'TR2BUCFLR2089B2 BB 012'                                        
         DC    C'TR2BUCFLR2090B2 BB 003'                                        
         DC    C'TR2BUCFLR2093B2 BB 012'                                        
         DC    C'TR2BUCFLR2094B2 BB 003'                                        
         DC    C'TR2BUCFLR2098B2 BB 003'                                        
         DC    C'TR2BUCFLR2102B2 BB 003'                                        
         DC    C'TR2BUCFLR2106B2 BB 003'                                        
         DC    C'TR2BUCFLR2109B2 BB 013'                                        
         DC    C'TR2BUCFLR2110B2 BB 003'                                        
         DC    C'TR2BUCFLR2113B2 BB 013'                                        
         DC    C'TR2BUCFLR2114B2 BB 003'                                        
         DC    C'TR2BUCFLR2118B2 BB 003'                                        
         DC    C'TR2BUCFLR2121B2 BB 014'                                        
         DC    C'TR2BUCFLR2122B2 BB 013'                                        
         DC    C'TR2BUCFLR2123B2 BB 013'                                        
         DC    C'TR2BUCFLR2124B2 BB 013'                                        
         DC    C'TR2BUCFLR2162B2 BB 003'                                        
         DC    C'TR2BUCFLR2164B2 BB 013'                                        
         DC    C'TR2BUCFLR9012B2 BB 022'                                        
         DC    C'TR2BUCFLR9022B2 BB 023'                                        
         DC    C'TR2BUCFLR9203B2 BB 024'                                        
         DC    C'TR2BUCFLR9802B2 BB 030'                                        
         DC    C'TR2BUCFLR9996B2 BB 027'                                        
         DC    C'TR2BUPOD 8012B2 POD028'                                        
         DC    C'TR2BUPOD 8022B2 POD029'                                        
         DC    C'TR2BUPOD 8202B2 POD248'                                        
         DC    C'TR2BUPOD 8992B2 POD026'                                        
         DC    C'TR2CABON 7201D2 BON245'                                        
         DC    C'TR2CACFL 0024D2 MB 007'                                        
         DC    C'TR2CACFL 2001D2 MB 008'                                        
         DC    C'TR2CACFL 2005D2 MB 009'                                        
         DC    C'TR2CACFL 2009D2 MB 009'                                        
         DC    C'TR2CACFL 2010D2 MB 001'                                        
         DC    C'TR2CACFL 2013D2 MB 009'                                        
         DC    C'TR2CACFL 2014D2 MB 001'                                        
         DC    C'TR2CACFL 2017D2 MB 009'                                        
         DC    C'TR2CACFL 2018D2 MB 001'                                        
         DC    C'TR2CACFL 2021D2 MB 009'                                        
         DC    C'TR2CACFL 2022D2 MB 001'                                        
         DC    C'TR2CACFL 2025D2 MB 009'                                        
         DC    C'TR2CACFL 2026D2 MB 001'                                        
         DC    C'TR2CACFL 2029D2 MB 009'                                        
         DC    C'TR2CACFL 2030D2 MB 001'                                        
         DC    C'TR2CACFL 2033D2 MB 009'                                        
         DC    C'TR2CACFL 2034D2 MB 001'                                        
         DC    C'TR2CACFL 2037D2 MB 009'                                        
         DC    C'TR2CACFL 2038D2 MB 001'                                        
         DC    C'TR2CACFL 2041D2 MB 009'                                        
         DC    C'TR2CACFL 2042D2 MB 001'                                        
         DC    C'TR2CACFL 2045D2 MB 009'                                        
         DC    C'TR2CACFL 2046D2 MB 001'                                        
         DC    C'TR2CACFL 2086D2 MB 001'                                        
         DC    C'TR2CACFL 2090D2 MB 010'                                        
         DC    C'TR2CACFL 2091D2 MB 009'                                        
         DC    C'TR2CACFL 2092D2 MB 003'                                        
         DC    C'TR2CACFL 2098D2 MB 004'                                        
         DC    C'TR2CACFL 2464D2 MB 002'                                        
         DC    C'TR2CACFL 9203D2 MB 024'                                        
         DC    C'TR2CACFL 9996D2 MB 028'                                        
         DC    C'TR2CAPOD 8012D2 POD022'                                        
         DC    C'TR2CAPOD 8022D2 POD023'                                        
         DC    C'TR2CAPOD 8202D2 POD248'                                        
         DC    C'TR2CAPOD 8982D2 POD026'                                        
         DC    C'TR2CAPOD 8992D2 POD027'                                        
         DC    C'TR2CVBON 7201C2 BON025'                                        
         DC    C'TR2CVBON 7201C2 BON245'                                        
         DC    C'TR2CVCAV 2004C2 CA 002'                                        
         DC    C'TR2CVCAV 9012C2 CA 027'                                        
         DC    C'TR2CVCAV 9203C2 CA 024'                                        
         DC    C'TR2CVCFL 0125C2 CF 016'                                        
         DC    C'TR2CVCFL 0126C2 CF 043'                                        
         DC    C'TR2CVCFL 2033C2 CF 038'                                        
         DC    C'TR2CVCFL 2054C2 CF 041'                                        
         DC    C'TR2CVCFL 2065C2 CF 009'                                        
         DC    C'TR2CVCFL 2071C2 CF 018'                                        
         DC    C'TR2CVCFL 9203C2 CF 024'                                        
         DC    C'TR2CVIMP 2006C2 CF 032'                                        
         DC    C'TR2CVIMP 2007C2 CI 002'                                        
         DC    C'TR2CVIMP 9012C2 CI 027'                                        
         DC    C'TR2CVIMP 9203C2 CI 024'                                        
         DC    C'TR2CVMAL 2010C2 CF 032'                                        
         DC    C'TR2CVMAL 2011C2 CB 002'                                        
         DC    C'TR2CVMAL 9012C2 CB 027'                                        
         DC    C'TR2CVMAL 9203C2 CB 024'                                        
         DC    C'TR2CVPOD 8012C2 POD022'                                        
         DC    C'TR2CVPOD 8022C2 POD023'                                        
         DC    C'TR2CVPOD 8202C2 POD248'                                        
         DC    C'TR2CVPOD 8973C2 POD028'                                        
         DC    C'TR2CVPOD 8982C2 POD029'                                        
         DC    C'TR2CVPOD 8992C2 POD026'                                        
         DC    C'TR2CVSIL 2020C2 TS 038'                                        
         DC    C'TR2CVSIL 2021C2 TS 039'                                        
         DC    C'TR2CVSIL 2022C2 TS 002'                                        
         DC    C'TR2CVSIL 9203C2 TS 024'                                        
         DC    C'TR2CVSUB 2025C2 TU 039'                                        
         DC    C'TR2CVSUB 2026C2 TU 002'                                        
         DC    C'TR2CVSUB 2042C2 SUB002'                                        
         DC    C'TR2CVSUB 9203C2 TU 024'                                        
         DC    C'TR2CVS1P 2031C2 TP 039'                                        
         DC    C'TR2CVS1P 9203C2 TP 024'                                        
         DC    C'TR2CVTAH 2027C2 TH 039'                                        
         DC    C'TR2CVTAH 2028C2 TH 002'                                        
         DC    C'TR2CVTAH 2040C2 TH 002'                                        
         DC    C'TR2CVTAH 9203C2 TH 024'                                        
         DC    C'TR2CVTBL 2029C2 TZ 039'                                        
         DC    C'TR2CVTFL 0123C2 TF 001'                                        
         DC    C'TR2CVTFL 0208C2 TF 037'                                        
         DC    C'TR2CVTFL 2015C2 TF 006'                                        
         DC    C'TR2CVTFL 2035C2 TF 040'                                        
         DC    C'TR2CVTFL 2038C2 TF 004'                                        
         DC    C'TR2CVTFL 2043C2 TF 010'                                        
         DC    C'TR2CVTFL 2067C2 TF 011'                                        
         DC    C'TR2CVTFL 2067C2 TF 042'                                        
         DC    C'TR2CVTFL 2068C2 TF 008'                                        
         DC    C'TR2CVTFL 2070C2 TF 044'                                        
         DC    C'TR2CVTFL 9012C2 TF 027'                                        
         DC    C'TR2CVTFL 9203C2 TF 024'                                        
         DC    C'TR2CVTFL 9996C2 TF 243'                                        
         DC    C'TR2GCBON 7201G2 BON245'                                        
         DC    C'TR2GCBON 7973G2 BON029'                                        
         DC    C'TR2GCPOD 8012G2 POD022'                                        
         DC    C'TR2GCPOD 8022G2 POD023'                                        
         DC    C'TR2GCPOD 8202G2 POD248'                                        
         DC    C'TR2GCPOD 8982G2 POD030'                                        
         DC    C'TR2GCPOD 8992G2 POD026'                                        
         DC    C'TR2GCTFL 2001G2 GF 004'                                        
         DC    C'TR2GCTFL 2003G2 GF 004'                                        
         DC    C'TR2GCTFL 2004G2 GF 001'                                        
         DC    C'TR2GCTFL 2005G2 GF 004'                                        
         DC    C'TR2GCTFL 2006G2 GF 001'                                        
         DC    C'TR2GCTFL 2007G2 GF 004'                                        
         DC    C'TR2GCTFL 2008G2 GF 001'                                        
         DC    C'TR2GCTFL 2009G2 GF 004'                                        
         DC    C'TR2GCTFL 2010G2 GF 001'                                        
         DC    C'TR2GCTFL 2011G2 GF 004'                                        
         DC    C'TR2GCTFL 2012G2 GF 001'                                        
         DC    C'TR2GCTFL 2013G2 GF 004'                                        
         DC    C'TR2GCTFL 2014G2 GF 001'                                        
         DC    C'TR2GCTFL 2015G2 GF 004'                                        
         DC    C'TR2GCTFL 2016G2 GF 001'                                        
         DC    C'TR2GCTFL 2017G2 GF 004'                                        
         DC    C'TR2GCTFL 2018G2 GF 001'                                        
         DC    C'TR2GCTFL 2019G2 GF 004'                                        
         DC    C'TR2GCTFL 2020G2 GF 001'                                        
         DC    C'TR2GCTFL 2021G2 GF 004'                                        
         DC    C'TR2GCTFL 2022G2 GF 001'                                        
         DC    C'TR2GCTFL 2023G2 GF 004'                                        
         DC    C'TR2GCTFL 2024G2 GF 001'                                        
         DC    C'TR2GCTFL 2025G2 GF 004'                                        
         DC    C'TR2GCTFL 2026G2 GF 001'                                        
         DC    C'TR2GCTFL 2027G2 GF 004'                                        
         DC    C'TR2GCTFL 2028G2 GF 001'                                        
         DC    C'TR2GCTFL 2029G2 GF 004'                                        
         DC    C'TR2GCTFL 2031G2 GF 004'                                        
         DC    C'TR2GCTFL 2032G2 GF 001'                                        
         DC    C'TR2GCTFL 2033G2 GF 004'                                        
         DC    C'TR2GCTFL 2034G2 GF 001'                                        
         DC    C'TR2GCTFL 2035G2 GF 004'                                        
         DC    C'TR2GCTFL 2036G2 GF 001'                                        
         DC    C'TR2GCTFL 2037G2 GF 005'                                        
         DC    C'TR2GCTFL 2038G2 GF 006'                                        
         DC    C'TR2GCTFL 2039G2 GF 004'                                        
         DC    C'TR2GCTFL 9012G2 GF 027'                                        
         DC    C'TR2GCTFL 9022G2 GF 028'                                        
         DC    C'TR2GCTFL 9203G2 GF 024'                                        
         DC    C'TR2GCTFL 9996G2 GF 031'                                        
         DC    C'TR2OLBON 7201O2 BON245'                                        
         DC    C'TR2OLCFL 9012O2 LF 022'                                        
         DC    C'TR2OLPOD 8012O2 POD024'                                        
         DC    C'TR2OLPOD 8022O2 POD023'                                        
         DC    C'TR2OLPOD 8992O2 POD026'                                        
         DC    C'TR2POBON 7201P2 BON245'                                        
         DC    C'TR2POPFL 0028P2 PD 008'                                        
         DC    C'TR2POPFL 2001P2 PD 009'                                        
         DC    C'TR2POPFL 2002P2 PD 001'                                        
         DC    C'TR2POPFL 2003P2 PD 010'                                        
         DC    C'TR2POPFL 2004P2 PD 001'                                        
         DC    C'TR2POPFL 2005P2 PD 010'                                        
         DC    C'TR2POPFL 2006P2 PD 010'                                        
         DC    C'TR2POPFL 2007P2 PD 010'                                        
         DC    C'TR2POPFL 2008P2 PD 001'                                        
         DC    C'TR2POPFL 2009P2 PD 010'                                        
         DC    C'TR2POPFL 2010P2 PD 001'                                        
         DC    C'TR2POPFL 2011P2 PD 010'                                        
         DC    C'TR2POPFL 2012P2 PD 001'                                        
         DC    C'TR2POPFL 2013P2 PD 010'                                        
         DC    C'TR2POPFL 2014P2 PD 001'                                        
         DC    C'TR2POPFL 2015P2 PD 010'                                        
         DC    C'TR2POPFL 2016P2 PD 001'                                        
         DC    C'TR2POPFL 2017P2 PD 010'                                        
         DC    C'TR2POPFL 2018P2 PD 001'                                        
         DC    C'TR2POPFL 2019P2 PD 010'                                        
         DC    C'TR2POPFL 2020P2 PD 001'                                        
         DC    C'TR2POPFL 2021P2 PD 010'                                        
         DC    C'TR2POPFL 2022P2 PD 001'                                        
         DC    C'TR2POPFL 2023P2 PD 011'                                        
         DC    C'TR2POPFL 2024P2 PD 001'                                        
         DC    C'TR2POPFL 2026P2 PD 010'                                        
         DC    C'TR2POPFL 2027P2 PD 001'                                        
         DC    C'TR2POPFL 9012P2 PD 022'                                        
         DC    C'TR2POPFL 9022P2 PD 023'                                        
         DC    C'TR2POPFL 9203P2 PD 024'                                        
         DC    C'TR2POPFL 9996P2 PD 030'                                        
         DC    C'TR2POPOD 8012P2 POD029'                                        
         DC    C'TR2POPOD 8022P2 POD028'                                        
         DC    C'TR2POPOD 8973P2 POD027'                                        
         DC    C'TR2POPOD 8982P2 POD031'                                        
         DC    C'TR2POPOD 8992P2 POD026'                                        
         DC    C'TR3BUBON 7201B3 BON025'                                        
         DC    C'TR3BUCFLR0088B3 BB 010'                                        
         DC    C'TR3BUCFLR2001B3 BB 011'                                        
         DC    C'TR3BUCFLR2002B3 BB 004'                                        
         DC    C'TR3BUCFLR2003B3 BB 011'                                        
         DC    C'TR3BUCFLR2004B3 BB 004'                                        
         DC    C'TR3BUCFLR2005B3 BB 011'                                        
         DC    C'TR3BUCFLR2006B3 BB 001'                                        
         DC    C'TR3BUCFLR2007B3 BB 011'                                        
         DC    C'TR3BUCFLR2008B3 BB 001'                                        
         DC    C'TR3BUCFLR2009B3 BB 011'                                        
         DC    C'TR3BUCFLR2010B3 BB 004'                                        
         DC    C'TR3BUCFLR2012B3 BB 004'                                        
         DC    C'TR3BUCFLR2013B3 BB 011'                                        
         DC    C'TR3BUCFLR2014B3 BB 001'                                        
         DC    C'TR3BUCFLR2016B3 BB 001'                                        
         DC    C'TR3BUCFLR2017B3 BB 011'                                        
         DC    C'TR3BUCFLR2018B3 BB 001'                                        
         DC    C'TR3BUCFLR2019B3 BB 011'                                        
         DC    C'TR3BUCFLR2020B3 BB 004'                                        
         DC    C'TR3BUCFLR2021B3 BB 011'                                        
         DC    C'TR3BUCFLR2022B3 BB 004'                                        
         DC    C'TR3BUCFLR2023B3 BB 011'                                        
         DC    C'TR3BUCFLR2024B3 BB 001'                                        
         DC    C'TR3BUCFLR2025B3 BB 011'                                        
         DC    C'TR3BUCFLR2026B3 BB 001'                                        
         DC    C'TR3BUCFLR2027B3 BB 011'                                        
         DC    C'TR3BUCFLR2028B3 BB 001'                                        
         DC    C'TR3BUCFLR2029B3 BB 011'                                        
         DC    C'TR3BUCFLR2030B3 BB 004'                                        
         DC    C'TR3BUCFLR2031B3 BB 011'                                        
         DC    C'TR3BUCFLR2032B3 BB 004'                                        
         DC    C'TR3BUCFLR2033B3 BB 011'                                        
         DC    C'TR3BUCFLR2034B3 BB 001'                                        
         DC    C'TR3BUCFLR2035B3 BB 011'                                        
         DC    C'TR3BUCFLR2036B3 BB 001'                                        
         DC    C'TR3BUCFLR2037B3 BB 011'                                        
         DC    C'TR3BUCFLR2038B3 BB 001'                                        
         DC    C'TR3BUCFLR2039B3 BB 011'                                        
         DC    C'TR3BUCFLR2040B3 BB 004'                                        
         DC    C'TR3BUCFLR2061B3 BB 012'                                        
         DC    C'TR3BUCFLR2062B3 BB 002'                                        
         DC    C'TR3BUCFLR2063B3 BB 012'                                        
         DC    C'TR3BUCFLR2064B3 BB 002'                                        
         DC    C'TR3BUCFLR2065B3 BB 012'                                        
         DC    C'TR3BUCFLR2066B3 BB 002'                                        
         DC    C'TR3BUCFLR2067B3 BB 012'                                        
         DC    C'TR3BUCFLR2068B3 BB 002'                                        
         DC    C'TR3BUCFLR2069B3 BB 012'                                        
         DC    C'TR3BUCFLR2070B3 BB 002'                                        
         DC    C'TR3BUCFLR2071B3 BB 012'                                        
         DC    C'TR3BUCFLR2072B3 BB 002'                                        
         DC    C'TR3BUCFLR2073B3 BB 012'                                        
         DC    C'TR3BUCFLR2074B3 BB 002'                                        
         DC    C'TR3BUCFLR2076B3 BB 002'                                        
         DC    C'TR3BUCFLR2077B3 BB 012'                                        
         DC    C'TR3BUCFLR2078B3 BB 002'                                        
         DC    C'TR3BUCFLR2079B3 BB 012'                                        
         DC    C'TR3BUCFLR2080B3 BB 002'                                        
         DC    C'TR3BUCFLR2081B3 BB 012'                                        
         DC    C'TR3BUCFLR2082B3 BB 002'                                        
         DC    C'TR3BUCFLR2083B3 BB 012'                                        
         DC    C'TR3BUCFLR2084B3 BB 002'                                        
         DC    C'TR3BUCFLR2085B3 BB 012'                                        
         DC    C'TR3BUCFLR2086B3 BB 002'                                        
         DC    C'TR3BUCFLR2087B3 BB 012'                                        
         DC    C'TR3BUCFLR2088B3 BB 002'                                        
         DC    C'TR3BUCFLR2089B3 BB 012'                                        
         DC    C'TR3BUCFLR2090B3 BB 001'                                        
         DC    C'TR3BUCFLR2091B3 BB 012'                                        
         DC    C'TR3BUCFLR2092B3 BB 002'                                        
         DC    C'TR3BUCFLR2093B3 BB 012'                                        
         DC    C'TR3BUCFLR2094B3 BB 002'                                        
         DC    C'TR3BUCFLR2095B3 BB 012'                                        
         DC    C'TR3BUCFLR2096B3 BB 002'                                        
         DC    C'TR3BUCFLR2097B3 BB 012'                                        
         DC    C'TR3BUCFLR2098B3 BB 002'                                        
         DC    C'TR3BUCFLR2099B3 BB 012'                                        
         DC    C'TR3BUCFLR2100B3 BB 002'                                        
         DC    C'TR3BUCFLR2101B3 BB 012'                                        
         DC    C'TR3BUCFLR2102B3 BB 012'                                        
         DC    C'TR3BUCFLR2103B3 BB 012'                                        
         DC    C'TR3BUCFLR2104B3 BB 012'                                        
         DC    C'TR3BUCFLR2105B3 BB 013'                                        
         DC    C'TR3BUCFLR2106B3 BB 013'                                        
         DC    C'TR3BUCFLR2107B3 BB 013'                                        
         DC    C'TR3BUCFLR2108B3 BB 013'                                        
         DC    C'TR3BUCFLR2109B3 BB 013'                                        
         DC    C'TR3BUCFLR2110B3 BB 013'                                        
         DC    C'TR3BUCFLR2112B3 BB 014'                                        
         DC    C'TR3BUCFLR2113B3 BB 014'                                        
         DC    C'TR3BUCFLR2115B3 BB 014'                                        
         DC    C'TR3BUCFLR2116B3 BB 015'                                        
         DC    C'TR3BUCFLR2176B3 BB 003'                                        
         DC    C'TR3BUCFLR2177B3 BB 003'                                        
         DC    C'TR3BUCFLR2178B3 BB 002'                                        
         DC    C'TR3BUCFLR2179B3 BB 002'                                        
         DC    C'TR3BUCFLR2180B3 BB 002'                                        
         DC    C'TR3BUCFLR2181B3 BB 002'                                        
         DC    C'TR3BUCFLR2182B3 BB 002'                                        
         DC    C'TR3BUCFLR9012B3 BB 028'                                        
         DC    C'TR3BUCFLR9022B3 BB 030'                                        
         DC    C'TR3BUCFLR9203B3 BB 024'                                        
         DC    C'TR3BUCFLR9996B3 BB 027'                                        
         DC    C'TR3BUPOD 8012B3 POD022'                                        
         DC    C'TR3BUPOD 8022B3 POD023'                                        
         DC    C'TR3BUPOD 8202B3 POD248'                                        
         DC    C'TR3BUPOD 8982B3 POD029'                                        
         DC    C'TR3BUPOD 8992B3 POD026'                                        
         DC    C'TR3CABON 7201D3 BON025'                                        
         DC    C'TR3CABON 7201D3 BON245'                                        
         DC    C'TR3CACFL 0029D3 MB 005'                                        
         DC    C'TR3CACFL 2001D3 MB 006'                                        
         DC    C'TR3CACFL 2002D3 MB 001'                                        
         DC    C'TR3CACFL 2005D3 MB 006'                                        
         DC    C'TR3CACFL 2006D3 MB 001'                                        
         DC    C'TR3CACFL 2009D3 MB 006'                                        
         DC    C'TR3CACFL 2010D3 MB 001'                                        
         DC    C'TR3CACFL 2013D3 MB 006'                                        
         DC    C'TR3CACFL 2014D3 MB 001'                                        
         DC    C'TR3CACFL 2017D3 MB 006'                                        
         DC    C'TR3CACFL 2018D3 MB 001'                                        
         DC    C'TR3CACFL 2020D3 MB 001'                                        
         DC    C'TR3CACFL 2021D3 MB 006'                                        
         DC    C'TR3CACFL 2022D3 MB 002'                                        
         DC    C'TR3CACFL 2025D3 MB 006'                                        
         DC    C'TR3CACFL 2026D3 MB 001'                                        
         DC    C'TR3CACFL 2033D3 MB 006'                                        
         DC    C'TR3CACFL 2034D3 MB 001'                                        
         DC    C'TR3CACFL 2037D3 MB 006'                                        
         DC    C'TR3CACFL 2038D3 MB 001'                                        
         DC    C'TR3CACFL 2041D3 MB 006'                                        
         DC    C'TR3CACFL 2042D3 MB 001'                                        
         DC    C'TR3CACFL 2045D3 MB 006'                                        
         DC    C'TR3CACFL 2046D3 MB 001'                                        
         DC    C'TR3CACFL 2049D3 MB 006'                                        
         DC    C'TR3CACFL 2050D3 MB 001'                                        
         DC    C'TR3CACFL 2061D3 MB 006'                                        
         DC    C'TR3CACFL 2062D3 MB 001'                                        
         DC    C'TR3CACFL 2065D3 MB 006'                                        
         DC    C'TR3CACFL 2066D3 MB 001'                                        
         DC    C'TR3CACFL 2103D3 MB 006'                                        
         DC    C'TR3CACFL 2104D3 MB 001'                                        
         DC    C'TR3CACFL 2107D3 MB 006'                                        
         DC    C'TR3CACFL 2108D3 MB 001'                                        
         DC    C'TR3CACFL 2111D3 MB 007'                                        
         DC    C'TR3CACFL 2112D3 MB 006'                                        
         DC    C'TR3CACFL 2113D3 MB 001'                                        
         DC    C'TR3CACFL 2116D3 MB 006'                                        
         DC    C'TR3CACFL 2120D3 MB 006'                                        
         DC    C'TR3CACFL 9012D3 MB 022'                                        
         DC    C'TR3CACFL 9022D3 MB 023'                                        
         DC    C'TR3CACFL 9203D3 MB 024'                                        
         DC    C'TR3CACFL 9802D3 MB 026'                                        
         DC    C'TR3CACFL 9996D3 MB 027'                                        
         DC    C'TR3CAPOD 8012D3 POD029'                                        
         DC    C'TR3CAPOD 8022D3 POD030'                                        
         DC    C'TR3CAPOD 8202D3 POD021'                                        
         DC    C'TR3CAPOD 8973D3 POD031'                                        
         DC    C'TR3CAPOD 8982D3 POD028'                                        
         DC    C'TR3CAPOD 8992D3 POD032'                                        
         DC    C'TR3CHCFL 9996C3 CF 239'                                        
         DC    C'TR3CHPOD 8992C3 POD030'                                        
         DC    C'TR3CVBON 7201C3 BON245'                                        
         DC    C'TR3CVCFL 2001C3 CF 019'                                        
         DC    C'TR3CVCFL 2002C3 CF 003'                                        
         DC    C'TR3CVCFL 2003C3 CF 019'                                        
         DC    C'TR3CVCFL 2004C3 CF 003'                                        
         DC    C'TR3CVCFL 2005C3 CF 019'                                        
         DC    C'TR3CVCFL 2006C3 CF 001'                                        
         DC    C'TR3CVCFL 2007C3 CF 019'                                        
         DC    C'TR3CVCFL 2008C3 CF 001'                                        
         DC    C'TR3CVCFL 2009C3 CF 019'                                        
         DC    C'TR3CVCFL 2010C3 CF 001'                                        
         DC    C'TR3CVCFL 2011C3 CF 019'                                        
         DC    C'TR3CVCFL 2012C3 CF 003'                                        
         DC    C'TR3CVCFL 2013C3 CF 019'                                        
         DC    C'TR3CVCFL 2014C3 CF 004'                                        
         DC    C'TR3CVCFL 2015C3 CF 019'                                        
         DC    C'TR3CVCFL 2016C3 CF 004'                                        
         DC    C'TR3CVCFL 2017C3 CF 019'                                        
         DC    C'TR3CVCFL 2018C3 CF 002'                                        
         DC    C'TR3CVCFL 2019C3 CF 019'                                        
         DC    C'TR3CVCFL 2020C3 CF 001'                                        
         DC    C'TR3CVCFL 2021C3 CF 019'                                        
         DC    C'TR3CVCFL 2022C3 CF 004'                                        
         DC    C'TR3CVCFL 2023C3 CF 019'                                        
         DC    C'TR3CVCFL 2024C3 CF 004'                                        
         DC    C'TR3CVCFL 2025C3 CF 019'                                        
         DC    C'TR3CVCFL 2026C3 CF 004'                                        
         DC    C'TR3CVCFL 2027C3 CF 019'                                        
         DC    C'TR3CVCFL 2028C3 CF 003'                                        
         DC    C'TR3CVCFL 2029C3 CF 019'                                        
         DC    C'TR3CVCFL 2030C3 CF 002'                                        
         DC    C'TR3CVCFL 2031C3 CF 019'                                        
         DC    C'TR3CVCFL 2032C3 CF 004'                                        
         DC    C'TR3CVCFL 2033C3 CF 019'                                        
         DC    C'TR3CVCFL 2034C3 CF 002'                                        
         DC    C'TR3CVCFL 2035C3 CF 019'                                        
         DC    C'TR3CVCFL 2036C3 CF 002'                                        
         DC    C'TR3CVCFL 2037C3 CF 019'                                        
         DC    C'TR3CVCFL 2038C3 CF 002'                                        
         DC    C'TR3CVCFL 2039C3 CF 019'                                        
         DC    C'TR3CVCFL 2040C3 CF 002'                                        
         DC    C'TR3CVCFL 2041C3 CF 020'                                        
         DC    C'TR3CVCFL 2042C3 CF 004'                                        
         DC    C'TR3CVCFL 2043C3 CF 019'                                        
         DC    C'TR3CVCFL 2044C3 CF 001'                                        
         DC    C'TR3CVCFL 2045C3 CF 019'                                        
         DC    C'TR3CVCFL 2046C3 CF 001'                                        
         DC    C'TR3CVCFL 2047C3 CF 019'                                        
         DC    C'TR3CVCFL 2048C3 CF 003'                                        
         DC    C'TR3CVCFL 2051C3 CF 019'                                        
         DC    C'TR3CVCFL 2052C3 CF 003'                                        
         DC    C'TR3CVCFL 2054C3 CF 003'                                        
         DC    C'TR3CVCFL 2056C3 CF 001'                                        
         DC    C'TR3CVCFL 2058C3 CF 002'                                        
         DC    C'TR3CVCFL 2059C3 CF 019'                                        
         DC    C'TR3CVCFL 2060C3 CF 003'                                        
         DC    C'TR3CVCFL 2062C3 CF 001'                                        
         DC    C'TR3CVCFL 2063C3 CF 019'                                        
         DC    C'TR3CVCFL 2064C3 CF 002'                                        
         DC    C'TR3CVCFL 2065C3 CF 019'                                        
         DC    C'TR3CVCFL 2066C3 CF 003'                                        
         DC    C'TR3CVCFL 2067C3 CF 019'                                        
         DC    C'TR3CVCFL 2068C3 CF 003'                                        
         DC    C'TR3CVCFL 2069C3 CF 019'                                        
         DC    C'TR3CVCFL 2070C3 CF 001'                                        
         DC    C'TR3CVCFL 2072C3 CF 004'                                        
         DC    C'TR3CVCFL 2073C3 CF 019'                                        
         DC    C'TR3CVCFL 2074C3 CF 001'                                        
         DC    C'TR3CVCFL 2075C3 CF 019'                                        
         DC    C'TR3CVCFL 2076C3 CF 003'                                        
         DC    C'TR3CVCFL 2077C3 CF 019'                                        
         DC    C'TR3CVCFL 2078C3 CF 003'                                        
         DC    C'TR3CVCFL 2079C3 CF 019'                                        
         DC    C'TR3CVCFL 2080C3 CF 004'                                        
         DC    C'TR3CVCFL 2082C3 CF 001'                                        
         DC    C'TR3CVCFL 2083C3 CF 019'                                        
         DC    C'TR3CVCFL 2084C3 CF 004'                                        
         DC    C'TR3CVCFL 2085C3 CF 019'                                        
         DC    C'TR3CVCFL 2086C3 CF 001'                                        
         DC    C'TR3CVCFL 2092C3 CF 019'                                        
         DC    C'TR3CVCFL 2093C3 CF 001'                                        
         DC    C'TR3CVCFL 2098C3 CF 021'                                        
         DC    C'TR3CVCFL 2099C3 CF 021'                                        
         DC    C'TR3CVCFL 2100C3 CF 021'                                        
         DC    C'TR3CVCFL 2101C3 CF 021'                                        
         DC    C'TR3CVCFL 2102C3 CF 035'                                        
         DC    C'TR3CVCFL 2189C3 CF 005'                                        
         DC    C'TR3CVCFL 9012C3 CF 028'                                        
         DC    C'TR3CVCFL 9022C3 CF 029'                                        
         DC    C'TR3CVCFL 9203C3 CF 033'                                        
         DC    C'TR3CVPOD 8012C3 POD022'                                        
         DC    C'TR3CVPOD 8022C3 POD023'                                        
         DC    C'TR3CVPOD 8202C3 POD027'                                        
         DC    C'TR3CVPOD 8982C3 POD026'                                        
         DC    C'TR3CVPOD 8992C3 POD031'                                        
         DC    C'TR3CVTFL 9203C3 TF 033'                                        
         DC    C'TR3GCBON 7201G3 BON245'                                        
         DC    C'TR3GCPOD 8012G3 POD022'                                        
         DC    C'TR3GCPOD 8022G3 POD025'                                        
         DC    C'TR3GCPOD 8202G3 POD248'                                        
         DC    C'TR3GCPOD 8982G3 POD026'                                        
         DC    C'TR3GCPOD 8992G3 POD027'                                        
         DC    C'TR3GCTFL 2001G3 GF 010'                                        
         DC    C'TR3GCTFL 2002G3 GF 001'                                        
         DC    C'TR3GCTFL 2003G3 GF 010'                                        
         DC    C'TR3GCTFL 2004G3 GF 001'                                        
         DC    C'TR3GCTFL 2005G3 GF 010'                                        
         DC    C'TR3GCTFL 2006G3 GF 001'                                        
         DC    C'TR3GCTFL 2007G3 GF 010'                                        
         DC    C'TR3GCTFL 2008G3 GF 001'                                        
         DC    C'TR3GCTFL 2009G3 GF 010'                                        
         DC    C'TR3GCTFL 2010G3 GF 001'                                        
         DC    C'TR3GCTFL 2011G3 GF 010'                                        
         DC    C'TR3GCTFL 2012G3 GF 001'                                        
         DC    C'TR3GCTFL 2013G3 GF 010'                                        
         DC    C'TR3GCTFL 2014G3 GF 001'                                        
         DC    C'TR3GCTFL 2015G3 GF 010'                                        
         DC    C'TR3GCTFL 2016G3 GF 001'                                        
         DC    C'TR3GCTFL 2017G3 GF 010'                                        
         DC    C'TR3GCTFL 2018G3 GF 001'                                        
         DC    C'TR3GCTFL 2019G3 GF 010'                                        
         DC    C'TR3GCTFL 2020G3 GF 001'                                        
         DC    C'TR3GCTFL 2021G3 GF 010'                                        
         DC    C'TR3GCTFL 2022G3 GF 001'                                        
         DC    C'TR3GCTFL 2023G3 GF 010'                                        
         DC    C'TR3GCTFL 2024G3 GF 001'                                        
         DC    C'TR3GCTFL 2025G3 GF 010'                                        
         DC    C'TR3GCTFL 2026G3 GF 001'                                        
         DC    C'TR3GCTFL 2027G3 GF 010'                                        
         DC    C'TR3GCTFL 2028G3 GF 001'                                        
         DC    C'TR3GCTFL 2029G3 GF 011'                                        
         DC    C'TR3GCTFL 2030G3 GF 010'                                        
         DC    C'TR3GCTFL 2031G3 GF 010'                                        
         DC    C'TR3GCTFL 2032G3 GF 010'                                        
         DC    C'TR3GCTFL 2033G3 GF 010'                                        
         DC    C'TR3GCTFL 2034G3 GF 010'                                        
         DC    C'TR3GCTFL 2035G3 GF 010'                                        
         DC    C'TR3GCTFL 2036G3 GF 001'                                        
         DC    C'TR3GCTFL 2037G3 GF 005'                                        
         DC    C'TR3GCTFL 2037G3 GF 012'                                        
         DC    C'TR3GCTFL 2038G3 GF 005'                                        
         DC    C'TR3GCTFL 2039G3 GF 010'                                        
         DC    C'TR3GCTFL 2040G3 GF 006'                                        
         DC    C'TR3GCTFL 2041G3 GF 010'                                        
         DC    C'TR3GCTFL 2042G3 GF 010'                                        
         DC    C'TR3GCTFL 2043G3 GF 005'                                        
         DC    C'TR3GCTFL 2068G3 GF 003'                                        
         DC    C'TR3GCTFL 2069G3 GF 003'                                        
         DC    C'TR3GCTFL 9012G3 GF 028'                                        
         DC    C'TR3GCTFL 9022G3 GF 029'                                        
         DC    C'TR3GCTFL 9203G3 GF 024'                                        
         DC    C'TR3GCTFL 9996G3 GF 030'                                        
         DC    C'TR3OLBON 7201O3 BON245'                                        
         DC    C'TR3OLCFL 9012O3 LF 022'                                        
         DC    C'TR3OLCFL 9022O3 LF 024'                                        
         DC    C'TR3OLPOD 8012O3 POD026'                                        
         DC    C'TR3OLPOD 8022O3 POD023'                                        
         DC    C'TR3POBON 7201P3 BON245'                                        
         DC    C'TR3POPFL 2001P3 PD 009'                                        
         DC    C'TR3POPFL 2002P3 PD 001'                                        
         DC    C'TR3POPFL 2003P3 PD 010'                                        
         DC    C'TR3POPFL 2004P3 PD 001'                                        
         DC    C'TR3POPFL 2006P3 PD 001'                                        
         DC    C'TR3POPFL 2007P3 PD 011'                                        
         DC    C'TR3POPFL 2008P3 PD 011'                                        
         DC    C'TR3POPFL 2009P3 PD 009'                                        
         DC    C'TR3POPFL 2010P3 PD 001'                                        
         DC    C'TR3POPFL 2011P3 PD 009'                                        
         DC    C'TR3POPFL 2012P3 PD 001'                                        
         DC    C'TR3POPFL 2013P3 PD 009'                                        
         DC    C'TR3POPFL 2014P3 PD 001'                                        
         DC    C'TR3POPFL 2015P3 PD 009'                                        
         DC    C'TR3POPFL 2016P3 PD 001'                                        
         DC    C'TR3POPFL 2017P3 PD 009'                                        
         DC    C'TR3POPFL 2018P3 PD 001'                                        
         DC    C'TR3POPFL 2021P3 PD 009'                                        
         DC    C'TR3POPFL 2022P3 PD 001'                                        
         DC    C'TR3POPFL 2023P3 PD 009'                                        
         DC    C'TR3POPFL 2024P3 PD 001'                                        
         DC    C'TR3POPFL 2025P3 PD 009'                                        
         DC    C'TR3POPFL 2026P3 PD 001'                                        
         DC    C'TR3POPFL 2027P3 PD 009'                                        
         DC    C'TR3POPFL 2028P3 PD 001'                                        
         DC    C'TR3POPFL 2029P3 PD 009'                                        
         DC    C'TR3POPFL 2030P3 PD 001'                                        
         DC    C'TR3POPFL 2031P3 PD 009'                                        
         DC    C'TR3POPFL 2033P3 PD 012'                                        
         DC    C'TR3POPFL 2035P3 PD 012'                                        
         DC    C'TR3POPFL 2037P3 PD 009'                                        
         DC    C'TR3POPFL 2039P3 PD 009'                                        
         DC    C'TR3POPFL 2041P3 PD 009'                                        
         DC    C'TR3POPFL 2043P3 PD 009'                                        
         DC    C'TR3POPFL 2045P3 PD 009'                                        
         DC    C'TR3POPFL 2050P3 PD 001'                                        
         DC    C'TR3POPFL 2054P3 PD 002'                                        
         DC    C'TR3POPFL 2056P3 PD 003'                                        
         DC    C'TR3POPFL 9012P3 PD 021'                                        
         DC    C'TR3POPFL 9022P3 PD 029'                                        
         DC    C'TR3POPFL 9203P3 PD 024'                                        
         DC    C'TR3POPFL 9802P3 PD 030'                                        
         DC    C'TR3POPFL 9996P3 PD 027'                                        
         DC    C'TR3POPOD 8012P3 POD022'                                        
         DC    C'TR3POPOD 8022P3 POD023'                                        
         DC    C'TR3POPOD 8202P3 POD031'                                        
         DC    C'TR3POPOD 8982P3 POD026'                                        
         DC    C'TR3POPOD 8992P3 POD028'                                        
         DC    C'TR4BUBON 7201B4 BON245'                                        
         DC    C'TR4BUCFLR2001B4 BB 011'                                        
         DC    C'TR4BUCFLR2002B4 BB 001'                                        
         DC    C'TR4BUCFLR2003B4 BB 011'                                        
         DC    C'TR4BUCFLR2004B4 BB 001'                                        
         DC    C'TR4BUCFLR2005B4 BB 011'                                        
         DC    C'TR4BUCFLR2006B4 BB 001'                                        
         DC    C'TR4BUCFLR2007B4 BB 011'                                        
         DC    C'TR4BUCFLR2008B4 BB 001'                                        
         DC    C'TR4BUCFLR2009B4 BB 011'                                        
         DC    C'TR4BUCFLR2010B4 BB 001'                                        
         DC    C'TR4BUCFLR2011B4 BB 011'                                        
         DC    C'TR4BUCFLR2012B4 BB 001'                                        
         DC    C'TR4BUCFLR2013B4 BB 011'                                        
         DC    C'TR4BUCFLR2014B4 BB 001'                                        
         DC    C'TR4BUCFLR2015B4 BB 011'                                        
         DC    C'TR4BUCFLR2016B4 BB 001'                                        
         DC    C'TR4BUCFLR2017B4 BB 011'                                        
         DC    C'TR4BUCFLR2018B4 BB 001'                                        
         DC    C'TR4BUCFLR2019B4 BB 011'                                        
         DC    C'TR4BUCFLR2020B4 BB 001'                                        
         DC    C'TR4BUCFLR2021B4 BB 011'                                        
         DC    C'TR4BUCFLR2022B4 BB 001'                                        
         DC    C'TR4BUCFLR2023B4 BB 012'                                        
         DC    C'TR4BUCFLR2024B4 BB 002'                                        
         DC    C'TR4BUCFLR2025B4 BB 012'                                        
         DC    C'TR4BUCFLR2026B4 BB 002'                                        
         DC    C'TR4BUCFLR2027B4 BB 012'                                        
         DC    C'TR4BUCFLR2028B4 BB 002'                                        
         DC    C'TR4BUCFLR2029B4 BB 012'                                        
         DC    C'TR4BUCFLR2030B4 BB 002'                                        
         DC    C'TR4BUCFLR2031B4 BB 012'                                        
         DC    C'TR4BUCFLR2032B4 BB 002'                                        
         DC    C'TR4BUCFLR2033B4 BB 012'                                        
         DC    C'TR4BUCFLR2034B4 BB 002'                                        
         DC    C'TR4BUCFLR2035B4 BB 012'                                        
         DC    C'TR4BUCFLR2036B4 BB 002'                                        
         DC    C'TR4BUCFLR2037B4 BB 012'                                        
         DC    C'TR4BUCFLR2038B4 BB 002'                                        
         DC    C'TR4BUCFLR2039B4 BB 012'                                        
         DC    C'TR4BUCFLR2040B4 BB 002'                                        
         DC    C'TR4BUCFLR2041B4 BB 012'                                        
         DC    C'TR4BUCFLR2042B4 BB 002'                                        
         DC    C'TR4BUCFLR2043B4 BB 012'                                        
         DC    C'TR4BUCFLR2044B4 BB 002'                                        
         DC    C'TR4BUCFLR2045B4 BB 011'                                        
         DC    C'TR4BUCFLR2046B4 BB 001'                                        
         DC    C'TR4BUCFLR2047B4 BB 013'                                        
         DC    C'TR4BUCFLR2048B4 BB 003'                                        
         DC    C'TR4BUCFLR2050B4 BB 014'                                        
         DC    C'TR4BUCFLR2051B4 BB 014'                                        
         DC    C'TR4BUCFLR2052B4 BB 014'                                        
         DC    C'TR4BUCFLR2053B4 BB 014'                                        
         DC    C'TR4BUCFLR2054B4 BB 014'                                        
         DC    C'TR4BUCFLR2055B4 BB 014'                                        
         DC    C'TR4BUCFLR2056B4 BB 014'                                        
         DC    C'TR4BUCFLR9203B4 BB 026'                                        
         DC    C'TR4BUPOD 8012B4 POD022'                                        
         DC    C'TR4BUPOD 8022B4 POD246'                                        
         DC    C'TR4BUPOD 8202B4 POD023'                                        
         DC    C'TR4CABON 7201D4 BON245'                                        
         DC    C'TR4CACFL 2001D4 MB 008'                                        
         DC    C'TR4CACFL 2002D4 MB 001'                                        
         DC    C'TR4CACFL 2005D4 MB 008'                                        
         DC    C'TR4CACFL 2006D4 MB 001'                                        
         DC    C'TR4CACFL 2013D4 MB 008'                                        
         DC    C'TR4CACFL 2014D4 MB 001'                                        
         DC    C'TR4CACFL 2017D4 MB 008'                                        
         DC    C'TR4CACFL 2018D4 MB 002'                                        
         DC    C'TR4CACFL 2021D4 MB 003'                                        
         DC    C'TR4CACFL 2022D4 MB 001'                                        
         DC    C'TR4CACFL 2025D4 MB 008'                                        
         DC    C'TR4CACFL 2026D4 MB 001'                                        
         DC    C'TR4CACFL 2029D4 MB 008'                                        
         DC    C'TR4CACFL 2033D4 MB 008'                                        
         DC    C'TR4CACFL 2034D4 MB 001'                                        
         DC    C'TR4CACFL 2037D4 MB 008'                                        
         DC    C'TR4CACFL 2038D4 MB 001'                                        
         DC    C'TR4CACFL 2062D4 MB 004'                                        
         DC    C'TR4CACFL 2073D4 MB 009'                                        
         DC    C'TR4CACFL 2074D4 MB 010'                                        
         DC    C'TR4CACFL 2075D4 MB 001'                                        
         DC    C'TR4CACFL 2079D4 MB 001'                                        
         DC    C'TR4CACFL 2082D4 MB 005'                                        
         DC    C'TR4CACFL 9203D4 MB 024'                                        
         DC    C'TR4CAPOD 8012D4 POD022'                                        
         DC    C'TR4CAPOD 8022D4 POD027'                                        
         DC    C'TR4CAPOD 8202D4 POD026'                                        
         DC    C'TR4CAPOD 8982D4 POD030'                                        
         DC    C'TR4CDPOD 8992X4 POD026'                                        
         DC    C'TR4CVBON 7201C4 BON245'                                        
         DC    C'TR4CVCFL 0117C4 CF 031'                                        
         DC    C'TR4CVCFL 0127C4 CF 032'                                        
         DC    C'TR4CVCFL 2001C4 CF 033'                                        
         DC    C'TR4CVCFL 2002C4 CF 005'                                        
         DC    C'TR4CVCFL 2003C4 CF 033'                                        
         DC    C'TR4CVCFL 2004C4 CF 006'                                        
         DC    C'TR4CVCFL 2005C4 CF 034'                                        
         DC    C'TR4CVCFL 2006C4 CF 007'                                        
         DC    C'TR4CVCFL 2007C4 CF 033'                                        
         DC    C'TR4CVCFL 2008C4 CF 008'                                        
         DC    C'TR4CVCFL 2009C4 CF 033'                                        
         DC    C'TR4CVCFL 2010C4 CF 009'                                        
         DC    C'TR4CVCFL 2011C4 CF 033'                                        
         DC    C'TR4CVCFL 2012C4 CF 010'                                        
         DC    C'TR4CVCFL 2013C4 CF 033'                                        
         DC    C'TR4CVCFL 2014C4 CF 011'                                        
         DC    C'TR4CVCFL 2015C4 CF 033'                                        
         DC    C'TR4CVCFL 2016C4 CF 012'                                        
         DC    C'TR4CVCFL 2017C4 CF 033'                                        
         DC    C'TR4CVCFL 2018C4 CF 013'                                        
         DC    C'TR4CVCFL 2019C4 CF 033'                                        
         DC    C'TR4CVCFL 2021C4 CF 033'                                        
         DC    C'TR4CVCFL 2023C4 CF 033'                                        
         DC    C'TR4CVCFL 2024C4 CF 033'                                        
         DC    C'TR4CVCFL 2025C4 CF 033'                                        
         DC    C'TR4CVCFL 2027C4 CF 033'                                        
         DC    C'TR4CVCFL 2028C4 CF 033'                                        
         DC    C'TR4CVCFL 2029C4 CF 033'                                        
         DC    C'TR4CVCFL 2031C4 CF 035'                                        
         DC    C'TR4CVCFL 2032C4 CF 035'                                        
         DC    C'TR4CVCFL 2033C4 CF 035'                                        
         DC    C'TR4CVCFL 2034C4 CF 035'                                        
         DC    C'TR4CVCFL 2035C4 CF 035'                                        
         DC    C'TR4CVCFL 2036C4 CF 035'                                        
         DC    C'TR4CVCFL 2037C4 CF 035'                                        
         DC    C'TR4CVCFL 2038C4 CF 035'                                        
         DC    C'TR4CVCFL 2039C4 CF 035'                                        
         DC    C'TR4CVCFL 2040C4 CF 014'                                        
         DC    C'TR4CVCFL 2041C4 CF 033'                                        
         DC    C'TR4CVCFL 9203C4 CF 024'                                        
         DC    C'TR4CVPOD 8012C4 POD022'                                        
         DC    C'TR4CVPOD 8022C4 POD026'                                        
         DC    C'TR4CVPOD 8202C4 POD027'                                        
         DC    C'TR4CVTFL 9203C4 TF 024'                                        
         DC    C'TR4GCBON 7201G4 BON025'                                        
         DC    C'TR4GCPOD 8012G4 POD022'                                        
         DC    C'TR4GCPOD 8022G4 POD027'                                        
         DC    C'TR4GCPOD 8202G4 POD028'                                        
         DC    C'TR4GCPOD 8982G4 POD031'                                        
         DC    C'TR4GCPOD 8992G4 POD026'                                        
         DC    C'TR4GCTFL 2001G4 GF 006'                                        
         DC    C'TR4GCTFL 2002G4 GF 001'                                        
         DC    C'TR4GCTFL 2003G4 GF 006'                                        
         DC    C'TR4GCTFL 2004G4 GF 001'                                        
         DC    C'TR4GCTFL 2005G4 GF 006'                                        
         DC    C'TR4GCTFL 2006G4 GF 001'                                        
         DC    C'TR4GCTFL 2007G4 GF 006'                                        
         DC    C'TR4GCTFL 2008G4 GF 001'                                        
         DC    C'TR4GCTFL 2009G4 GF 006'                                        
         DC    C'TR4GCTFL 2010G4 GF 001'                                        
         DC    C'TR4GCTFL 2011G4 GF 006'                                        
         DC    C'TR4GCTFL 2012G4 GF 001'                                        
         DC    C'TR4GCTFL 2013G4 GF 006'                                        
         DC    C'TR4GCTFL 2014G4 GF 001'                                        
         DC    C'TR4GCTFL 2015G4 GF 006'                                        
         DC    C'TR4GCTFL 2016G4 GF 001'                                        
         DC    C'TR4GCTFL 2017G4 GF 006'                                        
         DC    C'TR4GCTFL 2018G4 GF 001'                                        
         DC    C'TR4GCTFL 2019G4 GF 006'                                        
         DC    C'TR4GCTFL 2020G4 GF 001'                                        
         DC    C'TR4GCTFL 2021G4 GF 006'                                        
         DC    C'TR4GCTFL 2022G4 GF 001'                                        
         DC    C'TR4GCTFL 2024G4 GF 001'                                        
         DC    C'TR4GCTFL 2025G4 GF 006'                                        
         DC    C'TR4GCTFL 2026G4 GF 001'                                        
         DC    C'TR4GCTFL 2027G4 GF 006'                                        
         DC    C'TR4GCTFL 2028G4 GF 001'                                        
         DC    C'TR4GCTFL 2029G4 GF 006'                                        
         DC    C'TR4GCTFL 2030G4 GF 001'                                        
         DC    C'TR4GCTFL 2031G4 GF 006'                                        
         DC    C'TR4GCTFL 2032G4 GF 001'                                        
         DC    C'TR4GCTFL 2034G4 GF 001'                                        
         DC    C'TR4GCTFL 2035G4 GF 007'                                        
         DC    C'TR4GCTFL 2036G4 GF 002'                                        
         DC    C'TR4GCTFL 2037G4 GF 003'                                        
         DC    C'TR4GCTFL 2039G4 GF 008'                                        
         DC    C'TR4GCTFL 2042G4 GF 009'                                        
         DC    C'TR4GCTFL 9203G4 GF 024'                                        
         DC    C'TR4OLBON 7201O4 BON025'                                        
         DC    C'TR4OLPOD 8022O4 POD027'                                        
         DC    C'TR4OLPOD 8992O4 POD026'                                        
         DC    C'TR4POBON 7201P4 BON245'                                        
         DC    C'TR4POPFL 2001P4 PD 010'                                        
         DC    C'TR4POPFL 2002P4 PD 001'                                        
         DC    C'TR4POPFL 2003P4 PD 011'                                        
         DC    C'TR4POPFL 2004P4 PD 001'                                        
         DC    C'TR4POPFL 2005P4 PD 002'                                        
         DC    C'TR4POPFL 2006P4 PD 002'                                        
         DC    C'TR4POPFL 2007P4 PD 002'                                        
         DC    C'TR4POPFL 2008P4 PD 002'                                        
         DC    C'TR4POPFL 2009P4 PD 002'                                        
         DC    C'TR4POPFL 2010P4 PD 002'                                        
         DC    C'TR4POPFL 2011P4 PD 012'                                        
         DC    C'TR4POPFL 2012P4 PD 001'                                        
         DC    C'TR4POPFL 2013P4 PD 012'                                        
         DC    C'TR4POPFL 2014P4 PD 001'                                        
         DC    C'TR4POPFL 2015P4 PD 012'                                        
         DC    C'TR4POPFL 2016P4 PD 001'                                        
         DC    C'TR4POPFL 2017P4 PD 012'                                        
         DC    C'TR4POPFL 2018P4 PD 001'                                        
         DC    C'TR4POPFL 2019P4 PD 012'                                        
         DC    C'TR4POPFL 2020P4 PD 001'                                        
         DC    C'TR4POPFL 2021P4 PD 012'                                        
         DC    C'TR4POPFL 2022P4 PD 001'                                        
         DC    C'TR4POPFL 2023P4 PD 012'                                        
         DC    C'TR4POPFL 2024P4 PD 001'                                        
         DC    C'TR4POPFL 2025P4 PD 028'                                        
         DC    C'TR4POPFL 9203P4 PD 024'                                        
         DC    C'TR4POPOD 8012P4 POD022'                                        
         DC    C'TR4POPOD 8022P4 POD026'                                        
         DC    C'TR4POPOD 8202P4 POD027'                                        
         DC    C'TR5BUBON 7201B5 BON245'                                        
         DC    C'TR5BUCFLR2001B5 BB 008'                                        
         DC    C'TR5BUCFLR2002B5 BB 001'                                        
         DC    C'TR5BUCFLR2003B5 BB 009'                                        
         DC    C'TR5BUCFLR2004B5 BB 004'                                        
         DC    C'TR5BUCFLR2005B5 BB 008'                                        
         DC    C'TR5BUCFLR2006B5 BB 001'                                        
         DC    C'TR5BUCFLR2007B5 BB 009'                                        
         DC    C'TR5BUCFLR2008B5 BB 001'                                        
         DC    C'TR5BUCFLR2010B5 BB 004'                                        
         DC    C'TR5BUCFLR2011B5 BB 009'                                        
         DC    C'TR5BUCFLR2012B5 BB 001'                                        
         DC    C'TR5BUCFLR2015B5 BB 008'                                        
         DC    C'TR5BUCFLR2016B5 BB 001'                                        
         DC    C'TR5BUCFLR2017B5 BB 008'                                        
         DC    C'TR5BUCFLR2018B5 BB 001'                                        
         DC    C'TR5BUCFLR2019B5 BB 009'                                        
         DC    C'TR5BUCFLR2020B5 BB 004'                                        
         DC    C'TR5BUCFLR2024B5 BB 004'                                        
         DC    C'TR5BUCFLR2025B5 BB 004'                                        
         DC    C'TR5BUCFLR2027B5 BB 009'                                        
         DC    C'TR5BUCFLR2028B5 BB 009'                                        
         DC    C'TR5BUCFLR2029B5 BB 002'                                        
         DC    C'TR5BUCFLR2030B5 BB 010'                                        
         DC    C'TR5BUCFLR2031B5 BB 002'                                        
         DC    C'TR5BUCFLR2036B5 BB 011'                                        
         DC    C'TR5BUCFLR2037B5 BB 002'                                        
         DC    C'TR5BUCFLR2043B5 BB 011'                                        
         DC    C'TR5BUCFLR2051B5 BB 012'                                        
         DC    C'TR5BUCFLR2053B5 BB 008'                                        
         DC    C'TR5BUCFLR2054B5 BB 001'                                        
         DC    C'TR5BUCFLR2056B5 BB 003'                                        
         DC    C'TR5BUCFLR2057B5 BB 011'                                        
         DC    C'TR5BUCFLR9012B5 BB 030'                                        
         DC    C'TR5BUCFLR9203B5 BB 024'                                        
         DC    C'TR5BUCFLR9996B5 BB 031'                                        
         DC    C'TR5BUPOD 8012B5 POD022'                                        
         DC    C'TR5BUPOD 8022B5 POD023'                                        
         DC    C'TR5BUPOD 8022B5 POD029'                                        
         DC    C'TR5BUPOD 8202B5 POD023'                                        
         DC    C'TR5BUPOD 8982B5 POD027'                                        
         DC    C'TR5BUPOD 8992B5 POD028'                                        
         DC    C'TR5CABON 7201D5 BON245'                                        
         DC    C'TR5CACFL 0055D5 MB 005'                                        
         DC    C'TR5CACFL 0057D5 MB 006'                                        
         DC    C'TR5CACFL 2001D5 MB 007'                                        
         DC    C'TR5CACFL 2002D5 MB 001'                                        
         DC    C'TR5CACFL 2005D5 MB 007'                                        
         DC    C'TR5CACFL 2006D5 MB 001'                                        
         DC    C'TR5CACFL 2009D5 MB 007'                                        
         DC    C'TR5CACFL 2010D5 MB 001'                                        
         DC    C'TR5CACFL 2017D5 MB 007'                                        
         DC    C'TR5CACFL 2018D5 MB 002'                                        
         DC    C'TR5CACFL 2021D5 MB 007'                                        
         DC    C'TR5CACFL 2022D5 MB 001'                                        
         DC    C'TR5CACFL 2025D5 MB 007'                                        
         DC    C'TR5CACFL 2026D5 MB 001'                                        
         DC    C'TR5CACFL 2038D5 MB 007'                                        
         DC    C'TR5CACFL 2039D5 MB 001'                                        
         DC    C'TR5CACFL 2042D5 MB 007'                                        
         DC    C'TR5CACFL 2043D5 MB 001'                                        
         DC    C'TR5CACFL 2050D5 MB 008'                                        
         DC    C'TR5CACFL 9012D5 MB 022'                                        
         DC    C'TR5CACFL 9203D5 MB 024'                                        
         DC    C'TR5CACFL 9702D5 MB 026'                                        
         DC    C'TR5CAPOD 8012D5 POD023'                                        
         DC    C'TR5CAPOD 8022D5 POD027'                                        
         DC    C'TR5CAPOD 8973D5 POD029'                                        
         DC    C'TR5CAPOD 8982D5 POD030'                                        
         DC    C'TR5CAPOD 8992D5 POD028'                                        
         DC    C'TR5CDCHV 9012X5 CHV001'                                        
         DC    C'TR5CVBON 7201C5 BON245'                                        
         DC    C'TR5CVCFL 0078C5 CF 053'                                        
         DC    C'TR5CVCFL 0206C5 CF 006'                                        
         DC    C'TR5CVCFL 2010C5 CF 055'                                        
         DC    C'TR5CVCFL 2011C5 CF 057'                                        
         DC    C'TR5CVCFL 2012C5 CF 001'                                        
         DC    C'TR5CVCFL 2013C5 CF 057'                                        
         DC    C'TR5CVCFL 2014C5 CF 001'                                        
         DC    C'TR5CVCFL 2015C5 CF 057'                                        
         DC    C'TR5CVCFL 2016C5 CF 001'                                        
         DC    C'TR5CVCFL 2017C5 CF 057'                                        
         DC    C'TR5CVCFL 2018C5 CF 001'                                        
         DC    C'TR5CVCFL 2021C5 CF 057'                                        
         DC    C'TR5CVCFL 2022C5 CF 001'                                        
         DC    C'TR5CVCFL 2025C5 CF 057'                                        
         DC    C'TR5CVCFL 2026C5 CF 002'                                        
         DC    C'TR5CVCFL 2029C5 CF 057'                                        
         DC    C'TR5CVCFL 2030C5 CF 002'                                        
         DC    C'TR5CVCFL 2031C5 CF 057'                                        
         DC    C'TR5CVCFL 2032C5 CF 002'                                        
         DC    C'TR5CVCFL 2033C5 CF 057'                                        
         DC    C'TR5CVCFL 2034C5 CF 002'                                        
         DC    C'TR5CVCFL 2035C5 CF 057'                                        
         DC    C'TR5CVCFL 2036C5 CF 004'                                        
         DC    C'TR5CVCFL 2041C5 CF 057'                                        
         DC    C'TR5CVCFL 2042C5 CF 003'                                        
         DC    C'TR5CVCFL 2043C5 CF 057'                                        
         DC    C'TR5CVCFL 2044C5 CF 005'                                        
         DC    C'TR5CVCFL 2047C5 CF 057'                                        
         DC    C'TR5CVCFL 2048C5 CF 001'                                        
         DC    C'TR5CVCFL 2049C5 CF 057'                                        
         DC    C'TR5CVCFL 2050C5 CF 001'                                        
         DC    C'TR5CVCFL 2055C5 CF 010'                                        
         DC    C'TR5CVCFL 2058C5 CF 058'                                        
         DC    C'TR5CVCFL 2059C5 CF 059'                                        
         DC    C'TR5CVCFL 2060C5 CF 059'                                        
         DC    C'TR5CVCFL 2061C5 CF 059'                                        
         DC    C'TR5CVCFL 2069C5 CF 060'                                        
         DC    C'TR5CVCFL 2154C5 CF 001'                                        
         DC    C'TR5CVCFL 2662C5 CF 002'                                        
         DC    C'TR5CVCFL 9012C5 CF 022'                                        
         DC    C'TR5CVCFL 9203C5 CF 024'                                        
         DC    C'TR5CVPOD 8012C5 POD023'                                        
         DC    C'TR5CVPOD 8022C5 POD026'                                        
         DC    C'TR5CVPOD 8202C5 POD248'                                        
         DC    C'TR5CVPOD 8973C5 POD027'                                        
         DC    C'TR5CVPOD 8982C5 POD029'                                        
         DC    C'TR5CVPOD 8992C5 POD030'                                        
         DC    C'TR5CVTFL 0055C5 TF 052'                                        
         DC    C'TR5CVTFL 0079C5 TF 053'                                        
         DC    C'TR5CVTFL 0140C5 TF 054'                                        
         DC    C'TR5CVTFL 0141C5 TF 011'                                        
         DC    C'TR5CVTFL 0143C5 TF 055'                                        
         DC    C'TR5CVTFL 9203C5 TF 028'                                        
         DC    C'TR5GCBON 7201G5 BON245'                                        
         DC    C'TR5GCPOD 8012G5 POD022'                                        
         DC    C'TR5GCPOD 8022G5 POD028'                                        
         DC    C'TR5GCPOD 8202G5 POD248'                                        
         DC    C'TR5GCPOD 8982G5 POD031'                                        
         DC    C'TR5GCPOD 8992G5 POD026'                                        
         DC    C'TR5GCTFL 0024G5 GF 006'                                        
         DC    C'TR5GCTFL 2001G5 GF 007'                                        
         DC    C'TR5GCTFL 2002G5 GF 001'                                        
         DC    C'TR5GCTFL 2003G5 GF 007'                                        
         DC    C'TR5GCTFL 2004G5 GF 001'                                        
         DC    C'TR5GCTFL 2005G5 GF 007'                                        
         DC    C'TR5GCTFL 2006G5 GF 001'                                        
         DC    C'TR5GCTFL 2007G5 GF 007'                                        
         DC    C'TR5GCTFL 2008G5 GF 001'                                        
         DC    C'TR5GCTFL 2009G5 GF 007'                                        
         DC    C'TR5GCTFL 2011G5 GF 007'                                        
         DC    C'TR5GCTFL 2012G5 GF 001'                                        
         DC    C'TR5GCTFL 2013G5 GF 007'                                        
         DC    C'TR5GCTFL 2014G5 GF 001'                                        
         DC    C'TR5GCTFL 2015G5 GF 007'                                        
         DC    C'TR5GCTFL 2016G5 GF 001'                                        
         DC    C'TR5GCTFL 2017G5 GF 007'                                        
         DC    C'TR5GCTFL 2018G5 GF 001'                                        
         DC    C'TR5GCTFL 2019G5 GF 007'                                        
         DC    C'TR5GCTFL 2021G5 GF 007'                                        
         DC    C'TR5GCTFL 2022G5 GF 007'                                        
         DC    C'TR5GCTFL 2023G5 GF 007'                                        
         DC    C'TR5GCTFL 2024G5 GF 007'                                        
         DC    C'TR5GCTFL 2025G5 GF 007'                                        
         DC    C'TR5GCTFL 2026G5 GF 007'                                        
         DC    C'TR5GCTFL 2027G5 GF 008'                                        
         DC    C'TR5GCTFL 2028G5 GF 002'                                        
         DC    C'TR5GCTFL 2030G5 GF 007'                                        
         DC    C'TR5GCTFL 2031G5 GF 001'                                        
         DC    C'TR5GCTFL 2033G5 GF 009'                                        
         DC    C'TR5GCTFL 2205G5 GF 001'                                        
         DC    C'TR5GCTFL 9012G5 GF 029'                                        
         DC    C'TR5GCTFL 9203G5 GF 024'                                        
         DC    C'TR5GCTFL 9702G5 GF 027'                                        
         DC    C'TR5GCTFL 9996G5 GF 247'                                        
         DC    C'TR5OLBON 7201O5 BON025'                                        
         DC    C'TR5OLBON 7201O5 BON245'                                        
         DC    C'TR5OLCFL 9702O5 LF 027'                                        
         DC    C'TR5OLPOD 8012O5 POD030'                                        
         DC    C'TR5OLPOD 8022O5 POD023'                                        
         DC    C'TR5OLPOD 8973O5 POD028'                                        
         DC    C'TR5OLPOD 8982O5 POD031'                                        
         DC    C'TR5OLPOD 8992O5 POD029'                                        
         DC    C'TR5POBON 7201P5 BON245'                                        
         DC    C'TR5POPFL 2001P5 PD 007'                                        
         DC    C'TR5POPFL 2002P5 PD 001'                                        
         DC    C'TR5POPFL 2003P5 PD 008'                                        
         DC    C'TR5POPFL 2004P5 PD 001'                                        
         DC    C'TR5POPFL 2017P5 PD 008'                                        
         DC    C'TR5POPFL 2018P5 PD 001'                                        
         DC    C'TR5POPFL 2019P5 PD 008'                                        
         DC    C'TR5POPFL 2020P5 PD 001'                                        
         DC    C'TR5POPFL 2023P5 PD 008'                                        
         DC    C'TR5POPFL 2027P5 PD 008'                                        
         DC    C'TR5POPFL 2028P5 PD 001'                                        
         DC    C'TR5POPFL 2029P5 PD 008'                                        
         DC    C'TR5POPFL 2031P5 PD 002'                                        
         DC    C'TR5POPFL 9012P5 PD 029'                                        
         DC    C'TR5POPFL 9203P5 PD 024'                                        
         DC    C'TR5POPOD 8012P5 POD022'                                        
         DC    C'TR5POPOD 8022P5 POD023'                                        
         DC    C'TR5POPOD 8202P5 POD248'                                        
         DC    C'TR5POPOD 8982P5 POD026'                                        
         DC    C'TR5POPOD 8992P5 POD027'                                        
         DC    C'TSCADCFL 2001L3HMFL003'                                        
         DC    C'TSCHVBON 7201V1HBON031'                                        
         DC    C'TSCHVBON 7201V4HBON031'                                        
         DC    C'TSCHVCFL 0055V2HSAN004'                                        
         DC    C'TSCHVCFL 0057V2HHMB005'                                        
         DC    C'TSCHVCFL 0059V2HSAN005'                                        
         DC    C'TSCHVCFL 2001V1HALS032'                                        
         DC    C'TSCHVCFL 2002V1HALS001'                                        
         DC    C'TSCHVCFL 2005V2HHMB006'                                        
         DC    C'TSCHVCFL 2006V2HHMB001'                                        
         DC    C'TSCHVCFL 2009V2HSAN006'                                        
         DC    C'TSCHVCFL 2010V2HSAN001'                                        
         DC    C'TSCHVCFL 2021V1HCOS032'                                        
         DC    C'TSCHVCFL 2022V1HCOS001'                                        
         DC    C'TSCHVCFL 2025V1HDEN032'                                        
         DC    C'TSCHVCFL 2026V1HDEN001'                                        
         DC    C'TSCHVCFL 2029V1HFRV032'                                        
         DC    C'TSCHVCFL 2030V1HFRV001'                                        
         DC    C'TSCHVCFL 2041V1HMTS033'                                        
         DC    C'TSCHVCFL 2042V1HMTS001'                                        
         DC    C'TSCHVCFL 2049V1HSAC032'                                        
         DC    C'TSCHVCFL 2050V1HSAC001'                                        
         DC    C'TSCHVCFL 2053V1HSDI032'                                        
         DC    C'TSCHVCFL 2057V1HSFR033'                                        
         DC    C'TSCHVCFL 2058V1HSFR001'                                        
         DC    C'TSCHVCFL 2065V5HCHI005'                                        
         DC    C'TSCHVCFL 2066V5HCHI001'                                        
         DC    C'TSCHVCFL 2073V3HMFL004'                                        
         DC    C'TSCHVCFL 2074V3HMFL001'                                        
         DC    C'TSCHVCFL 2079V1HCOS034'                                        
         DC    C'TSCHVCFL 2080V1HCOS003'                                        
         DC    C'TSCHVCFL 2081V1HDEN034'                                        
         DC    C'TSCHVCFL 2082V1HDEN002'                                        
         DC    C'TSCHVCFL 2082V1HFRV033'                                        
         DC    C'TSCHVCFL 2089V1HPHO034'                                        
         DC    C'TSCHVCFL 2090V1HPHO001'                                        
         DC    C'TSCHVCFL 2091V1HSAC033'                                        
         DC    C'TSCHVCFL 2092V1HSAC004'                                        
         DC    C'TSCHVCFL 2092V1HSAC005'                                        
         DC    C'TSCHVCFL 2093V1HSDI034'                                        
         DC    C'TSCHVCFL 2098V1HTUC001'                                        
         DC    C'TSCHVCFL 2099V1HPHO005'                                        
         DC    C'TSCHVCFL 4029V1HFRV035'                                        
         DC    C'TSCHVCFL 4057V1HSFR036'                                        
         DC    C'TSCHVCFL 4065V5HCHI004'                                        
         DC    C'TSCHVCFL 4083V1HFRV036'                                        
         DC    C'TSCHVCFL 4091V1HSAC036'                                        
         DC    C'TSCHVTFL 2037V1HLVG033'                                        
         DC    C'TSCHVTFL 2045V1HPHO032'                                        
         DC    C'TSCHVTFL 2046V1HPHO006'                                        
         DC    C'TSCHVTFL 2061V1HTUC032'                                        
         DC    C'TSCHVTFL 2062V1HTUC007'                                        
         DC    C'TSCHVTFL 2090V1HPHO004'                                        
         DC    C'TSCHVTFL 4037V1HLVG037'                                        
         DC    C'TSCHVTFL 4045V1HPHO038'                                        
         DC    C'TSCHVTFL 4061V1HTUC038'                                        
         DC    C'TSPONPFL 2001N1HPHO001'                                        
         DC    C'TSPONPFL 2003N1HSFR001'                                        
         DC    C'TSPONPFL 4002N1HSFR004'                                        
         DC    C'TSTGMTFL 2001M1HMTS001'                                        
         DC    C'TSTGMTFL 2078M2HHMB001'                                        
         DC    C'TSTGMTFL 2082M2HSAN001'                                        
         DC    C'TSTGMTFL 2087M3HMFL004'                                        
         DC    C'TSTGMTFL 2089M1HPHO001'                                        
         DC    C'TSTGMTFL 2090M1HTUC001'                                        
         DC    C'TSTGMTFL 2091M2HSAN003'                                        
         DC    C'TSTGMTFL 2214M1HMTS002'                                        
         DC    C'TSTGMTFL 4083M1HTUC006'                                        
         DC    C'TSTGMTFL 4085M1HPHO006'                                        
         DC    C'TSTGMTFL 4086M1HMTS006'                                        
         DC    AL1(255)                                                         
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
RELO     DS    A                                                                
SAVERD   DS    A                                                                
*                                                                               
IPARMS   DS    0XL32                                                            
ASYSFACS DS    A                   A(SYSTEM FACILITIES TABLE)                   
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL ENTRY)                                 
ACOMFACS DS    A                   A(COMMON FACILITIES TABLE)                   
ASELIST  DS    A                   A(SELIST ENTRY)                              
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR INPUT OUTPUT BLOCK)             
*                                                                               
PLIST    DS    6F                                                               
*                                                                               
FULL     DS    F                                                                
FADRH    DS    F                   ADDR OF ERROR FIELD HEADER                   
FERN     DS    X                   ERROR NUMBER (FOR MESSAGE)                   
FERNA    DS    XL3                 ADDR OF ERROR MSG IF FERN IS 255             
FERRDSP  DS    X                   DISPLACEMENT TO ERROR IN FIELD               
HDRN     DS    X                   HEADER MESSAGE NUMBER (NO ERROR)             
*                                                                               
AERRMSGS DS    A                                                                
AOKMSGS  DS    A                                                                
ACPETAB  DS    A                                                                
SYSNAME  DS    CL4                                                              
*                                                                               
BINPARM  DS    0XL32                                                            
BINPAR1  DS    A                                                                
BINPAR2  DS    A                                                                
BINPAR3  DS    A                                                                
BINPAR4  DS    A                                                                
BINPAR5  DS    A                                                                
BINPAR6  DS    A                                                                
BINPAR7  DS    A                                                                
BINPAR8  DS    A                                                                
*                                                                               
AGETTXT  DS    A                                                                
ASCANNER DS    A                                                                
ADMGR    DS    A                                                                
BINSRCH  DS    A                                                                
*                                                                               
ASSB     DS    A                                                                
ATSAR    DS    A                                                                
LTSAR    DS    F                                                                
*                                                                               
TRMNUM   DS    H                                                                
WORK     DS    CL80                                                             
*                                                                               
TYPE     DS    C                   RECORD TYPE                                  
*                                                                               
FLAG     DS    X                   FLAG BYTE                                    
FLSCROLL EQU   X'80'               OK TO APPLY SCROLL                           
*                                                                               
VMED     DS    C                   VALIDATED INPUT FIELDS                       
VCLT     DS    CL4                                                              
VPRD     DS    CL4                                                              
VEST     DS    CL4                                                              
*                                                                               
LAST     DS    CL32                                                             
SCANBLK  DS    5CL(SCBLKLQ)                                                     
SVBLOCK  DS    (A2SAVEL)X                                                       
SVBLOCKL EQU   *-SVBLOCK                                                        
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE (SAVED IN SVBLOCK)                                    *         
***********************************************************************         
         SPACE 1                                                                
A2SAVED  DSECT                                                                  
SVID     DS    CL8                                                              
SVTYPE   DS    C                                                                
SVMED    DS    C                                                                
SVCLT    DS    CL4                                                              
SVPRD    DS    CL4                                                              
SVEST    DS    CL4                                                              
*                                                                               
SVHMED   DS    C                                                                
SVHCLT   DS    CL4                                                              
SVHPRD   DS    CL4                                                              
SVHEST   DS    CL4                                                              
SVHDCLT  DS    CL3                                                              
SVHDPRD  DS    CL3                                                              
SVHDEST  DS    CL3                                                              
A2SAVEL  EQU   *-A2SAVED                                                        
         EJECT                                                                  
***********************************************************************         
* SORT DSECTS FOR DISPLAY                                             *         
***********************************************************************         
         SPACE 1                                                                
CPED     DSECT                     CPE INPUT FILE - BINSRCH RECORD              
CPEMED   DS    C                   MEDIA                                        
CPECLT   DS    CL4                 CLIENT - ADWARE                              
CPEPRD   DS    CL4                 PRODUCT                                      
CPEEST   DS    CL4                 ESTIMATE                                     
CPEDCLT  DS    CL3                 CLIENT - DDS                                 
CPEDPRD  DS    CL3                 PRODUCT                                      
CPEDEST  DS    CL3                 ESTIMATE                                     
CPELQ    EQU   *-CPED                                                           
*                                                                               
ACLID    DSECT                     ADWARE CLIENT DSECT                          
ACMED    DS    C                                                                
ACCLT    DS    CL4                                                              
ACDCLT   DS    CL3                                                              
ACLIKLQ EQU    *-ACLID             KEY LENGTH                                   
ACLIRLQ EQU    *-ACLID             RECORD LENGTH                                
*                                                                               
APRDD    DSECT                     ADWARE PRODUCT DSECT                         
APMED    DS    C                                                                
APCLT    DS    CL4                                                              
APPRD    DS    CL4                                                              
APDCLT   DS    CL3                                                              
APDPRD   DS    CL3                                                              
APRDKLQ  EQU   *-APRDD             KEY LENGTH                                   
APRDRLQ  EQU   *-APRDD             RECORD LENGTH                                
*                                                                               
AESTD    DSECT                     ADWARE ESTIMATE DSECT                        
AEMED    DS    C                                                                
AECLT    DS    CL4                                                              
AEPRD    DS    CL4                                                              
AEEST    DS    CL4                                                              
AEDCLT   DS    CL3                                                              
AEDPRD   DS    CL3                                                              
AEDEST   DS    CL3                                                              
AESTKLQ  EQU   *-AESTD             KEY LENGTH                                   
AESTRLQ  EQU   *-AESTD             RECORD LENGTH                                
*                                                                               
DCLID    DSECT                     DDS CLIENT DSECT                             
DCMED    DS    C                                                                
DCDCLT   DS    CL3                                                              
DCCLT    DS    CL4                                                              
DCLIKLQ  EQU   *-DCLID                                                          
DCLIRLQ  EQU   *-DCLID                                                          
*                                                                               
DPRDD    DSECT                     DDS PRODUCT DSECT                            
DPMED    DS    C                                                                
DPDCLT   DS    CL3                                                              
DPDPRD   DS    CL3                                                              
DPCLT    DS    CL4                                                              
DPPRD    DS    CL4                                                              
DPRDKLQ  EQU   *-DPRDD             KEY LENGTH                                   
DPRDRLQ  EQU   *-DPRDD             RECORD LENGTH                                
*                                                                               
DESTD    DSECT                     DDS ESTIMATE DSECT                           
DEMED    DS    C                                                                
DEDCLT   DS    CL3                                                              
DEDPRD   DS    CL3                                                              
DEDEST   DS    CL3                                                              
DECLT    DS    CL4                                                              
DEPRD    DS    CL4                                                              
DEEST    DS    CL4                                                              
DESTKLQ  EQU   *-DESTD             KEY LENGTH                                   
DESTRLQ  EQU   *-DESTD             RECORD LENGTH                                
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SRA2DFFD DSECT                                                                  
         DS    CL64                                                             
* SRA2DFFD                                                                      
       ++INCLUDE SRA2DFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FASRPARM                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASRPARM                                                       
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
* FATWA                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* FASRS                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASRS                                                          
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRA2D00   07/29/02'                                      
         END                                                                    
