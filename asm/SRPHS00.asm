*          DATA SET SRPHS00    AT LEVEL 004 AS OF 11/21/08                      
*PHASE T13800A                                                                  
         TITLE '$PHASE - DISPLAY PHASE LIST'                                    
MAIN     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,*$PHASE*,RA,CLEAR=YES,RR=RE                                
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RD,SAVERD                                                        
         ST    RE,RELO                                                          
         MVC   IPARMS,0(R1)                                                     
         L     R9,ATWA                                                          
         USING T138FFD,R9          R9=A(TWA)                                    
         L     R8,AUTL                                                          
         USING UTLD,R8             R8=A(UTL ENTRY)                              
*                                                                               
         BRAS  RE,INIT             DO INITIALISATION                            
         BRAS  RE,VALP1                                                         
         BNE   MAIN04                                                           
         BRAS  RE,VALP3                                                         
         BNE   MAIN04                                                           
         BRAS  RE,VALP4                                                         
         BNE   MAIN04                                                           
*                                                                               
         XC    FADRH,FADRH                                                      
         CLI   OPSW,C'H'                                                        
         BE    MAIN02                                                           
         CLI   OPSW,C'Z'                                                        
         BE    MAIN02                                                           
*                                                                               
         BRAS  RE,DISP             DISPLAY PHASE INFO                           
         B     MAIN04                                                           
*                                                                               
MAIN02   BRAS  RE,CHAIN            DISPLAY CHAIN INFO                           
*                                                                               
MAIN04   MVC   TTEST,SAVETEST      RESTORE TTEST VALUE                          
         MVC   TTEST1,SAVETST1                                                  
         CLI   FERN,0                                                           
         BE    *+12                                                             
         BRAS  RE,DISERR                                                        
         B     XMOD                                                             
*                                                                               
         BRAS  RE,DISOK                                                         
         BRAS  RE,OUTNEXT                                                       
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE P1                                                         *         
***********************************************************************         
         SPACE 1                                                                
VALP1    NTR1  ,                                                                
         LA    R2,SRVP1H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BNE   *+12                                                             
         MVI   FERN,1              MISSING INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
         CLI   FHDA,C'?'           ASKED FOR HELP?                              
         BNE   VP101                                                            
         BRAS  RE,P1HELP                                                        
         BH    VP101               SELECT FROM HELP BY PFKEY                    
         MVI   INHELP,YES                                                       
         B     EXITL                                                            
*                                                                               
VP101    CLI   FHIL,7                                                           
         BNH   *+12                                                             
         MVI   FERN,6              TOO MUCH IN FIELD                            
         B     EXITL                                                            
*                                                                               
         MVI   HEX,NO                                                           
         CLI   FHDA,C'T'           INPUT STARTS WITH A 'T'                      
         BNE   VP104                                                            
         CLI   FHIL,6              AND IS LENGTH 4 OR 6                         
         BE    *+12                                                             
         CLI   FHIL,4                                                           
         BNE   VP104                                                            
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB,FHDA                                                         
         MVI   DUB,C'0'                                                         
         CLI   FHIL,4                                                           
         BNE   *+10                                                             
         MVC   DUB+4(2),=C'00'                                                  
*                                                                               
         MVI   SCREENS,C'N'                                                     
*        CLI   DUB+4,C'C'         DISPLAY SCREENS?                              
*        BL    VP102              NO                                            
*        CLI   DUB+4,C'F'                                                       
*        BH    VP102              NO                                            
         MVI   SCREENS,C'Y'                                                     
*                                                                               
VP102    GOTO1 AHEXIN,DMCB,DUB,PHASE,6                                          
         OC    12(4,R1),12(R1)                                                  
         BZ    VP104               IF SPP NOT HEX ASSUME SYSTEM NAME            
*                                                                               
         CLC   FHDA(2),=C'T0'      VALIDATE SYSTEM NAME                         
         BNE   VP105               FOR 'T0XXXX' PHASES, DON'T LOOK UP           
         MVC   SPHASE(1),SYSTEM+2                                               
         MVI   SYSTEM,0                                                         
         MVC   PROGRAM,SYSTEM+1    SAVE PROGRAM NUMBER                          
         OI    TTEST1,TTESTURT     SET USE REAL TTEST                           
*NOPAHYD OI    TTEST,TTESTSRA      *TEMP*                                       
         B     VP105                                                            
*                                                                               
VP104    XC    PHASE,PHASE                                                      
         XR    R1,R1                                                            
         IC    R1,FHIL                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8              CHECK FOR SPECIAL KEYWORD                    
         BNE   VP106                                                            
         CLC   FHDA(0),=CL8'FACILS '                                            
         MVI   SYSTEM,0                                                         
         MVI   PROGRAM,X'0A'                                                    
         OI    TTEST1,TTESTURT     SET USE REAL TTEST                           
*NOPAHYD OI    TTEST,TTESTSRA      *TEMP*                                       
*                                                                               
VP105    LA    R2,SRVP2H                                                        
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    EXITOK                                                           
*                                                                               
         MVI   HEX,YES                                                          
         CLI   FHDA,C'?'           ASKED FOR HELP?                              
         BNE   *+12                                                             
         BRAS  RE,P2HELP                                                        
         B     EXITL                                                            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FHIL                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   EXITOK                                                           
         CLC   FHDA(0),=CL8'LEVEL' SHOW LEVEL STAMPS?                           
         MVI   LVLSTMP,C'Y'                                                     
         B     EXITOK                                                           
*                                                                               
VP106    L     R3,ASYSTAB          TRY TO VALIDATE INPUT AS NAME                
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         AHI   R3,6                                                             
         USING SYSLSTD,R3                                                       
         EX    R1,VP1CLC                                                        
         BE    VP110                                                            
         BXLE  R3,R4,*-8                                                        
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
VP1CLC   CLC   FHDA(0),SYSLNAME                                                 
*                                                                               
VP110    MVC   SYSTEM,SYSLNUM      SET SYSTEM NUMBER                            
         L     R3,ASELIST          NOW FIND SELIST ENTRY FOR SYSTEM             
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         AHI   R3,6                                                             
         USING SELISTD,R3                                                       
         CLC   SEOVSYS,SYSTEM                                                   
         BE    *+10                                                             
         BXLE  R3,R4,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         L     R3,SEPGMS           SET UP PGMLIST ENTRY FOR BELOW               
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         AHI   R3,6                                                             
         USING PGMLSTD,R3                                                       
*                                                                               
VP111    LA    R2,SRVP2H           NOW TRY TO VALIDATE PROGRAM                  
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BNE   *+12                                                             
         MVI   FERN,1              MISSING INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
         CLI   FHDA,C'?'           ASKED FOR HELP?                              
         BNE   VP111A                                                           
         BRAS  RE,P2HELP                                                        
         BH    VP111A              SELECT FROM HELP BY PFKEY                    
         MVI   INHELP,YES                                                       
         B     EXITL                                                            
*                                                                               
VP111A   CLI   FHIL,L'PGMNAME                                                   
         BNH   *+12                                                             
         MVI   FERN,6              TOO MUCH IN FIELD                            
         B     EXITL                                                            
*                                                                               
         TM    FHII,FHIIHE         HEX INPUT                                    
         BZ    VP114               NO                                           
         CLI   FHIL,4                                                           
         BE    *+12                                                             
         CLI   FHIL,2                                                           
         BNE   VP114                                                            
*                                                                               
         MVC   DUB(4),FHDA         HEXIN INPUT FIELD                            
         CLI   FHIL,2                                                           
         BNE   *+10                                                             
         MVC   DUB+2(2),=C'00'                                                  
         GOTO1 AHEXIN,DMCB,DUB,PROGRAM,4                                        
         OC    12(4,R1),12(R1)                                                  
         BZ    *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         CLC   PGMNUM,PROGRAM      SEARCH PGMLST FOR PROGRAM NUM                
         BE    VP112                                                            
         BXLE  R3,R4,*-10                                                       
         B     VP114                                                            
*                                                                               
VP112    CLI   PGMCOSYS,0          TEST OVERRIDE SYSTEM                         
         BE    *+10                                                             
         MVC   SYSTEM,PGMCOSYS     YES - SET SYSTEM NUMBER                      
         B     EXITOK                                                           
*                                                                               
VP114    XR    R1,R1                                                            
         IC    R1,FHIL                                                          
         BCTR  R1,0                                                             
         EX    R1,VP2CLC           SEARCH PGMLST FOR PROGRAM NAME               
         BE    VP116                                                            
         BXLE  R3,R4,*-8                                                        
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
VP2CLC   CLC   FHDA(0),PGMNAME                                                  
*                                                                               
VP116    MVC   PROGRAM,PGMNUM      SET PROGRAM                                  
         CLI   PGMCOSYS,0          TEST OVERRIDE SYSTEM                         
         BE    *+10                                                             
         MVC   SYSTEM,PGMCOSYS     YES - SET SYSTEM NUMBER                      
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE P3                                                         *         
***********************************************************************         
         SPACE 1                                                                
VALP3    NTR1  ,                                                                
         LA    R2,SRVP3H           VALIDATE TEST LEVEL                          
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BE    EXITOK                                                           
         CLI   FHIL,1                                                           
         BE    *+12                                                             
         MVI   FERN,5                                                           
         B     EXITL                                                            
*                                                                               
         MVI   FERN,4              INVALID LEVEL                                
         CLI   FHDA,C'A'                                                        
         BL    EXITL                                                            
         CLI   FHDA,C'C'                                                        
         BH    EXITL                                                            
         MVC   TTEST,FHDA                                                       
         NI    TTEST,TTESTLVL      SET LEVEL                                    
         OI    TTEST1,TTESTURT     SET USE REAL TTEST                           
*NOPAHYD OI    TTEST,TTESTSRA      *TEMP*                                       
         MVI   FERN,0                                                           
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE P4 (OPTIONS FIELD)                                         *         
***********************************************************************         
         SPACE 1                                                                
VALP4    NTR1  ,                                                                
         LA    R2,SRVP4H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BE    EXITOK                                                           
*                                                                               
         GOTO1 ASCANNER,DMCB,SRVP4H,(X'84',WORK)                                
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BNZ   *+12                                                             
         MVI   FERN,7              INVALID FORMAT                               
         B     EXITL                                                            
*                                                                               
         CHI   R0,2                                                             
         BNH   *+12                                                             
         MVI   FERN,9              TOO MANY PARAMETERS                          
         B     EXITL                                                            
*                                                                               
         LA    R3,WORK                                                          
         USING SCANBLKD,R3                                                      
VP402    CLI   SC2NDLEN,0                                                       
         BE    VP404                                                            
         MVC   FERRDSP,SC2NDNUM                                                 
         MVI   FERN,8              NO VALID SECOND PARAMETER                    
         B     EXITL                                                            
*                                                                               
VP404    TM    SC1STVAL,SCHEXQ     DID WE HAVE A START NUMBER?                  
         BZ    VP410               NO                                           
         CLI   SC1STLEN,2          SHOULD ONLY BE 2 BYTES                       
         BNE   VP410                                                            
         CLI   HEXSET,YES                                                       
         BNE   VP406                                                            
         MVC   FERRDSP,SC1STNUM                                                 
         MVI   FERN,12             CAN'T HAVE TWO HEX START VALUES              
         B     EXITL                                                            
*                                                                               
VP406    MVC   DUB,ZEROS           JUSTIFY FIELD INTO DUB                       
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         LHI   R1,L'DUB                                                         
         SR    R1,RF                                                            
         LA    R1,DUB(R1)                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SC1STFLD                                                 
         GOTO1 AHEXIN,DMCB,DUB,FULL,L'DUB,0                                     
         MVI   HEXSET,YES                                                       
         L     RF,FULL                                                          
         CHI   RF,255                                                           
         BNH   VP408                                                            
         MVC   FERRDSP,SC1STNUM                                                 
         MVI   FERN,13                                                          
         B     EXITL                                                            
*                                                                               
VP408    STC   RF,SPHASE                                                        
         B     VP418                                                            
                                                                                
VP410    XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         CHI   RF,8                                                             
         BNH   VP412                                                            
         MVI   FERN,10             TOO MUCH INPUT IN PARAMETER                  
         MVC   FERRDSP,SC1STFLD                                                 
         B     EXITL                                                            
*                                                                               
VP412    BCTR  RF,0                                                             
         LHI   R1,1                                                             
         EX    RF,*+8                                                           
         BE    VP414                                                            
         CLC   SC1STFLD(0),=CL8'CORERES'                                        
         LHI   R1,2                                                             
         EX    RF,*+8                                                           
         BE    VP414                                                            
         CLC   SC1STFLD(0),=CL8'CHAIN'                                          
         LHI   R1,3                                                             
         EX    RF,*+8                                                           
         BE    VP414                                                            
         CLC   SC1STFLD(0),=CL8'MAXCHAIN'                                       
*                                                                               
VP414    CLI   OPSW,C' '                                                        
         BNH   VP416                                                            
         MVC   FERRDSP,SC1STNUM                                                 
         MVI   FERN,11             CANNOT COMBINE PARAMETERS LIKE THIS          
         B     EXITL                                                            
*                                                                               
VP416    MVI   OPSW,C'C'           CORERES                                      
         CHI   R1,1                                                             
         BE    VP418                                                            
         MVI   OPSW,C'H'           CHAIN                                        
         CHI   R1,2                                                             
         BE    VP418                                                            
         MVI   OPSW,C'Z'           MAXCHAIN                                     
*                                                                               
VP418    AHI   R3,SCBLKLQ                                                       
         BCT   R0,VP402                                                         
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         MVI   HDRN,1                                                           
         MVI   LVLSTMP,C'N'                                                     
         MVI   SCREENS,C'N'                                                     
         MVC   SAVETEST,TTEST      SAVE TTEST VALUE IN W/S                      
         MVC   SAVETST1,TTEST1                                                  
         MVI   TTEST,0                                                          
*                                                                               
         L     RF,=A(ERRMSGS)                                                   
         A     RF,RELO                                                          
         ST    RF,AERRMSGS                                                      
         L     RF,=A(OKMSGS)                                                    
         A     RF,RELO                                                          
         ST    RF,AOKMSGS                                                       
         L     RF,=A(PHDATA)                                                    
         A     RF,RELO                                                          
         ST    RF,APHDATA                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   AGETFACT,CGETFACT                                                
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         DROP  RF                                                               
*                                                                               
         L     RF,ASYSFACS                                                      
         USING SYSFACD,RF                                                       
         MVC   ACALLOV,VCALLOV                                                  
         MVC   ASELIST,VSELIST                                                  
         MVC   ASSB,VSSB                                                        
         DROP  RF                                                               
*                                                                               
         L     RF,ASSB                                                          
         USING SSBD,RF                                                          
         MVC   SYSNAME,SSBSYSN4                                                 
         ICM   RF,15,SSBTKADR                                                   
         USING TCBD,RF                                                          
         ICM   R0,15,TCBPGMX                                                    
         S     R0,TCBPGMA                                                       
         ST    R0,TCBLEN                                                        
         DROP  RF                                                               
*                                                                               
         GOTO1 AGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         L     R1,FASYSLST-FACTSD(R1)                                           
         ST    R1,ASYSTAB                                                       
*                                                                               
         L     RF,ATIOB            LOAD A(TIOB)                                 
         USING TIOBD,RF                                                         
         XR    R0,R0                                                            
         IC    R0,TIOBAID          USE PFKEYS 1-12 ONLY                         
         CHI   R0,12                                                            
         BNH   *+8                                                              
         AHI   R0,-12                                                           
         STC   R0,PFKEY            SAVE PFKEY PRESSED                           
         ICM   R1,3,TIOBCURS       DISPLACEMENT TO FIELD FOR CURSOR             
         STCM  R1,3,SCURSOR                                                     
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY PHASE INFORMATION                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING PROGSPCD,PSREC                                                   
DISP     NTR1  ,                                                                
*                                                                               
DSP02    ICM   RF,7,SYSTEM                                                      
         ICM   RF,8,=C'S'                                                       
         GOTO1 ACALLOV,DMCB,PSREC,(RF),(C'D',0)                                 
         CLI   4(R1),255                                                        
         BE    DSP08                                                            
*                                                                               
         CLI   OPSW,C'C'           APPLY CORERES FILTER                         
         BNE   DSP04                                                            
         TM    PSFLAG1,CTPHSCRQ                                                 
         BZ    DSP08                                                            
         B     DSP06                                                            
*                                                                               
DSP04    CLI   LVLSTMP,C'Y'        WAS LEVEL STAMP REQUESTED?                   
         BNE   DSP06               NO                                           
         CLI   SYSTEM,X'07'        FOR TALENT, PRINT ALL PHASES                 
         BE    DSP06                                                            
         CLI   SYSTEM,X'00'        FOR FACILITIES, PRINT ALL PHASES             
         BE    DSP06                                                            
*                                                                               
         CLI   SCREENS,C'Y'        DISPLAY LEVELS FOR SCREENS?                  
         BE    DSP06               YES                                          
         CLI   SPHASE,X'C0'        YES - DON'T DISPLAY SCREENS                  
         BH    DSP08               IF TA0AXX XX>C0 NOT DISPLAYED                
*                                                                               
DSP06    BRAS  RE,FORMAT           DISPLAY PHASE DETAILS                        
         BNE   EXITL                                                            
         B     DSP08                                                            
*                                                                               
DSP08    XR    R0,R0               LOOK FOR THE PHASE THAT MATCHES BEST         
         IC    R0,SPHASE                                                        
         AHI   R0,1                                                             
         CHI   R0,255                                                           
         BH    EXITOK                                                           
         STC   R0,SPHASE                                                        
         B     DSP02                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LONGEST CHAIN LENGTH TO THIS PHASE               *         
***********************************************************************         
         SPACE 1                                                                
CHAIN    NTR1  ,                                                                
         BRAS  RE,BLDMAP                                                        
*                                                                               
         XR    R0,R0               USER SET A PHASE?                            
         ICM   R0,1,SPHASE                                                      
         BNZ   CHN10               YES                                          
*                                                                               
         LA    R3,MAPTBL                                                        
         USING MPAREAD,R3                                                       
         XR    R0,R0                                                            
CHN02    CLI   MPNODE,0            LOOK FOR HIGHEST END NODE                    
         BE    CHN04                                                            
         MVC   BYTE,MPNODE                                                      
         NI    BYTE,X'0F'                                                       
         CLM   R0,1,BYTE                                                        
         BH    *+8                                                              
         ICM   R0,1,BYTE           SAVE IT IN R1                                
         AHI   R3,MPAREAL                                                       
         B     CHN02                                                            
         DROP  R3                                                               
*                                                                               
CHN04    SLL   R0,4                MAKE IT A START NODE                         
         STC   R0,BYTE1                                                         
*                                                                               
         LA    R3,MAPTBL           GET LONGEST PHASE FOR THIS NODE              
         USING MPAREAD,R3                                                       
         XR    R0,R0                                                            
         XR    R1,R1                                                            
CHN06    CLI   MPNODE,0            END OF NODES                                 
         BE    CHN09                                                            
         MVC   BYTE,MPNODE                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,BYTE1                                                       
         BNE   CHN08                                                            
         C     R1,MPLEN                                                         
         BH    CHN08                                                            
         ICM   R1,15,MPLEN                                                      
         ICM   R0,1,MPOVR                                                       
*                                                                               
CHN08    AHI   R3,MPAREAL                                                       
         B     CHN06                                                            
         DROP  R3                                                               
*                                                                               
CHN09    LTR   R0,R0                                                            
         BNZ   CHN10                                                            
         CLI   BYTE1,X'10'         IF A 10 NODE, JUST 01 PHASE (ROOT)           
         BE    CHN10                                                            
*                                                                               
* DIDN'T FIND AN N/0 NODE, SO LOOK FOR LONGEST N-1/ANY NODE                     
*                                                                               
         XR    R0,R0                                                            
         IC    R0,BYTE1            SET BYTE1 TO N-1                             
         SRL   R0,4                                                             
         BCTR  R0,0                                                             
         B     CHN04                                                            
*                                                                               
CHN10    STCM  R0,1,SPHASE         SET LONGEST MATCH FOR THIS NODE              
         ICM   RF,7,SYSTEM                                                      
         ICM   RF,8,=C'S'                                                       
         GOTO1 ACALLOV,DMCB,PSREC,(RF),(C'D',0)                                 
*                                                                               
         BRAS  RE,FORMAT           DISPLAY PHASE DETAILS                        
         MVC   TOTLEN,PSLEN                                                     
         BRAS  RE,GETPATH                                                       
         BL    EXITL                                                            
*                                                                               
         LA    R2,SRVPFKH                                                       
         USING FHD,R2                                                           
         LA    R3,FHDA                                                          
         MVC   0(6,R3),=CL6'Total='                                             
         AHI   R3,6                                                             
         EDIT  (B4,TOTLEN),(15,(R3)),0,ALIGN=LEFT,ZERO=NOBLANK                  
         AR    R3,R0                                                            
         MVC   0(18,R3),=C', PGM Area length='                                  
         AHI   R3,18                                                            
         EDIT  (B4,TCBLEN),(15,(R3)),0,ALIGN=LEFT,ZERO=NOBLANK                  
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT P4 FOR NEXT HIT OF ENTER                          *         
***********************************************************************         
         SPACE 1                                                                
OUTNEXT  NTR1  ,                                                                
         LA    RF,SRVP5H                                                        
         ST    RF,FADRH                                                         
*                                                                               
         LA    R2,SRVP4H                                                        
         USING FHD,R2                                                           
         MVC   SRVP4,SPACES                                                     
*                                                                               
         CLI   OPSW,C'H'                                                        
         BNE   *+14                                                             
         MVC   FHDA(8),=CL8'CHAIN'                                              
         B     EXITOK                                                           
*                                                                               
         CLI   OPSW,C'Z'                                                        
         BNE   OUT02                                                            
         MVC   FHDA(8),=CL8'MAXCHAIN'                                           
         B     EXITOK                                                           
*                                                                               
OUT02    CLI   SPHASE,255                                                       
         BNE   *+8                                                              
         MVI   SPHASE,0                                                         
         GOTO1 AHEXOUT,DMCB,SPHASE,FHDA,1,0                                     
         CLI   OPSW,C'C'                                                        
         BNE   EXITOK                                                           
         MVI   FHDA+2,C','                                                      
         MVC   FHDA+3(8),=CL8'CORERES'                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD MAP TABLE ENTRIES FOR EACH PHASE INTO WORK         *         
***********************************************************************         
         SPACE 1                                                                
BLDMAP   NTR1  ,                                                                
         MVC   WPHASE,SPHASE                                                    
         XC    MAPTBL,MAPTBL                                                    
         XR    R0,R0                                                            
*                                                                               
MAP02    STC   R0,SPHASE                                                        
         ICM   RF,7,SYSTEM                                                      
         ICM   RF,8,=C'S'                                                       
         GOTO1 ACALLOV,DMCB,PSREC,(RF),(C'D',0)                                 
         CLI   4(R1),255                                                        
         BE    MAP10                                                            
*                                                                               
MAP04    CLI   PSNODES,0           IGNORE ANYTHING WITH ZERO NODES              
         BE    MAP10                                                            
         TM    PSFLAG1,(CTPHSSCQ+CTPHSCSQ)                                      
         BNZ   MAP10               IGNORE SCREENS                               
*                                                                               
         LA    R3,MAPTBL                                                        
         USING MPAREAD,R3                                                       
MAP06    CLI   MPNODE,0            LOOK FOR THIS NODE OR EMPTY SLOT             
         BE    MAP08                                                            
         CLC   PSNODES,MPNODE                                                   
         BE    MAP08                                                            
         AHI   R3,MPAREAL                                                       
         B     MAP06                                                            
*                                                                               
MAP08    MVC   MPNODE,PSNODES      SET LONGEST MATCH FOR THIS NODE              
         CLI   OPSW,C'Z'                                                        
         BE    MAP09                                                            
         TM    PSFLAG1,CTPHSCRQ                                                 
         BZ    *+10                                                             
         XC    PSLEN,PSLEN                                                      
*                                                                               
MAP09    CLC   MPLEN,PSLEN                                                      
         BH    MAP10                                                            
         MVC   MPLEN,PSLEN                                                      
         MVC   MPLANG,PSLANG                                                    
         MVC   MPOVR,PSOVR                                                      
         MVC   MPLVL,PSLVL                                                      
         DROP  R3                                                               
*                                                                               
MAP10    AHI   R0,1                                                             
         CHI   R0,255              WITHIN RANGE?                                
         BNH   MAP02               NO                                           
*                                                                               
MAP12    LA    R3,MAPTBL           SORT NODES INTO ASCENDING ORDER              
         USING MPAREAD,R3                                                       
*                                                                               
MAP14    CLI   MPNODE,0            END OF LIST?                                 
         BE    MAP22               YES                                          
         LA    R4,MPAREAL(R3)                                                   
NXT      USING MPAREAD,R4                                                       
*                                                                               
MAP16    CLI   NXT.MPNODE,0        END OF LIST?                                 
         BE    MAP20               YES                                          
         CLC   MPNODE,NXT.MPNODE   UNSORTED NODE IS LOWER THAN SORTED           
         BL    MAP18               NO                                           
         XC    MPNODE(MPAREAL),NXT.MPNODE                                       
         XC    NXT.MPNODE(MPAREAL),MPNODE                                       
         XC    MPNODE(MPAREAL),NXT.MPNODE                                       
*                                                                               
MAP18    AHI   R4,MPAREAL          NEXT IN UNSORTED LIST                        
         B     MAP16                                                            
*                                                                               
MAP20    AHI   R3,MPAREAL                                                       
         B     MAP14                                                            
         DROP  R3,NXT                                                           
*                                                                               
MAP22    MVC   SPHASE,WPHASE       RESTORE ORIGINAL PHASE                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GO FIND LONGEST PATH TO THIS OVERLAY USING MAP TABLE                *         
* EXIT: TOTLEN  = LENGTH OF LONGEST PATH                              *         
***********************************************************************         
         SPACE 1                                                                
GETPATH  NTR1  ,                                                                
         XC    NODES,NODES         FIRST BUILD NODE LIST                        
         LA    R2,NODES            (BUILD IN REVERSE SEQUENCE ORDER)            
*                                                                               
         LA    R4,MAPTBL                                                        
         USING MPAREAD,R4                                                       
GP01     CLI   0(R4),0                                                          
         BE    GP05                                                             
         CLC   PSNODES,MPNODE                                                   
         BE    GP02                                                             
         AHI   R4,MPAREAL                                                       
         B     GP01                                                             
         DROP  R4                                                               
*                                                                               
GP02     LR    R3,R4               R3 = LAST FOUND ENTRY                        
         USING MPAREAD,R3                                                       
         PACK  STNODE,MPNODE       GET START NODE                               
         NI    STNODE,X'0F'                                                     
         CLI   STNODE,0            STNODE IS ZERO FOR ROOT PHASE                
         BE    GP08                                                             
*                                                                               
         LA    R3,MAPTBL                                                        
GP04     MVC   NDNODE,MPNODE       GET END NODE                                 
         NI    NDNODE,X'0F'                                                     
         CLC   STNODE,NDNODE       START NODE = END NODE?                       
         BE    GP06                YES - SAVE NODE                              
         AHI   R3,MPAREAL                                                       
         CLI   0(R3),0                                                          
         BH    GP04                                                             
GP05     MVI   FERN,15                                                          
         B     EXITL                                                            
*                                                                               
GP06     MVC   0(1,R2),STNODE      SAVE THIS NODE                               
         AHI   R2,1                NEXT NODE SAVE AREA                          
         LR    R4,R3               SAVE NEW START NODE                          
         B     GP02                                                             
*                                                                               
GP08     LA    R2,NODES            GET LONGEST PHASE FOR EACH NODE PAIR         
*                                                                               
GP10     OC    0(2,R2),0(R2)       REACHED END OF NODES?                        
         BZ    EXITOK              YES                                          
         PACK  BYTE,1(1,R2)        CREATE 'START-END' NODE IN BYTE              
         OC    BYTE,0(R2)                                                       
         XR    R0,R0               R0 = LENGTH OF LONGEST MATCH                 
         LA    R3,MAPTBL                                                        
*                                                                               
GP12     CLC   MPNODE,BYTE         MATCH NODES?                                 
         BE    GP14                NO                                           
         AHI   R3,MPAREAL                                                       
         CLI   0(R3),0             REACHED HIGH ADDRESS?                        
         BNE   GP12                NO - NEXT NODE                               
         B     GP16                                                             
*                                                                               
GP14     ICM   R0,15,MPLEN                                                      
         A     R0,TOTLEN                                                        
         ST    R0,TOTLEN                                                        
*                                                                               
         MVC   SPHASE,MPOVR        DISPLAY PHASE INFO                           
         ICM   RF,7,SYSTEM                                                      
         ICM   RF,8,=C'S'                                                       
         GOTO1 ACALLOV,DMCB,PSREC,(RF),(C'D',0)                                 
         CLI   OPSW,C'Z'                                                        
         BE    *+12                                                             
         TM    PSFLAG1,CTPHSCRQ                                                 
         BO    *+8                                                              
         BRAS  RE,FORMAT           DISPLAY PHASE DETAILS                        
*                                                                               
GP16     AHI   R2,1                POINT TO NEXT START NODE                     
         B     GP10                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FORMAT PHASE LIST ENTRY INTO TWA FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
FORMAT   NTR1  ,                                                                
         BRAS  RE,GETADDR                                                       
         BZ    EXITL                                                            
*                                                                               
         LR    R5,R1                                                            
         USING DCOLSD,R5                                                        
         GOTO1 AHEXOUT,DMCB,PSOVR,0(R5),1,0                                     
         GOTO1 (RF),(R1),PSNODES,4(R5),1,0                                      
*                                                                               
         CLI   LVLSTMP,C'Y'        IF LVLSTMP=Y PUT OUT LEVEL STAMP             
         BNE   FRM06               ELSE LENGTH AND ADDRESS OF PHASE             
*                                                                               
         ICM   R0,7,SYSTEM         LOAD PHASE AND LOOK FOR LEVEL STAMP          
         ICM   R0,8,=C'R'                                                       
         GOTO1 ACALLOV,DMCB,APHDATA,(R0),0                                      
         CLI   4(R1),255                                                        
         BNE   *+12                                                             
         MVI   FERN,14                                                          
         B     EXITL                                                            
*                                                                               
         L     R3,0(R1)            GET ADDRESS OF PHASE                         
         LR    R0,R3                                                            
         ICM   RF,15,PSLEN                                                      
         AR    R0,RF                                                            
*                                                                               
FRM02    CR    R3,R0               REACHED END?                                 
         BNL   FRM08               NO                                           
         CLC   =C'LEVEL=',0(R3)                                                 
         BNE   *+14                                                             
         CLC   =C'DATE=',10(R3)                                                 
         BE    FRM04                                                            
         AHI   R3,1                                                             
         B     FRM02                                                            
                                                                                
FRM04    MVC   DLEVEL,6(R3)        MOVE LEVEL # INTO TWA                        
         MVC   DDATE,15(R3)        MOVE DATE INTO TWA                           
         B     FRM08                                                            
*                                                                               
FRM06    EDIT  (B4,PSLEN),(6,6(R5)),0                                           
*                                                                               
         CLI   OPSW,C'C'           CORERES SHOWS ADDRESS                        
         BNE   FRM08                                                            
         ICM   R0,7,SYSTEM         GET PHASE ADDRESS IN CORE                    
         ICM   R0,8,=C'R'                                                       
         GOTO1 ACALLOV,DMCB,0,(R0),0                                            
         CLI   4(R1),255                                                        
         BNE   *+12                                                             
         MVI   FERN,14                                                          
         B     EXITL                                                            
*                                                                               
         MVC   FULL,0(R1)          OUTPUT IT                                    
         GOTO1 AHEXOUT,DMCB,FULL,DADDR,L'FULL,0                                 
*                                                                               
FRM08    CLI   PSLVL,0             OUTPUT LEVEL STAMP IF SET                    
         BE    *+10                                                             
         MVC   DLVL,PSLVL                                                       
*                                                                               
         TM    PSFLAG1,CTPHSCRQ                                                 
         BZ    *+8                                                              
         MVI   DCORE,C'*'                                                       
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET A(NEXT TWA OUTPUT FIELD) IN R1                                  *         
***********************************************************************         
         SPACE 1                                                                
GETADDR  LHI   R0,LDLINE                                                        
         SRDL  R0,32                                                            
         LHI   RF,DNORMLQ          NORMAL DISPLAY COLUMNS                       
         CLI   OPSW,C'C'                                                        
         BNE   *+8                                                              
         LHI   RF,DCORELQ          CORERES DISPLAY COLUMNS                      
         CLI   LVLSTMP,C'Y'                                                     
         BNE   *+8                                                              
         LHI   RF,DLVLLQ           OVERRIDEN BY LEVEL STAMP DISPLAY             
         STH   RF,HALF             LENGTH OF COLUMN IN FIELD                    
         DR    R0,RF                                                            
         STH   R1,HALF1            NUMBER OF COLUMNS WE CAN FIT                 
*                                                                               
         L     R1,COUNT                                                         
         LA    RF,1(R1)                                                         
         ST    RF,COUNT                                                         
*                                                                               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         D     R0,=A(NLINE)                                                     
         CH    R1,HALF1                                                         
         BNL   GETADDRX                                                         
         MHI   R0,LLINE                                                         
         MH    R1,HALF                                                          
*                                                                               
         LA    RF,SRVT1                                                         
         AR    RF,R1                                                            
         USING DCOLSD,RF                                                        
         MVC   DNUM(3),DCNUM                                                    
         MVC   DNODE,DCNODE                                                     
         CLI   LVLSTMP,C'Y'                                                     
         BE    GADR02                                                           
         MVC   DLEN,DCLEN                                                       
         CLI   OPSW,C'C'                                                        
         BNE   *+10                                                             
         MVC   DADDR,DCADDR                                                     
         B     GADR04                                                           
*                                                                               
GADR02   MVC   DLEVEL,DCLVL                                                     
         MVC   DDATE,DCDATE                                                     
         DROP  RF                                                               
*                                                                               
GADR04   LA    R1,SRVL1(R1)                                                     
         AR    R1,R0                                                            
         LR    RF,R1                                                            
GETADDRX LTR   R1,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* HELP FOR P1                                                         *         
***********************************************************************         
         SPACE 1                                                                
P1HELP   NTR1  ,                                                                
         LA    R3,SRVP1H                                                        
         USING FHD,R3                                                           
         LA    R0,SAVESCR          SAVE BOTTOM HALF OF FF SCREEN                
         LHI   R1,SAVESCRL                                                      
         LA    RE,SRVT1H                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 ACALLOV,DMCB,(X'FE',SRVT1H),0                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,SRVX1H                                                        
         MVC   FHDA(L'P1HL1),P1HL1                                              
         OI    FHAT,FHATHI                                                      
         XR    RF,RF               NEXT LINE ON SCREEN                          
         IC    RF,FHLN                                                          
         AR    R3,RF                                                            
         MVC   FHDA(L'P1HL2),P1HL2                                              
         OI    FHAT,FHATHI                                                      
         XR    RF,RF               NEXT LINE ON SCREEN                          
         IC    RF,FHLN                                                          
         AR    R3,RF                                                            
         ST    R3,FADRH            SET CURSOR POSITION                          
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
         LHI   R0,L'SYSLNAME+2                                                  
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0               FIND OUT HOW MANY FIT ON A LINE              
         STH   RF,FULL+2           FULL+2(2) = NUMBER ON A LINE                 
*                                                                               
         L     R2,ASYSTAB          SYSTEM TABLE                                 
         LH    R4,0(R2)                                                         
         L     R5,2(R2)                                                         
         AHI   R2,6                                                             
         USING SYSLSTD,R2                                                       
*                                                                               
P1H02    LA    R0,SRVXFKH          MAKE SURE IT STILL FITS ON SCREEN            
         CR    R3,R0                                                            
         BNL   P1H06                                                            
*                                                                               
         LH    R0,FULL+2           LOOP ROUND SETTING INFO ON LINES             
         LA    RF,FHDA                                                          
*                                                                               
P1H04    MVC   0(L'SYSLNAME,RF),SYSLNAME                                        
         AH    RF,FULL                                                          
         BXLE  R2,R4,*+8           NEXT FIELD IN LIST                           
         B     P1H06                                                            
         BCT   R0,P1H04                                                         
*                                                                               
         XR    RF,RF               NEXT LINE ON SCREEN                          
         IC    RF,FHLN                                                          
         AR    R3,RF                                                            
         BXLE  R2,R4,P1H02         NEXT FIELD IN LIST                           
*                                                                               
P1H06    CLI   PFKEY,2             'SELECT' CURSOR PRESSED                      
         BE    P1H08               YES                                          
         MVI   FERN,0                                                           
         MVI   HDRN,2                                                           
         B     EXITL                                                            
*                                                                               
P1H08    LA    R3,SRVX1H           FIND FIELD FOR CURSOR                        
         CLC   SCURSOR,FHAD                                                     
         BL    P1H14                                                            
         LA    R3,SRVXFKH                                                       
         CLC   SCURSOR,FHAD                                                     
         BNL   P1H14                                                            
*                                                                               
         LA    R3,SRVX1H           SET FIRST ADDRESS                            
         ST    R3,FULL1                                                         
         LA    RF,SRVXFKH                                                       
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
*                                                                               
P1H10    CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    P1H12                                                            
         ST    R3,FULL1                                                         
         IC    RE,FHLN                                                          
         BXLE  R3,RE,P1H10                                                      
         DC    H'0'                                                             
*                                                                               
P1H12    L     R3,FULL1            CURSOR FIELD IS IN FULL1                     
         CLC   =C'>>',FHDA         INFO FIELD?                                  
         BE    P1H14               YES - NO SELECT                              
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FHAD                                                        
         LH    RE,SCURSOR                                                       
         SR    RE,RF               GET DISP INTO FIELD                          
         SRDL  RE,32                                                            
         LH    R1,FULL                                                          
         DR    RE,R1                                                            
         MH    RF,FULL             GET 1ST CHARACTER OF THIS ENTRY              
         LA    RF,FHDA(RF)                                                      
*                                                                               
         AHI   R1,-3               REMEMBER THE LENGTH+2 ABOVE                  
         EX    R1,*+8                                                           
         BNH   P1H14                                                            
         CLC   0(0,RF),SPACES                                                   
         MVC   SRVP1,SPACES                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRVP1(0),0(RF)                                                   
         LA    R3,SRVP1H                                                        
         ST    R3,FADRH                                                         
         AHI   R1,1                                                             
         STC   R1,FHIL                                                          
*                                                                               
         LA    R0,SAVESCR          RESTORE BOTTOM HALF OF FF SCREEN             
         LHI   R1,SAVESCRL                                                      
         LA    RE,SRVT1H                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         B     EXITH               SHOW SELECT DONE                             
*                                                                               
P1H14    MVI   FERN,3              INVALID CURSOR POSITION                      
         B     EXITL                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* HELP FOR P2                                                         *         
***********************************************************************         
         SPACE 1                                                                
P2HELP   NTR1  ,                                                                
         LA    R3,SRVP2H                                                        
         USING FHD,R3                                                           
         LA    R0,SAVESCR          SAVE BOTTOM HALF OF FF SCREEN                
         LHI   R1,SAVESCRL                                                      
         LA    RE,SRVT1H                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 ACALLOV,DMCB,(X'FE',SRVT1H),0                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   HEX,YES                                                          
         BNE   P2H01                                                            
         LA    R3,SRVX1H                                                        
         MVC   FHDA(L'P2HL2),P2HL2                                              
         OI    FHAT,FHATHI                                                      
         LA    RF,SRVP2H                                                        
         ST    RF,FADRH                                                         
         MVI   FERN,6                                                           
         B     EXITL                                                            
*                                                                               
P2H01    LA    R3,SRVX1H                                                        
         MVC   FHDA(L'P2HL1),P2HL1                                              
         OI    FHAT,FHATHI                                                      
         XR    RF,RF               NEXT LINE ON SCREEN                          
         IC    RF,FHLN                                                          
         AR    R3,RF                                                            
         AR    R3,RF                                                            
         ST    R3,FADRH                                                         
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
         LHI   R0,L'PGMNAME+2                                                   
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0               FIND OUT HOW MANY FIT ON A LINE              
         STH   RF,FULL+2           FULL+2(2) = NUMBER ON A LINE                 
*                                                                               
         L     R2,ASELIST          FIND SELIST ENTRY FOR SYSTEM                 
         LH    R4,0(R2)                                                         
         L     R5,2(R2)                                                         
         AHI   R2,6                                                             
         USING SELISTD,R2                                                       
         CLC   SEOVSYS,SYSTEM                                                   
         BE    *+10                                                             
         BXLE  R2,R4,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         L     R2,SEPGMS           PGMLIST ENTRY FOR SYSTEM                     
         LH    R4,0(R2)                                                         
         L     R5,2(R2)                                                         
         AHI   R2,6                                                             
         USING PGMLSTD,R2                                                       
*                                                                               
P2H02    LA    R0,SRVXFKH          MAKE SURE IT STILL FITS ON SCREEN            
         CR    R3,R0                                                            
         BNL   P2H06                                                            
*                                                                               
         LH    R0,FULL+2           LOOP ROUND SETTING INFO ON LINES             
         LA    RF,FHDA                                                          
*                                                                               
P2H04    MVC   0(L'PGMNAME,RF),PGMNAME                                          
         AH    RF,FULL                                                          
         BXLE  R2,R4,*+8           NEXT FIELD IN LIST                           
         B     P2H06                                                            
         BCT   R0,P2H04                                                         
*                                                                               
         XR    RF,RF               NEXT LINE ON SCREEN                          
         IC    RF,FHLN                                                          
         AR    R3,RF                                                            
         BXLE  R2,R4,P2H02         NEXT FIELD IN LIST                           
*                                                                               
P2H06    CLI   PFKEY,2             'SELECT' CURSOR PRESSED                      
         BE    P2H08               YES                                          
         MVI   FERN,0                                                           
         MVI   HDRN,2                                                           
         B     EXITL                                                            
*                                                                               
P2H08    LA    R3,SRVX1H           FIND FIELD FOR CURSOR                        
         CLC   SCURSOR,FHAD                                                     
         BL    P2H14                                                            
         LA    R3,SRVXFKH                                                       
         CLC   SCURSOR,FHAD                                                     
         BNL   P2H14                                                            
*                                                                               
         LA    R3,SRVX1H           SET FIRST ADDRESS                            
         ST    R3,FULL1                                                         
         LA    RF,SRVXFKH                                                       
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
*                                                                               
P2H10    CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    P2H12                                                            
         ST    R3,FULL1                                                         
         IC    RE,FHLN                                                          
         BXLE  R3,RE,P2H10                                                      
         DC    H'0'                                                             
*                                                                               
P2H12    L     R3,FULL1            CURSOR FIELD IS IN FULL1                     
         CLC   =C'>>',FHDA         INFO FIELD?                                  
         BE    P2H14               YES - NO SELECT                              
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FHAD                                                        
         LH    RE,SCURSOR                                                       
         SR    RE,RF               GET DISP INTO FIELD                          
         SRDL  RE,32                                                            
         LH    R1,FULL                                                          
         DR    RE,R1                                                            
         MH    RF,FULL             GET 1ST CHARACTER OF THIS ENTRY              
         LA    RF,FHDA(RF)                                                      
*                                                                               
         AHI   R1,-3               REMEMBER THE LENGTH+2 ABOVE                  
         EX    R1,*+8                                                           
         BNH   P2H14                                                            
         CLC   0(0,RF),SPACES                                                   
         MVC   SRVP2,SPACES                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRVP2(0),0(RF)                                                   
         LA    R3,SRVP2H                                                        
         ST    R3,FADRH                                                         
         AHI   R1,1                                                             
         STC   R1,FHIL                                                          
*                                                                               
         LA    R0,SAVESCR          RESTORE BOTTOM HALF OF FF SCREEN             
         LHI   R1,SAVESCRL                                                      
         LA    RE,SRVT1H                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         B     EXITH               SHOW SELECT DONE                             
*                                                                               
P2H14    MVI   FERN,3              INVALID CURSOR POSITION                      
         B     EXITL                                                            
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
         LA    RF,SRVP5H                                                        
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS AND HANDY ROUTINES                                      *         
***********************************************************************         
         SPACE 1                                                                
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
EXIT     XIT1  ,                                                                
*                                                                               
XMOD     L     RD,SAVERD                                                        
         XMOD1 ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
EMSGL    EQU   45                                                               
IMSGL    EQU   45                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
LLINE    EQU   SRVL1H-SRVT1H                                                    
LDLINE   EQU   L'SRVL1                                                          
NLINE    EQU   ((SRVPFKH-SRVL1H)/LLINE)                                         
         LTORG                                                                  
*                                                                               
P1HL1    DS    0CL78                                                            
         DC    CL50'>> You may enter a phase TSSPP(00), or use the key'         
         DC    CL28'word ''FAC'' (=T00A)'                                       
P1HL2    DS    0CL78                                                            
         DC    CL50'>> Or you may select from the list of systems belo'         
         DC    CL28'w'                                                          
*                                                                               
P2HL1    DS    0CL78                                                            
         DC    CL50'>> For the system selected, the following programs'         
         DC    CL28' are available'                                             
P2HL2    DS    0CL78                                                            
         DC    CL50'>> The only valid input for this type of entry is '         
         DC    CL28'L(evel)'                                                    
*                                                                               
DCNUM    DC    C'Num'                                                           
DCNODE   DC    C'ND'                                                            
DCLVL    DC    C'Lvl'                                                           
DCLEN    DC    C'Len  '                                                         
DCDATE   DC    C'Date    '                                                      
DCADDR   DC    C'Address '                                                      
DCOVR    DC    C'Oly'                                                           
*                                                                               
SPACES   DC    80C' '                                                           
ZEROS    DC    8C'0'                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* OK MESSAGES                                                         *         
***********************************************************************         
         SPACE 1                                                                
OKMSGS   DS    0CL(IMSGL)                                                       
OK1      DC    CL(IMSGL)'Phases displayed as requested'                         
OK2      DC    CL(IMSGL)'Help displayed. Select with PFK or type name'          
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
         SPACE 1                                                                
ERRMSGS  DS    0CL(EMSGL)                                                       
ERR01    DC    CL(EMSGL)'Missing input field'                                   
ERR02    DC    CL(EMSGL)'Invalid input field'                                   
ERR03    DC    CL(EMSGL)'Invalid position for cursor selection'                 
ERR04    DC    CL(EMSGL)'Level can only be A, B or C'                           
ERR05    DC    CL(EMSGL)'Too much input in field'                               
ERR06    DC    CL(EMSGL)'Help displayed - respond appropriately'                
ERR07    DC    CL(EMSGL)'This field is in an invalid format'                    
ERR08    DC    CL(EMSGL)'No second parameter is valid for this field'           
ERR09    DC    CL(EMSGL)'Too many parameters input in field'                    
ERR10    DC    CL(EMSGL)'Input parameter is too long'                           
ERR11    DC    CL(EMSGL)'Parameters cannot be combined in this fashion'         
ERR12    DC    CL(EMSGL)'You can''t have two hex start values'                  
ERR13    DC    CL(EMSGL)'Hex value input is too big - Max is X''FF'''           
ERR14    DC    CL(EMSGL)'Invalid return from CALLOV'                            
ERR15    DC    CL(EMSGL)'Can''t match nodes - unable to display chain'          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER W/S                                                  *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
RELO     DS    F                                                                
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
ACALLOV  DS    A                                                                
AGETFACT DS    A                                                                
AHEXOUT  DS    A                                                                
AHEXIN   DS    A                                                                
ASSB     DS    A                                                                
ASCANNER DS    A                                                                
*                                                                               
AERRMSGS DS    A                                                                
AOKMSGS  DS    A                                                                
APHDATA  DS    A                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
*                                                                               
FADRH    DS    F                   ADDR OF ERROR FIELD HEADER                   
FERN     DS    X                   ERROR NUMBER (FOR MESSAGE)                   
FERNA    DS    XL3                 ADDR OF ERROR MSG IF FERN IS 255             
FERRDSP  DS    X                   DISPLACEMENT TO ERROR IN FIELD               
HDRN     DS    X                   HEADER MESSAGE NUMBER (NO ERROR)             
*                                                                               
PSREC    DS    XL(PROGSPCL)                                                     
*                                                                               
PHASE    DS    0XL3                                                             
SYSTEM   DS    C                                                                
PROGRAM  DS    C                                                                
SPHASE   DS    C                                                                
*                                                                               
WPHASE   DS    C                                                                
*                                                                               
DMCB     DS    6F                                                               
TOTLEN   DS    F                                                                
TCBLEN   DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
*                                                                               
COUNT    DS    F                                                                
ASYSTAB  DS    A                                                                
SAVETEST DS    X                                                                
SAVETST1 DS    X                                                                
OPSW     DS    C                                                                
LVLSTMP  DS    C                                                                
OVRLAYS  DS    C                                                                
SCREENS  DS    C                                                                
WORK     DS    CL128                                                            
MAPTBL   DS    CL256                                                            
NODES    DS    CL256                                                            
HEX      DS    X                                                                
HEXSET   DS    X                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
STNODE   DS    X                                                                
NDNODE   DS    X                                                                
INHELP   DS    X                                                                
PFKEY    DS    X                                                                
SCURSOR  DS    H                                                                
SYSNAME  DS    CL4                                                              
*                                                                               
SAVESCR  DS    XL(SRVWORK-SRVT1H)                                               
SAVESCRL EQU   *-SAVESCR                                                        
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
* SRPHSFFD                                                                      
       ++INCLUDE SRPHSFFD                                                       
         ORG   SRVT1H                                                           
* SRPHSFED                                                                      
       ++INCLUDE SRPHSFED                                                       
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMNS DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
DCOLSD   DSECT                                                                  
DNUM     DS    XL2                                                              
DLVL     DS    X                                                                
DCORE    DS    X                                                                
DNODE    DS    XL2                                                              
         DS    X                                                                
DLEN     DS    XL5                                                              
         DS    X                                                                
DNORMLQ  EQU   *-DCOLSD                                                         
         ORG   DLEN                                                             
DLEVEL   DS    XL3                                                              
         DS    X                                                                
DDATE    DS    XL8                                                              
         DS    X                                                                
DLVLLQ   EQU   *-DCOLSD                                                         
         ORG   DLEN                                                             
         DS    XL5                                                              
         DS    X                                                                
DADDR    DS    XL8                                                              
         DS    X                                                                
DCORELQ  EQU   *-DCOLSD                                                         
         EJECT                                                                  
***********************************************************************         
* MAP AREA DSECT                                                      *         
***********************************************************************         
         SPACE 1                                                                
MPAREAD  DSECT                                                                  
MPNODE   DS    X                                                                
MPLANG   DS    X                                                                
MPOVR    DS    X                                                                
MPLVL    DS    X                                                                
MPLEN    DS    AL4                                                              
MPAREAL  EQU   *-MPAREAD                                                        
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FAPROGSPCD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAPROGSPCD                                                     
         PRINT ON                                                               
* CTGENPHASE                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENPHASE                                                     
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FAPGMLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
DISPLAY  CSECT                                                                  
         ORG                                                                    
PHDATA   DS    0D                  THIS IS WHERE THE PHASE IS LOADED            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRPHS00   11/21/08'                                      
         END                                                                    
