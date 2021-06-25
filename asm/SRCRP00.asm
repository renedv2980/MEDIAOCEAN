*          DATA SET SRCRP00    AT LEVEL 215 AS OF 02/23/09                      
*PHASE T16E00A                                                                  
         TITLE '$CRAPPER - DISPLAY/ALTER CRAPPER TRAP'                          
         PRINT NOGEN                                                            
*&&ONLIN SET   Y                   ONLINE-ONLY MODULE                           
CRAP     RSECT                                                                  
         NMOD1 WORKL,*CRAPPER,R9,R8,CLEAR=YES,RR=RE                             
         USING WORKD,RC                                                         
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
         MVC   IPARMS,0(R1)                                                     
         L     RA,ATWA                                                          
         USING T16EFFD,RA          RA=A(TWA)                                    
         BRAS  RE,INIT                                                          
*                                                                               
         BRAS  RE,VALP1            VALIDATE P1                                  
         BNE   XMOD                                                             
         BRAS  RE,VALP2            VALIDATE P2                                  
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE P1 INPUT FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALP1    NTR1  ,                                                                
         LA    R2,CRPP1H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         XR    RF,RF                                                            
         ICM   RF,1,FHIL                                                        
         BNZ   *+14                                                             
         MVC   FHDA(2),=C'CR'                                                   
         LHI   RF,2                                                             
*                                                                               
         CLI   FHDA,C'?'                                                        
         BNE   *+12                                                             
         MVI   FERN,1                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                RE=INPUT COMPARISON LENGTH                   
         LA    R1,TYPTAB           MATCH TRACE TYPE                             
         USING TYPTABD,R1                                                       
VP102    CLI   TYPNAME,0           EOT?                                         
         BNE   *+12                NO                                           
         MVI   FERN,01                                                          
         B     EXITL                                                            
*                                                                               
         CLM   RF,1,TYPVMNL        SCOPE FOR INPUT LENGTH                       
         BL    VP104                                                            
         CLM   RF,1,TYPVMXL                                                     
         BH    VP104                                                            
*                                                                               
         EX    RF,*+8              COMPARE FOR TYPE                             
         BNE   VP104                                                            
         CLC   TYPNAME(0),FHDA                                                  
*                                                                               
         MVC   TYPE,TYPTYPE        SAVE TYPE                                    
         MVC   FHDA(L'TYPNAME),TYPNAME                                          
         MVI   FHOL,L'TYPNAME                                                   
         OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
*                                                                               
VP104    AHI   R1,TYPTABL          NEXT ENTRY                                   
         B     VP102                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* TABLE OF VALID ACTIONS (FROM P1 INPUT FIELD)                        *         
***********************************************************************         
         SPACE 1                                                                
TYPTAB   DC    CL8'CHAIN   ',AL1(01,04,01,00),AL4(0)                            
         DC    CL8'CRAPPER ',AL1(01,06,02,00),AL4(0)                            
         DC    AL1(0)                                                           
*                                                                               
TYPTABD  DSECT                                                                  
TYPNAME  DS    CL8                 NAME                                         
TYPVMNL  DS    X                   EXECUTED VALIDATION LENGTH (MIN)             
TYPVMXL  DS    X                   EXECUTED VALIDATION LENGTH (MAX)             
TYPTYPE  DS    X                   TYPE                                         
         DS    XL5                 N/D                                          
TYPTABL  EQU   *-TYPTABD                                                        
*                                                                               
CRAP     RSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE P2 INPUT FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALP2    NTR1  ,                                                                
         LA    R2,CRPP2H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FHIL                                                        
         BNZ   VP201                                                            
         CLI   TYPE,2              CRAPPER HAS DEFAULT                          
         BE    *+12                                                             
         MVI   FERN,2              SET INPUT KEYWORD MESSAGE                    
         B     XMOD                                                             
*                                                                               
         MVC   FHDA(2),=CL2'TR'                                                 
         LHI   RF,2                                                             
*                                                                               
VP201    CLI   FHDA,C'?'           ASKING FOR HELP?                             
         BNE   *+12                NO                                           
         BRAS  RE,P2HELP           YOU MUST BE JOKING - RIGHT?                  
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                RE=INPUT COMPARISON LENGTH                   
*                                                                               
         LA    R1,ACTTAB                                                        
         USING ACTTABD,R1                                                       
VP202    CLI   ACTNAME,0                                                        
         BNE   *+12                                                             
         MVI   FERN,03                                                          
         B     XMOD                                                             
*                                                                               
         CLI   ACTTYPE,0           TYPE 0 ARE FOR ALL                           
         BE    *+14                                                             
         CLC   ACTTYPE,TYPE        MUST BE FOR CORRECT TYPE                     
         BNE   VP204                                                            
*                                                                               
         CLM   RF,1,ACTVMNL        SCOPE FOR INPUT LENGTH                       
         BL    VP204                                                            
         CLM   RF,1,ACTVMXL                                                     
         BH    VP204                                                            
*                                                                               
         MVC   VERB,ACTVERB                                                     
         EX    RF,*+8              MATCH INPUT CHARACTERS                       
         BE    VP206                                                            
         CLC   ACTNAME(0),FHDA                                                  
*                                                                               
VP204    AHI   R1,ACTTABL          NEXT ENTRY                                   
         B     VP202                                                            
*                                                                               
VP206    MVC   FHDA(L'ACTNAME),ACTNAME                                          
         MVI   FHOL,L'ACTNAME                                                   
         OI    FHOI,FHOITR                                                      
         ICM   RF,15,ACTADDR       GO DO ACTION PROCESSING                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     EXIT                                                             
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* SHOW HELP FOR P2 INPUT FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
P2HELP   NTR1  ,                                                                
         BRAS  RE,LOADFE                                                        
         LA    R2,TWAL1H                                                        
         USING FHD,R2                                                           
*                                                                               
         LA    R1,ACTTAB                                                        
         USING ACTTABD,R1                                                       
         MVI   FERN,0                                                           
         MVI   HDRN,2                                                           
         XR    RF,RF                                                            
*                                                                               
P2H02    CLI   FHLN,0                                                           
         BE    XMOD                                                             
         CLI   ACTNAME,0                                                        
         BE    XMOD                                                             
*                                                                               
         CLI   ACTTYPE,0           TYPE 0 ARE FOR ALL                           
         BE    *+14                                                             
         CLC   ACTTYPE,TYPE        MUST BE FOR CORRECT TYPE                     
         BNE   P2H04                                                            
*                                                                               
         MVC   FHDA(L'ACTNAME),ACTNAME                                          
         MVC   FHDA+L'ACTNAME+4(L'ACTHELP),ACTHELP                              
         OI    FHOI,FHOITR                                                      
*                                                                               
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
*                                                                               
P2H04    AHI   R1,ACTTABL          NEXT ENTRY                                   
         B     P2H02                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* TABLE OF VALID ACTIONS (FROM P2 INPUT FIELD)                        *         
***********************************************************************         
         SPACE 1                                                                
ACTTAB   DC    CL8'HELP    ',AL1(00,03,00,00),AL4(P2HELP)                       
         DC    CL48'Prints out this helpful screen            '                 
*                                                                               
         DC    CL8'CHAIN   ',AL1(01,04,06,01),AL4(CCHN)                         
         DC    CL48'Shows W/S chain structure                 '                 
         DC    CL8'MAP     ',AL1(01,02,08,01),AL4(CMAP)                         
         DC    CL48'Shows the MAP area                        '                 
         DC    CL8'PGM     ',AL1(01,02,09,01),AL4(CPGM)                         
         DC    CL48'Shows contents of the PGMS area           '                 
         DC    CL8'TIA     ',AL1(01,02,07,01),AL4(CTIA)                         
         DC    CL48'Shows contents of TIA                     '                 
         DC    CL8'TRACE   ',AL1(02,04,01,01),AL4(CTRACE)                       
         DC    CL48'Sets CHAIN tracing on or off              '                 
         DC    CL8'TTL     ',AL1(02,03,03,01),AL4(CSCRN)                        
         DC    CL48'Shows bottom of offending TWA             '                 
         DC    CL8'TTU     ',AL1(02,03,02,01),AL4(CSCRN)                        
         DC    CL48'Shows top of offending TWA                '                 
         DC    CL8'UTL     ',AL1(01,03,04,01),AL4(CUTL)                         
         DC    CL48'Shows UTL of offending transaction        '                 
         DC    CL8'W/S     ',AL1(01,03,05,01),AL4(CWRK)                         
         DC    CL48'Shows contents of working storage         '                 
*                                                                               
         DC    CL8'CHAIN   ',AL1(01,04,06,02),AL4(CCHN)                         
         DC    CL48'Shows W/S chain structure                 '                 
         DC    CL8'MAP     ',AL1(01,02,08,02),AL4(CMAP)                         
         DC    CL48'Shows the MAP area                        '                 
         DC    CL8'PGM     ',AL1(01,02,09,02),AL4(CPGM)                         
         DC    CL48'Shows contents of the PGMS area           '                 
         DC    CL8'TIA     ',AL1(01,02,07,02),AL4(CTIA)                         
         DC    CL48'Shows contents of TIA                     '                 
         DC    CL8'TRACE   ',AL1(01,04,01,02),AL4(CTRACE)                       
         DC    CL48'Sets CRAPPER trace ON/OFF or shows STATUS '                 
         DC    CL8'TTL     ',AL1(02,02,03,02),AL4(CSCRN)                        
         DC    CL48'Shows bottom of offending TWA             '                 
         DC    CL8'TTU     ',AL1(01,02,02,02),AL4(CSCRN)                        
         DC    CL48'Shows top of offending TWA                '                 
         DC    CL8'UTL     ',AL1(01,02,04,02),AL4(CUTL)                         
         DC    CL48'Shows UTL of offending transaction        '                 
         DC    CL8'W/S     ',AL1(00,02,05,02),AL4(CWRK)                         
         DC    CL48'Shows contents of working storage         '                 
         DC    AL1(0)                                                           
*                                                                               
ACTTABD  DSECT                                                                  
ACTNAME  DS    CL8                 NAME                                         
ACTVMNL  DS    X                   EXECUTED VALIDATION LENGTH (MIN)             
ACTVMXL  DS    X                   EXECUTED VALIDATION LENGTH (MAX)             
ACTVERB  DS    X                   VERB                                         
ACTTYPE  DS    X                   FOR THIS TYPE ONLY                           
ACTADDR  DS    XL4                 PROCESSING ROUTINE                           
ACTHELP  DS    CL48                HELP STRING                                  
ACTTABL  EQU   *-ACTTABD                                                        
*                                                                               
CRAP     RSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* RESET TRACE INFORMATION FOR CRAPPER TRAP                            *         
***********************************************************************         
         SPACE 1                                                                
CTRACE   NTR1  ,                                                                
         CLI   TYPE,1                                                           
         BNE   CTRC04                                                           
         MVC   CRPVF1,VF1CHN                                                    
         OI    CRPVF1H+FHOI-FHD,FHOITR                                          
*                                                                               
         LA    R2,CRPCLFH                                                       
         USING FHD,R2                                                           
         XR    RF,RF                                                            
CTRC02   ICM   RF,1,FHLN                                                        
         BZ    CTRC04                                                           
         OI    FHOI,FHOITR                                                      
         OI    FHAT,FHATPR                                                      
         LR    RE,RF                                                            
         AHI   RE,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AHI   RE,-(FHDAD)                                                      
         EX    RE,*+4                                                           
         XC    FHDA(0),FHDA                                                     
         BXH   R2,RF,CTRC02                                                     
*                                                                               
CTRC04   LA    R2,CRPP3H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         XR    RF,RF                                                            
         ICM   RF,1,FHIL                                                        
         BNZ   *+14                                                             
         MVC   FHDA(2),=C'ST'                                                   
         LHI   RF,2                                                             
*                                                                               
         BCTR  RF,0                                                             
         LA    R3,COPTTAB                                                       
         USING OPTTABD,R3                                                       
CTRC06   CLI   OPTNAME,0                                                        
         BNE   *+12                                                             
         MVI   FERN,04                                                          
         B     EXITL                                                            
*                                                                               
         CLC   OPTTYPE,TYPE        CORRECT TYPE                                 
         BNE   CTRC08                                                           
*                                                                               
         CLM   RF,1,OPTVMNL        SCOPE FOR INPUT LENGTH                       
         BL    CTRC08                                                           
         CLM   RF,1,OPTVMXL                                                     
         BH    CTRC08                                                           
*                                                                               
         EX    RF,*+8              MATCH INPUT CHARACTERS                       
         BE    CTRC10                                                           
         CLC   OPTNAME(0),FHDA                                                  
*                                                                               
CTRC08   AHI   R3,OPTTABL          NEXT ENTRY                                   
         B     CTRC06                                                           
*                                                                               
CTRC10   ICM   RF,15,OPTADDR       GO DO ACTION PROCESSING                      
         A     RF,RELO                                                          
         BR    RF                                                               
*                                                                               
CTRC12   L     R7,ACRAPBLK                                                      
         USING CRAPPERD,R7         R7=A(CRAPBLK)                                
         MVI   CRTRAP,C'N'         RESET CRAPPER TRAPPING                       
         MVI   FERN,0                                                           
         MVI   HDRN,3                                                           
         MVC   CRPP3,SPACES                                                     
         B     EXIT                                                             
*                                                                               
CTRC14   L     R7,ACHANBLK                                                      
         USING CHAIND,R7           R7=A(CHANBLK)                                
         MVI   CHTRAP,C'N'         RESET CRAPPER TRAPPING                       
         MVI   FERN,0                                                           
         MVI   HDRN,8                                                           
         MVC   CRPP3,SPACES                                                     
         B     EXIT                                                             
*                                                                               
CTRC16   BRAS  RE,CRVAL            SET CRAPPER TRAPPING                         
         BNE   XMOD                                                             
         B     CTRC20                                                           
*                                                                               
CTRC18   L     R7,ACHANBLK                                                      
         USING CHAIND,R7           R7=A(CHANBLK)                                
         MVI   CHTRAP,C'Y'         SET DOING TRAP                               
         B     CTRC22                                                           
*                                                                               
CTRC20   BRAS  RE,CRDIS            DISPLAY CURRENT CRAPPER VALUES               
         B     CTRC24                                                           
*                                                                               
CTRC22   BRAS  RE,CHDIS            DISPLAY CURRENT CHAIN VALUES                 
         B     CTRC24                                                           
*                                                                               
CTRC24   MVC   FHDA(8),=CL8'STATUS'                                             
         CLI   HDRN,1                                                           
         BNE   *+8                                                              
         MVI   HDRN,11                                                          
         LA    RF,CRPP4H                                                        
         ST    RF,FADRH                                                         
         B     XMOD                                                             
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* TRACE OPTIONS                                                       *         
***********************************************************************         
         SPACE 1                                                                
COPTTAB  DC    CL8'ON      ',AL1(01,01,01,00),AL4(CTRC18)                       
         DC    CL8'OFF     ',AL1(01,02,01,00),AL4(CTRC14)                       
         DC    CL8'STATUS  ',AL1(00,05,01,00),AL4(CTRC22)                       
*                                                                               
         DC    CL8'ON      ',AL1(01,01,02,00),AL4(CTRC16)                       
         DC    CL8'OFF     ',AL1(01,02,02,00),AL4(CTRC12)                       
         DC    CL8'STATUS  ',AL1(00,05,02,00),AL4(CTRC20)                       
         DC    AL1(0)                                                           
*                                                                               
OPTTABD  DSECT                                                                  
OPTNAME  DS    CL8                 NAME                                         
OPTVMNL  DS    X                   EXECUTED VALIDATION LENGTH (MIN)             
OPTVMXL  DS    X                   EXECUTED VALIDATION LENGTH (MAX)             
OPTTYPE  DS    X                   TYPE                                         
         DS    X                   N/D                                          
OPTADDR  DS    XL4                 A(ACTION ROUTINE)                            
OPTTABL  EQU   *-OPTTABD                                                        
*                                                                               
CRAP     RSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SET/RESET CRAPPER TRAP VALUES                                       *         
***********************************************************************         
         SPACE 1                                                                
CRVAL    NTR1  ,                                                                
         L     R7,ACRAPBLK                                                      
         USING CRAPPERD,R7         R7=A(CRAPBLK)                                
         MVI   CRTRAP,C'N'         IN CASE OF ERROR                             
         MVI   CRINIT,C'N'                                                      
*                                                                               
         LA    R2,CRPSTOPH     *** STOP AFTER FIRST HIT?                        
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         XC    CRSTOP,CRSTOP                                                    
         BRAS  RE,VALIYN                                                        
         BNE   EXITL                                                            
         OC    CRSTOP,BYTE         SET INPUT VALUE                              
         BNZ   *+8                                                              
         MVI   CRSTOP,C'N'         DEFAULT IS TO NOT STOP                       
*                                                                               
         LA    R2,CRPFIXH      *** FIX CORE AFTER HIT?                          
         ST    R2,FADRH                                                         
         MVI   CRFIX,0                                                          
         BRAS  RE,VALIYN                                                        
         BNE   EXITL                                                            
         OC    CRFIX,BYTE          SET INPUT VALUE                              
         BNZ   *+8                                                              
         MVI   CRFIX,C'Y'          DEFAULT IS TO FIX CORE                       
*                                                                               
         LA    R2,CRPCOREH     *** ADDRESS TO COMPARE AGAINST                   
         ST    R2,FADRH                                                         
         XC    CRAWATCH,CRAWATCH                                                
         CLI   FHIL,0                                                           
         BNE   *+12                                                             
         MVI   FERN,28                                                          
         B     XMOD                                                             
*                                                                               
         CLI   FHDA,C'?'                                                        
         BNE   *+12                                                             
         BRAS  RE,DISHLP                                                        
         B     XMOD                                                             
*                                                                               
         BRAS  RE,VALIHEX                                                       
         BE    *+12                                                             
         BRAS  RE,VALFAC           VALIDATE FROM FACPAK LIST                    
         BNE   XMOD                                                             
*                                                                               
         MVC   CRAWATCH,FULL                                                    
*                                                                               
         LA    R2,CRPCLENH     *** LENGTH OF CORE TO MATCH                      
         ST    R2,FADRH                                                         
         XC    CRLWATCH,CRLWATCH                                                
         BRAS  RE,VALINUM                                                       
         BL    EXITL                                                            
*                                                                               
         L     R0,FULL                                                          
         C     R0,CRCOREL          SCOPE FOR SIZE                               
         BH    *+14                TOO BIG                                      
         MVC   CRLWATCH,FULL                                                    
         B     CRVL02                                                           
*                                                                               
         MVI   FERN,255            SPECIAL MESSAGE                              
         LA    RF,WORK                                                          
         STCM  RF,7,FERNA                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(L'ERRBIG),ERRBIG                                            
         LA    R3,WORK+L'ERRBIG                                                 
         EDIT  (B4,CRCOREL),(6,(R3)),ALIGN=LEFT                                 
         B     EXITL                                                            
*                                                                               
CRVL02   LA    R2,CRPISPCH     *** CORE IN DATASPACE?                           
         ST    R2,FADRH                                                         
         MVI   CRDSPC,0                                                         
         BRAS  RE,VALIYN                                                        
         BNE   EXITL                                                            
         CLI   BYTE,C'Y'                                                        
         BNE   CRVL10              DON'T VALIDATE CRPWSPC FIELD                 
*                                                                               
         LA    R2,CRPWSPCH     *** WHICH ONE?                                   
         ST    R2,FADRH                                                         
         XR    RE,RE                                                            
         ICM   RE,1,FHIL                                                        
         BNZ   *+12                                                             
         MVI   FERN,13                                                          
         B     EXITL                                                            
*                                                                               
         CLI   FHDA,C'?'                                                        
         BNZ   *+12                                                             
         MVI   FERN,13                                                          
         B     EXITL                                                            
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   CRVL04                                                           
         CLC   FHDA(0),TABS                                                     
         MVI   CRDSPC,C'T'         TABS DATASPACE?                              
         B     CRVL10                                                           
*                                                                               
CRVL04   EX    RE,*+8                                                           
         BNE   CRVL08                                                           
         CLC   FHDA(0),DMGR                                                     
         MVI   CRDSPC,C'D'         DMGR DATASPACE?                              
         B     CRVL10                                                           
*                                                                               
CRVL08   MVI   FERN,13                                                          
         B     EXITL                                                            
*                                                                               
CRVL10   MVI   CRTRAP,C'Y'         DO THIS LAST                                 
         MVI   CRHIT,C'N'                                                       
         XC    CRBEFR,CRBEFR                                                    
         XC    CRAFTR,CRAFTR                                                    
         XC    CRCOUNT,CRCOUNT                                                  
         B     EXITOK                                                           
         DROP  R2,R7                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY INFORMATION ON SCREEN                                       *         
***********************************************************************         
         SPACE 1                                                                
CRDIS    NTR1  ,                                                                
         L     R7,ACRAPBLK                                                      
         USING CRAPPERD,R7         R7=A(CRAPBLK)                                
         MVC   CRPSET,LNO                                                       
         CLI   CRTRAP,C'Y'         CRAPPER TRAP IS ACTIVE?                      
         BNE   *+10                NO                                           
         MVC   CRPSET,LYES                                                      
*                                                                               
         MVC   CRPINI,LNO                                                       
         CLI   CRINIT,C'Y'         CRAPPER TRAP IS INITIALISED?                 
         BNE   *+10                NO                                           
         MVC   CRPINI,LYES                                                      
*                                                                               
         MVC   CRPHIT,LNO                                                       
         CLI   CRHIT,C'Y'          CRAPPER TRAP HAS BEEN HIT?                   
         BNE   *+10                NO                                           
         MVC   CRPHIT,LYES                                                      
*                                                                               
         MVC   CRPSTOP,LNO                                                      
         CLI   CRSTOP,C'Y'         STOP AFTER FIRST HIT?                        
         BNE   *+10                NO                                           
         MVC   CRPSTOP,LYES                                                     
*                                                                               
         MVC   CRPFIX,LNO                                                       
         CLI   CRFIX,C'Y'          FIX CORE AFTER HIT?                          
         BNE   *+10                NO                                           
         MVC   CRPFIX,LYES                                                      
*                                                                               
         MVC   CRPISPC,LNO                                                      
         CLI   CRDSPC,0            CORE IN DATASPACE?                           
         BE    CRDS02              NO                                           
         MVC   CRPISPC,LYES                                                     
*                                                                               
         MVC   CRPWSPC,QUERY                                                    
         CLI   CRDSPC,C'T'         TABS DATASPACE?                              
         BNE   *+10                NO                                           
         MVC   CRPWSPC,TABS                                                     
         CLI   CRDSPC,C'D'         DMGR DATASPACE?                              
         BNE   *+10                NO                                           
         MVC   CRPWSPC,DMGR                                                     
*                                                                               
CRDS02   LH    R0,CRCOUNT          NUMBER OF HITS                               
         EDIT  (R0),CRPCNT,ALIGN=LEFT                                           
*                                                                               
         CLI   CRTRAP,C'Y'         CRAPPER TRAP IS ACTIVE?                      
         BNE   CRDS04              NO                                           
*                                                                               
         EDIT  (B4,CRLWATCH),CRPCLEN,ALIGN=LEFT,ZERO=NOBLANK                    
         GOTO1 HEXOUT,DMCB,CRAWATCH,CRPCORE,L'CRAWATCH                          
*                                                                               
CRDS04   CLI   CRHIT,C'Y'          CRAPPER TRAP HAS BEEN HIT?                   
         BNE   EXITOK              NO                                           
         EDIT  (B4,CRDSP),CRPDSP,ALIGN=LEFT,ZERO=NOBLANK                        
*                                                                               
         GOTO1 HEXOUT,DMCB,CRBEFR+00,CRPBFR1,16                                 
         GOTO1 (RF),(R1),CRBEFR+16,CRPBFR2,16                                   
         GOTO1 (RF),(R1),CRAFTR+00,CRPAFT1,16                                   
         GOTO1 (RF),(R1),CRAFTR+16,CRPAFT2,16                                   
         B     EXITOK                                                           
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY INFORMATION ON SCREEN                                       *         
***********************************************************************         
         SPACE 1                                                                
CHDIS    NTR1  ,                                                                
         L     R7,ACHANBLK                                                      
         USING CHAIND,R7           R7=A(CHANBLK)                                
         MVC   CRPSET,LNO                                                       
         CLI   CHTRAP,C'Y'         CHAIN TRAP IS ACTIVE?                        
         BNE   *+10                NO                                           
         MVC   CRPSET,LYES                                                      
*                                                                               
         MVC   CRPINI,LNO                                                       
         CLI   CHINIT,C'Y'         CHAIN TRAP IS INITIALISED?                   
         BNE   *+10                NO                                           
         MVC   CRPINI,LYES                                                      
*                                                                               
         MVC   CRPHIT,LNO                                                       
         CLI   CHHIT,C'Y'          CHAIN TRAP HAS BEEN HIT?                     
         BNE   *+10                NO                                           
         MVC   CRPHIT,LYES                                                      
*                                                                               
         ICM   R0,15,CHDSPMX       MAX DISPLACEMENT                             
         EDIT  (R0),CRPCORE,ALIGN=LEFT,ZERO=NOBLANK                             
         B     EXITOK                                                           
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SCREEN IF CRAPPER TRAP SPRUNG                    *         
***********************************************************************         
         SPACE 1                                                                
CSCRN    NTR1  ,                                                                
         BRAS  RE,TSTFIRE                                                       
         BNE   EXITL                                                            
         BRAS  RE,LOADFE                                                        
*                                                                               
         MVI   TWAL1,C'*'          PUT '***' IN FIRST LINE                      
         MVC   TWAL1+1(L'TWAL1-1),TWAL1                                         
         OI    TWAL1H+FHOI-FHD,FHOITR                                           
                                                                                
         L     R7,ACRAPBLK                                                      
         USING CRAPPERD,R7         R7=A(CRAPBLK)                                
         L     R6,ACHANBLK                                                      
         USING CHAIND,R6           R6=A(CHANBLK)                                
*                                                                               
         CLI   TYPE,1                                                           
         BE    CSCRN02                                                          
         MVC   FULL,CRATWA         A(TWA) FOR CRAPPER                           
         BRAS  RE,ON31                                                          
         L     R1,CRAUTL                                                        
         MVC   OVSYS,TOVSYS-UTLD(R1)                                            
         B     CSCRN04                                                          
*                                                                               
CSCRN02  MVC   FULL,CHATWA         A(TWA) FOR CHAIN                             
         BRAS  RE,ON31                                                          
         L     R1,CHAUTL                                                        
         MVC   OVSYS,TOVSYS-UTLD(R1)                                            
*                                                                               
CSCRN04  BRAS  RE,OFF31                                                         
         BRAS  RE,SCRNOUT                                                       
*                                                                               
         CLI   VERB,2                                                           
         BNE   CSCRN06                                                          
         MVI   HDRN,9                                                           
         MVC   CRPP2(8),=CL8'TTL'                                               
         LA    RF,CRPP3H                                                        
         ST    RF,FADRH                                                         
         B     EXIT                                                             
*                                                                               
CSCRN06  MVC   CRPP2(8),=CL8'TTU'                                               
         LA    RF,CRPP3H                                                        
         ST    RF,FADRH                                                         
         MVI   HDRN,4                                                           
         B     EXIT                                                             
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT UTL INFORMATION FOR TRAPS                         *         
***********************************************************************         
         SPACE 1                                                                
CUTL     NTR1  ,                                                                
         BRAS  RE,TSTFIRE          TEST TRAP FIRED                              
         BNE   EXITL               NO                                           
         BRAS  RE,LOADFE                                                        
*                                                                               
         L     RF,ACHANBLK                                                      
         USING CHAIND,RF           R7=A(CHANBLK)                                
         MVC   START,CHAUTL                                                     
         MVC   LENGTH,CHLUTL                                                    
         MVC   OFFSET,CHCUTL                                                    
         DROP  RF                                                               
*                                                                               
         CLI   TYPE,1              CHAIN?                                       
         BE    CUTL02              YES                                          
*                                                                               
         L     RF,ACRAPBLK                                                      
         USING CRAPPERD,RF         R7=A(CRAPBLK)                                
         MVC   START,CRAUTL                                                     
         MVC   LENGTH,CRLUTL                                                    
         MVC   OFFSET,CRCUTL                                                    
         DROP  RF                                                               
*                                                                               
CUTL02   BRAS  RE,VHP3             VALIDATE P3 AS HEX INPUT                     
         BNE   EXITL                                                            
         BRAS  RE,DISHEX                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT TIA INFORMATION FOR TRAPS                         *         
***********************************************************************         
         SPACE 1                                                                
CTIA     NTR1  ,                                                                
         BRAS  RE,TSTFIRE          TEST TRAP FIRED                              
         BNE   EXITL               NO                                           
         BRAS  RE,LOADFE                                                        
*                                                                               
         L     RF,ACHANBLK                                                      
         USING CHAIND,RF           R7=A(CHANBLK)                                
         MVC   START,CHATIA                                                     
         MVC   LENGTH,CHLTIA                                                    
         MVC   OFFSET,CHCTIA                                                    
         DROP  RF                                                               
*                                                                               
         CLI   TYPE,1              CHAIN?                                       
         BE    CTIA02              YES                                          
*                                                                               
         L     RF,ACRAPBLK                                                      
         USING CRAPPERD,RF         R7=A(CRAPBLK)                                
         MVC   START,CRATIA                                                     
         MVC   LENGTH,CRLTIA                                                    
         MVC   OFFSET,CRCTIA                                                    
         DROP  RF                                                               
*                                                                               
CTIA02   BRAS  RE,VHP3             VALIDATE P3 AS HEX INPUT                     
         BNE   EXITL                                                            
         BRAS  RE,DISHEX                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT MAP INFORMATION FOR TRAPS                         *         
***********************************************************************         
         SPACE 1                                                                
CMAP     NTR1  ,                                                                
         BRAS  RE,TSTFIRE          TEST TRAP FIRED                              
         BNE   EXITL               NO                                           
         BRAS  RE,LOADFE                                                        
*                                                                               
         L     RF,ACHANBLK                                                      
         USING CHAIND,RF           R7=A(CHANBLK)                                
         MVC   START,CHAMAP                                                     
         MVC   LENGTH,CHLMAP                                                    
         MVC   OFFSET,CHCMAP                                                    
         DROP  RF                                                               
*                                                                               
         CLI   TYPE,1              CHAIN?                                       
         BE    CMAP02              YES                                          
*                                                                               
         L     RF,ACRAPBLK                                                      
         USING CRAPPERD,RF         R7=A(CRAPBLK)                                
         MVC   START,CRAMAP                                                     
         MVC   LENGTH,CRLMAP                                                    
         MVC   OFFSET,CRCMAP                                                    
         DROP  RF                                                               
*                                                                               
CMAP02   BRAS  RE,VHP3             VALIDATE P3 AS HEX INPUT                     
         BNE   EXITL                                                            
         BRAS  RE,DISHEX                                                        
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PGMS INFORMATION FOR SPRUNG TRAPS                 *         
***********************************************************************         
         SPACE 1                                                                
CPGM     NTR1  ,                                                                
         BRAS  RE,TSTFIRE          TEST TRAP FIRED                              
         BNE   EXITL               NO                                           
         BRAS  RE,LOADFE                                                        
*                                                                               
         L     RF,ACHANBLK                                                      
         USING CHAIND,RF           R7=A(CHANBLK)                                
         MVC   START,CHAPGM                                                     
         MVC   LENGTH,CHLPGM                                                    
         MVC   OFFSET,CHCPGM                                                    
         DROP  RF                                                               
*                                                                               
         CLI   TYPE,1              CHAIN?                                       
         BE    CPGM02              YES                                          
*                                                                               
         L     RF,ACRAPBLK                                                      
         USING CRAPPERD,RF         R7=A(CRAPBLK)                                
         MVC   START,CRAPGM                                                     
         MVC   LENGTH,CRLPGM                                                    
         MVC   OFFSET,CRCPGM                                                    
         DROP  RF                                                               
*                                                                               
CPGM02   BRAS  RE,VHP3             VALIDATE P3 AS HEX INPUT                     
         BNE   EXITL                                                            
         BRAS  RE,DISHEX                                                        
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT WRK INFORMATION FOR TRAPS                         *         
***********************************************************************         
         SPACE 1                                                                
CWRK     NTR1  ,                                                                
         BRAS  RE,TSTFIRE          TEST TRAP FIRED                              
         BNE   EXITL               NO                                           
         BRAS  RE,LOADFE                                                        
*                                                                               
         L     RF,ACHANBLK                                                      
         USING CHAIND,RF           R7=A(CHANBLK)                                
         MVC   START,CHAWRK                                                     
         MVC   LENGTH,CHLWRK                                                    
         MVC   OFFSET,CHCWRK                                                    
         DROP  RF                                                               
*                                                                               
         CLI   TYPE,1              CHAIN?                                       
         BE    CWRK02              YES                                          
*                                                                               
         L     RF,ACRAPBLK                                                      
         USING CRAPPERD,RF         R7=A(CRAPBLK)                                
         MVC   START,CRAWRK                                                     
         MVC   LENGTH,CRLWRK                                                    
         MVC   OFFSET,CRCWRK                                                    
         DROP  RF                                                               
*                                                                               
CWRK02   BRAS  RE,VHP3             VALIDATE P3 AS HEX INPUT                     
         BNE   EXITL                                                            
         BRAS  RE,DISHEX                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT CHAIN INFORMATION FOR TRAPS                       *         
***********************************************************************         
         SPACE 1                                                                
CCHN     NTR1  ,                                                                
         BRAS  RE,TSTFIRE                                                       
         BNE   EXITL                                                            
         BRAS  RE,LOADFE                                                        
         BRAS  RE,ON31                                                          
*                                                                               
         L     R7,ACRAPBLK                                                      
         USING CRAPPERD,R7         R7=A(CRAPBLK)                                
         L     R6,ACHANBLK                                                      
         USING CHAIND,R6           R6=A(CHAINBLK)                               
*                                                                               
         LA    R0,NDSVREG                                                       
         LHI   R1,NDSVREGL                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LHI   R0,NDSVRCNT                                                      
         LA    R3,NDSVREG                                                       
*                                                                               
         L     R2,CRAWRK                                                        
         CLI   TYPE,1                                                           
         BNE   *+8                                                              
         L     R2,CHAWRK                                                        
         CLC   0(4,R2),=C'MNTR'    MAKE SURE CHAIN LOOKS OK                     
         BE    CCHN04                                                           
         CLC   0(4,R2),=C'SCRU'                                                 
         BE    CCHN04                                                           
         MVI   FERN,15             START IS BAD                                 
         B     EXITL                                                            
*                                                                               
CCHN02   MVC   FULL,8(R2)                                                       
         TM    FULL,X'7F'          THE 80 CAN BE ON NOTHING ELSE CAN            
         BZ    *+12                                                             
         AHI   R3,-L'NDSVREG                                                    
         B     CCHN06                                                           
*                                                                               
         ICM   RE,15,8(R2)                                                      
         S     RE,CRCWRK                                                        
         A     RE,CRAWRK                                                        
*                                                                               
         L     RF,CRAWRK                                                        
         A     RF,CRLWRK                                                        
         CR    RE,RF                                                            
         BL    *+12                                                             
         MVI   FERN,12                                                          
         B     EXITL                                                            
*                                                                               
         LR    R2,RE                                                            
         MVC   56(04,R1),04(R2)    SET CORRECT RD (FROM NEXT IN CHAIN)          
*                                                                               
CCHN04   OC    8(4,R2),8(R2)       END OF CHAIN?                                
         BZ    CCHN06                                                           
*                                                                               
         ICM   RE,15,8(R2)         TOO FAR?                                     
         S     RE,CRCWRK                                                        
         A     RE,CRAWRK                                                        
         L     RF,CRAWRK                                                        
         A     RF,CRLWRK                                                        
         CR    RE,RF                                                            
         BH    CCHN06                                                           
*                                                                               
         XC    00(60,R3),00(R3)                                                 
         MVC   00(04,R3),00(R2)    EYECATCHER - USED ONLY FOR DEBUGGING         
         MVC   04(52,R3),20(R2)    R0-RC                                        
         MVC   60(08,R3),12(R2)    RE-RF                                        
         LR    R1,R3               SAVE TO SET RD                               
         AHI   R3,L'NDSVREG                                                     
         BCT   R0,CCHN02                                                        
         MVC   00(04,R3),=CL4'OVFL'                                             
         B     CCHN08              TABLE OVERFLOW                               
*                                                                               
CCHN06   MVC   00(04,R3),=CL4'END*'                                             
*                                                                               
CCHN08   MVC   56(04,R3),FULL                                                   
         BRAS  RE,OFF31                                                         
*                                                                               
         LHI   R1,NDSVRCNT                                                      
         SR    R1,R0                                                            
         ST    R1,LENGTH           DUMMY MAX LENGTH FOR P3 VALIDATE             
*                                                                               
         MVC   START,CRAWRK                                                     
         MVC   OFFSET,CRCWRK                                                    
         CLI   TYPE,1                                                           
         BNE   *+16                                                             
         MVC   START,CHAWRK                                                     
         MVC   OFFSET,CHCWRK                                                    
*                                                                               
         BRAS  RE,VHP3             VALIDATE P3 AS HEX INPUT                     
         BNE   EXITL                                                            
*                                                                               
         MVC   LENGTH,CRLWRK                                                    
         CLI   TYPE,1                                                           
         BNE   *+10                                                             
         MVC   LENGTH,CHLWRK                                                    
*                                                                               
         BRAS  RE,WRKOUT                                                        
         B     EXIT                                                             
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE P3 AS A HEX VALUE                               *         
* NTRY: ASSUMES LENGTH SET CORRECTLY                                  *         
* EXIT: DISPLACE SET                                                  *         
***********************************************************************         
         SPACE 1                                                                
VHP3     NTR1  ,                                                                
         LA    R2,CRPP3H           DISPLACEMENT HERE                            
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         XC    FULL,FULL                                                        
         CLI   FHIL,0                                                           
         BE    *+12                                                             
         BRAS  RE,VALIHEX                                                       
         BL    EXITL                                                            
         MVC   DISPLACE,FULL                                                    
*                                                                               
         CLI   PFKEY,7                                                          
         BNE   VHP302                                                           
         L     R0,DISPLACE                                                      
         LHI   R1,256                                                           
         CLI   VERB,6                                                           
         BNE   *+8                                                              
         LHI   R1,5                                                             
         CR    R0,R1                                                            
         BH    *+10                                                             
         XR    R0,R0                                                            
         B     *+6                                                              
         SR    R0,R1                                                            
         ST    R0,DISPLACE                                                      
*                                                                               
VHP302   CLI   PFKEY,8                                                          
         BNE   VHP304                                                           
         L     R0,DISPLACE                                                      
         LHI   R1,256                                                           
         CLI   VERB,6                                                           
         BNE   *+8                                                              
         LHI   R1,5                                                             
         AR    R0,R1                                                            
         ST    R0,DISPLACE                                                      
*                                                                               
VHP304   CLC   LENGTH,DISPLACE     DISPLACEMENT OK?                             
         BNH   VHP310              NO                                           
*                                                                               
         LHI   R0,3                                                             
         MVC   FULL,DISPLACE                                                    
VHP306   CLI   FULL,0                                                           
         BNE   VHP308                                                           
         MVC   FULL(3),FULL+1                                                   
         MVI   FULL+3,0                                                         
         BCT   R0,VHP306                                                        
*                                                                               
VHP308   AHI   R0,1                                                             
         GOTO1 HEXOUT,DMCB,FULL,FHDA,(R0),0                                     
         SLL   R0,1                                                             
         STC   R0,FHOL                                                          
         OI    FHOI,FHOITR                                                      
         LA    RF,CRPP4H                                                        
         ST    RF,FADRH                                                         
         B     EXITOK                                                           
*                                                                               
VHP310   MVI   FERN,255                                                         
         LA    RF,WORK                                                          
         STCM  RF,7,FERNA                                                       
         MVC   WORK(L'ERRBIG),ERRBIG                                            
         LA    R3,WORK+L'ERRBIG                                                 
         MVC   0(2,R3),=C'X'''                                                  
*                                                                               
         L     RF,LENGTH                                                        
         BCTR  RF,0                                                             
         ST    RF,LENGTH                                                        
*                                                                               
         LHI   R0,3                                                             
VHP312   CLI   LENGTH,0                                                         
         BNE   VHP314                                                           
         MVC   LENGTH(3),LENGTH+1                                               
         MVI   LENGTH+3,0                                                       
         BCT   R0,VHP312                                                        
*                                                                               
VHP314   AHI   R0,1                                                             
         GOTO1 HEXOUT,DMCB,LENGTH,2(R3),(R0),0                                  
         SLL   R0,1                                                             
         AR    R3,R0                                                            
         MVI   2(R3),C''''                                                      
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT UTL INFORMATION                                   *         
* NTRY: FULL   = A(UTL BUFFER)                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
UTLOUT   NTR1  ,                                                                
         LA    R2,UTLST1H                                                       
         USING FHD,R2                                                           
         XR    RF,RF                                                            
UTLO02   CLI   FHLN,0                                                           
         BE    UTLO04                                                           
         OI    FHOI,FHOITR                                                      
         IC    RF,FHLN                                                          
         BXH   R2,RF,UTLO02                                                     
         DROP  R2                                                               
*                                                                               
UTLO04   BRAS  RE,ON31                                                          
         L     R1,FULL                                                          
         MVC   WORK,0(R1)                                                       
         BRAS  RE,OFF31                                                         
*                                                                               
         USING UTLD,WORK                                                        
         MVC   UTLLUID,TLUID                                                    
         GOTO1 HEXOUT,DMCB,TSTAT1,UTLST1,L'BYTE,0                               
         GOTO1 HEXOUT,DMCB,TSTAT2,UTLST2,L'BYTE,0                               
*                                                                               
         LHI   R0,TUTLLENV                                                      
         LA    R2,UTLU1H                                                        
         USING FHD,R2                                                           
         LA    R3,WORK                                                          
         L     RF,HEXOUT                                                        
         LA    R1,DMCB                                                          
*                                                                               
DOUT06   LHI   R4,(L'UTLU1/2)                                                   
         CHI   R0,(L'UTLU1/2)                                                   
         BH    *+6                                                              
         LR    R4,R0                                                            
*                                                                               
         GOTO1 (RF),(R1),0(R3),FHDA,(R4),0,0                                    
*                                                                               
         SR    R0,R4               ANY UTL LEFT TO DISPLAY?                     
         BNP   EXIT                NO                                           
         AR    R3,R4               NEXT PIECE OF UTL                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         BZ    EXIT                NO SCREEN LEFT                               
*                                                                               
         AR    R2,RE                                                            
         B     DOUT06                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SCREEN                                           *         
* NTRY: FULL   = A(SCREEN BUFFER)                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRNOUT  NTR1  ,                                                                
         LA    R2,TWAL1H                                                        
         USING FHD,R2                                                           
         XR    RF,RF                                                            
SCOU02   CLI   FHLN,0                                                           
         BE    SCOU04                                                           
         OI    FHOI,FHOITR                                                      
         OI    FHAT,FHATHI                                                      
         IC    RF,FHLN                                                          
         BXH   R2,RF,SCOU02                                                     
         DROP  R2                                                               
*                                                                               
SCOU04   BRAS  RE,ON31                                                          
*                                                                               
         L     R6,FULL             A(TWA) FOR CRAPPER                           
         AHI   R6,64                                                            
DMP      USING FHD,R6                                                           
*                                                                               
STWA0    CLI   DMP.FHLN,0          END OF TWA?                                  
         BE    STWAMSG             YES                                          
*                                                                               
         CLI   DMP.FHLN,9                                                       
         BL    DOTWERR2                                                         
         CLI   DMP.FHAT,FHATNP                                                  
         BE    SHWTWANX            NOP FIELD, CAN'T SEE IT                      
         CLI   DMP.FHLN,(FHDAD+FHDAD+79)                                        
         BNH   *+12                                                             
         MVI   FERN,16                                                          
         B     EXITL                                                            
*                                                                               
         XR    RE,RE                                                            
         IC    RE,DMP.FHLN                                                      
         AHI   RE,-(FHDAD)                                                      
         TM    DMP.FHAT,FHATXH     ANY EXTENDED HEADER?                         
         BZ    *+8                                                              
         AHI   RE,-(FHDAD)         REMOVE LENGTH OF EXTENDED HEADER             
*                                                                               
         CLM   RE,1,DMP.FHIL                                                    
         BNL   STWA0A                                                           
         TM    DMP.FHAT,FHATPR     PROTECTED AND                                
         BZ    DOTWERR5                                                         
         TM    DMP.FHII,FHIXAT     SPECIAL CODE FOR COLOR?                      
         BNO   DOTWERR5            NO, THEN ERROR                               
*                                                                               
STWA0A   NI    DMP.FHOL,X'7F'                                                   
         CLM   RE,1,DMP.FHOL                                                    
         BL    DOTWERR6                                                         
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,3,DMP.FHAD       LOAD DATA SCREEN ADDRESS                     
         BZ    DOTWERR2                                                         
         CHI   R5,(24*80-1)        ADDRESS OFF END OF SCREEN?                   
         BH    DOTWERR2                                                         
*                                                                               
         LR    R1,R5               END OF THIS FIELD OFF END OF SCREEN?         
         AR    R1,RE                                                            
         CHI   R1,(24*80)                                                       
         BH    DOTWERR4            YES                                          
*                                                                               
         TM    DMP.FHAT,FHATPR                                                  
         BO    *+12                                                             
         TM    DMP.FHOI,FHOIPR                                                  
         BZ    STWA1                                                            
         TM    DMP.FHAT,FHATMO                                                  
         BO    DOTWERR7                                                         
         TM    DMP.FHOI,FHOIMO                                                  
         BO    DOTWERR7                                                         
*                                                                               
STWA1    LH    R1,LSTSCRP                                                       
         CLM   R1,3,DMP.FHAD                                                    
         BH    DOTWERR3            THIS FIELD BEFORE PREVIOUS FIELD             
*                                                                               
         LR    R1,R5                                                            
         AR    R1,RE                                                            
         STH   R1,LSTSCRP          SAVE DISPLACEMENT TO THIS FIELD              
         LR    R0,R5                                                            
         SRDL  R0,32                                                            
         D     R0,=F'80'                                                        
         CHI   R0,80                                                            
         BH    DOTWERR8            FIELD ENDS AFTER END OF LINE                 
*                                                                               
         XR    R1,R1                                                            
         IC    R1,DMP.FHLN                                                      
         A     R1,DISP                                                          
         CHI   R1,(6*1024)         BIGGEST TWA WE ALLOW                         
         BH    DOTWERR9                                                         
*                                                                               
         CLI   VERB,2              UPPER TWA?                                   
         BNE   SHWVTWA                                                          
         CHI   R5,(17*80)          ROW 18 COL 1?                                
         BL    SHWTWA                                                           
         B     STWAMSG             UPPER PORTION MESSAGE                        
*                                                                               
SHWVTWA  CHI   R5,(07*80)          ROW 08 COL 1?                                
         BL    SHWTWANX            NO, GET THE NEXT FIELD                       
*                                                                               
SHWTWA   LA    R3,80                                                            
         XR    R4,R4                                                            
         DR    R4,R3               R5=ROW OFFSET, R4=COL OFFSET                 
         BCTR  R4,0                -1 FOR OFFSET INTO OUR TWA DATA              
*                                                                               
         CLI   VERB,3              LOWER TWA?                                   
         BNE   *+8                                                              
         AHI   R5,-7               LINE 08 IS FIRST LINE TO DISPLAY             
*                                                                               
         LA    R3,7(R3)            SKIP OVER HEADERS IN OUR TWA (79+8)          
         MR    R2,R5               R3 = LINE TO DISPLAY ON                      
         AR    R3,R4               R3 = COL ON THE LINE TO START                
         LA    R1,TWAL2H           SKIP HEADER OF ASTERIX                       
         LA    R2,0(R1,R3)         OUR ADDRESS TO START DISPLAY                 
         TM    1(R6),X'0C'         LOW INTENSITY                                
         BO    SHWTWANX               IS NOT SHOWN                              
         BCTR  RE,0                -1 FOR MVC                                   
         EX    RE,*+4                                                           
         MVC   8(0,R2),DMP.FHDA    COPY THE TEXT                                
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,C'T'           TRANSLATE WHOLE FIELD                        
         MVI   DMCB+1,C'U'                                                      
         TM    1(R6),X'40'         LOWER CASE?                                  
         BZ    *+8                 NO                                           
         MVI   DMCB+1,C'L'                                                      
         MVC   DMCB+2(1),OVSYS                                                  
         MVI   DMCB+3,0            ENGLISH, PLEASE                              
         LA    RF,8(R2)            SET ADDRESS AND LENGTH OF DATA               
         ST    RF,DMCB+4                                                        
         LA    RE,1(RE)                                                         
         STC   RE,DMCB+4                                                        
*                                                                               
         BRAS  RE,OFF31                                                         
         GOTO1 DICTATE,DMCB                                                     
         BRAS  RE,ON31                                                          
*                                                                               
SHWTWANX XR    RE,RE                                                            
         IC    RE,DMP.FHLN                                                      
         LR    R1,RE                                                            
         A     RE,DISP                                                          
         ST    RE,DISP                                                          
         AR    R6,R1                                                            
         B     STWA0                                                            
*                                                                               
STWAMSG  BRAS  RE,OFF31                                                         
         B     EXIT                                                             
*                                                                               
DOTWERR2 MVI   FERN,17                                                          
         B     EXIT                                                             
DOTWERR3 MVI   FERN,18                                                          
         B     EXIT                                                             
DOTWERR4 MVI   FERN,19                                                          
         B     EXIT                                                             
DOTWERR5 MVI   FERN,20                                                          
         B     EXIT                                                             
DOTWERR6 MVI   FERN,21                                                          
         B     EXIT                                                             
DOTWERR7 MVI   FERN,22                                                          
         B     EXIT                                                             
DOTWERR8 MVI   FERN,23                                                          
         B     EXIT                                                             
DOTWERR9 MVI   FERN,25                                                          
         B     EXIT                                                             
DOTWERRA MVI   FERN,26                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY SCREEN FROM ADDRESS IN START+DISPLACE USING OFFSET          *         
* NTRY: START    = A(START OF BUFFER)                                 *         
*       LENGTH   = L'BUFFER                                           *         
*       DISPLACE = DISPLACEMENT INTO BUFFER                           *         
*       OFFSET   = REAL ADDRESS IN CORE                               *         
***********************************************************************         
         SPACE 1                                                                
DISHEX   NTR1  ,                                                                
         BRAS  RE,ON31                                                          
         LA    R2,TWAL1H                                                        
         USING FHD,R2                                                           
         USING LIND,FHDA                                                        
         MVI   SAMIND,0                                                         
*                                                                               
DSS02    CLI   FHLN,0                                                           
         BE    DSS10                                                            
*                                                                               
         L     R3,START            R3 = A(CURRENT DISPLAY FROM BLOCK)           
         A     R3,DISPLACE                                                      
*                                                                               
         L     R4,LENGTH           R4 = AMOUNT TO DISPLAY ON THIS LINE          
         S     R4,DISPLACE                                                      
         BZ    DSS10                                                            
*                                                                               
         CHI   R4,16                                                            
         BL    *+8                                                              
         LHI   R4,16               CAN STILL DISPLAY A FULL LINE                
*                                                                               
         CHI   R4,1                MUST BE AT LEAST 2 BYTES LEFT                
         BNH   DSS06                                                            
         AHI   R4,-2                                                            
         EX    R4,DHCLC            SEE IF ENTIRE LINE IS THE SAME               
         BNE   DSS06                                                            
*                                                                               
         CLI   SAMIND,1            TEST LAST LINE ALL THE SAME ALSO             
         BNE   DSS04               NO                                           
         MVI   SAMIND,1                                                         
         BCTR  R3,0                TEST THIS LINE AND LAST LINE SAME            
         CLC   0(1,3),1(R3)                                                     
         LA    R3,1(R3)                                                         
         BNE   DSS04               DIFFERENT FROM LAST LINE                     
*                                                                               
         L     RF,DISPLACE         NEXT ROW                                     
         AHI   RF,16                                                            
         ST    RF,DISPLACE                                                      
         CLC   DISPLACE,LENGTH     MAKE SURE STILL IN BUFFER                    
         BNH   DSS02                                                            
         B     EXITOK                                                           
*                                                                               
DHCLC    CLC   1(0,R3),0(R3)                                                    
*                                                                               
DSS04    MVI   SAMIND,1                                                         
         O     R3,=X'80000000'                                                  
         GOTO1 HEXOUT,DMCB,(R3),LINCOL1,4                                       
         BRAS  RE,ON31                                                          
         MVC   LINCOL2,=CL8'--SAME--'                                           
         MVC   LINHEX(4),0(R3)                                                  
         TR    LINHEX(4),TRTAB                                                  
         B     DSS08                                                            
*                                                                               
DSS06    MVI   SAMIND,0                                                         
*                                                                               
         XC    WORK,WORK                                                        
         L     R4,LENGTH           R4 = AMOUNT TO DISPLAY ON THIS LINE          
         S     R4,DISPLACE                                                      
         CHI   R4,16                                                            
         BL    *+8                                                              
         LHI   R4,16               CAN STILL DISPLAY A FULL LINE                
*                                                                               
         O     R3,=X'80000000'                                                  
         GOTO1 HEXOUT,DMCB,(R3),WORK,(R4),0                                     
*                                                                               
         MVC   LINCOL1,WORK+00                                                  
         MVC   LINCOL2,WORK+08                                                  
         MVC   LINCOL3,WORK+16                                                  
         MVC   LINCOL4,WORK+24                                                  
*                                                                               
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   LINHEX(0),0(R3)                                                  
         EX    R4,*+4                                                           
         TR    LINHEX(0),TRTAB                                                  
*                                                                               
DSS08    L     RF,DISPLACE         START ADDRESS                                
         A     RF,OFFSET                                                        
         ST    RF,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,LINSTRT,L'FULL,0                                
*                                                                               
         L     RF,DISPLACE         NEXT ROW                                     
         AHI   RF,16                                                            
         ST    RF,DISPLACE                                                      
         CLC   DISPLACE,LENGTH     MAKE SURE STILL IN BUFFER                    
         BH    DSS10                                                            
*                                                                               
         XR    RF,RF               NEXT SCREEN LINE                             
         IC    RF,FHLN                                                          
         BXH   R2,RF,DSS02                                                      
*                                                                               
DSS10    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE FROM FACPAK LIST                                           *         
***********************************************************************         
         SPACE 1                                                                
VALFAC   NTR1  ,                                                                
         LA    R2,CRPCOREH     *** ADDRESS TO COMPARE AGAINST                   
         USING FHD,R2                                                           
         MVC   DUB,SPACES                                                       
         LLC   RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   DUB(0),FHDA                                                      
*                                                                               
         MVI   FERN,27                                                          
         LAY   R3,FACTAB                                                        
         USING FACTABD,R3                                                       
VFC02    CLI   FACNAME,X'FF'                                                    
         BE    EXITL                                                            
         CLC   DUB,FACNAME                                                      
         BE    VFC04                                                            
         LA    R3,FACTABL(R3)                                                   
         B     VFC02                                                            
*                                                                               
VFC04    L     RF,FACADDR                                                       
         CLI   FACFLAG1,LNKTABQ                                                 
         BNE   *+8                                                              
         A     RF,ALNKTAB                                                       
         CLI   FACFLAG1,SYSFTABQ                                                
         BNE   *+8                                                              
         A     RF,ASYSFACS                                                      
         CLI   FACFLAG1,COMFTABQ                                                
         BNE   *+8                                                              
         A     RF,ACOMFACS                                                      
         MVC   FULL,0(RF)                                                       
         GOTO1 HEXOUT,DMCB,FULL,FHDA,4                                          
         OI    FHOI,FHOITR                                                      
         MVI   FERN,0                                                           
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY HELP FOR A(CORE) SCREEN                                     *         
***********************************************************************         
         SPACE 1                                                                
DISHLP   NTR1  ,                                                                
         LA    R2,CRPCOREH     *** ADDRESS TO COMPARE AGAINST                   
         USING FHD,R2                                                           
         CLI   FHDA+1,C'1'                                                      
         BNL   *+8                                                              
         MVI   FHDA+1,C'1'                                                      
*                                                                               
         LLC   R0,FHDA+1                                                        
         N     R0,=X'0000000F'                                                  
         BCTR  R0,0                                                             
         MHI   R0,((23-11)*(74/(L'FACNAME+1))-1)                                
         CHI   R0,FACTABN                                                       
         BNH   *+12                                                             
         MVI   FHDA+1,C'1'                                                      
         B     DHL02                                                            
*                                                                               
         LAY   R3,FACTAB                                                        
         USING FACTABD,R3                                                       
         CHI   R0,0                                                             
         BE    DHL02                                                            
*                                                                               
         AHI   R3,FACTABL                                                       
         BCT   R0,*-4                                                           
*                                                                               
DHL02    MVI   HDRN,2                                                           
         MVI   FERN,0                                                           
*                                                                               
         LHI   R0,(10*80+1)        FIRST LINE                                   
         LA    R2,CRPLAST                                                       
         BRAS  RE,SEPLINE                                                       
*                                                                               
         LHI   RF,23-11                                                         
*                                                                               
DHL04    CLI   FACNAME,X'FF'                                                    
         BE    DHL10                                                            
*                                                                               
         MVI   FHLN,78+FHDAD                                                    
         MVI   FHAT,FHATHI+FHATPR                                               
         STCM  R0,3,FHAD                                                        
         MVI   FHII,0                                                           
         MVI   FHIL,0                                                           
         MVI   FHOI,FHOITR                                                      
         MVI   FHOL,0                                                           
         MVC   FHDA(78),SPACES                                                  
         MVI   FHDA,C'*'                                                        
         MVI   FHDA+77,C'*'                                                     
*                                                                               
         LA    R4,FHDA+3                                                        
         LHI   RE,74/(L'FACNAME+1)                                              
DHL06    CLI   FACNAME,X'FF'                                                    
         BE    DHL08                                                            
         MVC   0(L'FACNAME,R4),FACNAME                                          
         AHI   R4,L'FACNAME+1                                                   
         AHI   R3,FACTABL                                                       
         BCT   RE,DHL06                                                         
*                                                                               
DHL08    AHI   R0,80                                                            
         AHI   R2,78+FHDAD                                                      
         BCT   RF,DHL04                                                         
*                                                                               
         AHI   R4,-(L'FACNAME+1)                                                
         CLI   FACNAME,X'FF'                                                    
         BE    *+10                                                             
         MVC   0(L'FACNAME,R4),=CL8'more....'                                   
*                                                                               
DHL10    BRAS  RE,SEPLINE                                                       
         B     EXITOK                                                           
*                                                                               
SEPLINE  MVI   FHLN,78+FHDAD                                                    
         MVI   FHAT,FHATHI+FHATPR                                               
         STCM  R0,3,FHAD                                                        
         MVI   FHII,0                                                           
         MVI   FHIL,0                                                           
         MVI   FHOI,FHOITR                                                      
         MVI   FHOL,0                                                           
         MVC   FHDA(78),STARS                                                   
         AHI   R0,80                                                            
         AHI   R2,78+FHDAD                                                      
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY SCREEN LINE FORMAT                                          *         
***********************************************************************         
         SPACE 1                                                                
LIND     DSECT                     DISPLAY LINE FORMAT                          
LINSTRT  DS    CL8                 START ADDRESS                                
         DS    CL3                                                              
LINCOL1  DS    CL8                 FIRST COLUMN HEX VALUE                       
         DS    CL3                                                              
LINCOL2  DS    CL8                 SECOND COLUMN                                
         DS    CL3                                                              
LINCOL3  DS    CL8                 THIRD COLUMN                                 
         DS    CL3                                                              
LINCOL4  DS    CL8                 FOURTH COLUMN                                
         DS    CL4                                                              
LINHEX   DS    CL16                EBCDIC EQUIVALENT                            
*                                                                               
CRAP     RSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY WORKING STORAGE CHAIN                            *         
***********************************************************************         
         SPACE 1                                                                
WRKOUT   NTR1  ,                                                                
         LA    R2,TWAL1H                                                        
         USING FHD,R2                                                           
         L     R7,ACRAPBLK                                                      
         USING CRAPPERD,R7         R7=A(CRAPBLK)                                
         OI    FHAT,FHATHI                                                      
         USING RDLINED,FHDA                                                     
         MVC   RDLLBL(5),=C'Label'                                              
         MVC   RDLEYE(8),=C'Eyecatch'                                           
         MVC   RDLR1(2),=C'R1'                                                  
         MVC   RDLRB(2),=C'RB'                                                  
         MVC   RDLRD(2),=C'RD'                                                  
         MVC   RDLRE(2),=C'RE'                                                  
         MVC   RDLRF(2),=C'RF'                                                  
         MVC   RDLRERB(5),=C'RE-RB'                                             
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
*                                                                               
         BRAS  RE,ON31                                                          
         LA    R4,NDSVREG                                                       
         ICM   R0,15,DISPLACE                                                   
         BZ    WKO02                                                            
         AHI   R4,L'NDSVREG                                                     
         BCT   R0,*-4                                                           
*                                                                               
WKO02    OC    0(L'NDSVREG,R4),0(R4)                                            
         BZ    WKO16                                                            
         MVC   RDLLBL,0(R4)                                                     
*                                                                               
         CLC   =CL4'END*',RDLLBL   END OF CHAIN?                                
         BE    WKO16               YES                                          
         CLC   =CL4'STAR',RDLLBL   STORE ACCESS REGISTER                        
         BE    WKO14               YES                                          
*                                                                               
         MVC   FULL,48(R4)         THESE ADDRESSES ARE ALL 24 BIT               
         MVI   FULL,0                                                           
         CLC   FULL,LOCORE         MAKE SURE IT IS VALID                        
         BNH   WKO04                                                            
         CLC   FULL,HICORE                                                      
         BNL   WKO04                                                            
*                                                                               
         CLC   FULL,CRCPGM                                                      
         BH    *+12                                                             
         L     RF,FULL                                                          
         B     WKO03                                                            
*                                                                               
         L     RF,FULL                                                          
         S     RF,CRCPGM                                                        
         A     RF,CRAPGM                                                        
*                                                                               
WKO03    MVC   RDLEYE,22(RF)       EYECATCHER FROM RB LOCATION                  
         CLC   =X'90EC',0(RF)                                                   
         BNE   WKO04                                                            
         CLC   =X'A7F40006',16(RF)                                              
         BNE   *+10                                                             
         MVC   RDLEYE,20(RF)                                                    
         CLC   =X'A7F40006',20(RF)                                              
         BNE   *+10                                                             
         MVC   RDLEYE,24(RF)                                                    
*                                                                               
WKO04    OC    08(4,R4),08(R4)                                                  
         BZ    WKO06                                                            
         GOTO1 HEXOUT,DMCB,8(R4),RDLR1,4                                        
WKO06    OC    48(04,R4),48(R4)                                                 
         BZ    WKO08                                                            
         GOTO1 HEXOUT,DMCB,48(R4),RDLRB,4                                       
WKO08    OC    60(4,R4),60(R4)                                                  
         BZ    WKO10                                                            
         GOTO1 HEXOUT,DMCB,60(R4),RDLRE,4                                       
WKO10    OC    64(4,R4),64(R4)                                                  
         BZ    WKO12                                                            
         GOTO1 HEXOUT,DMCB,64(R4),RDLRF,4                                       
*                                                                               
WKO12    L     RF,60(R4)                                                        
         LA    RF,0(RF)                                                         
         L     RE,48(R4)                                                        
         LA    RE,0(RE)                                                         
         SR    RF,RE                                                            
         ST    RF,FULL                                                          
         OC    FULL,FULL                                                        
         BZ    WKO14                                                            
         GOTO1 HEXOUT,DMCB,FULL+1,RDLRERB,3                                     
*                                                                               
WKO14    MVC   FULL,56(R4)                                                      
         GOTO1 HEXOUT,DMCB,FULL+1,RDLRD,3                                       
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FHLN                                                        
         BZ    WKO16                                                            
         AHI   R4,L'NDSVREG                                                     
         BXH   R2,RF,WKO02                                                      
*                                                                               
WKO16    B     EXITOK                                                           
         DROP  R2,R7                                                            
         EJECT                                                                  
**********************************************************************          
* CHAIN DISPLAY LINE DSECT                                           *          
**********************************************************************          
         SPACE 1                                                                
RDLINED  DSECT                                                                  
RDLLBL   DS    CL4                 RD LABEL                                     
         DS    XL2                                                              
RDLRD    DS    CL6                 RD                                           
         DS    XL2                                                              
RDLEYE   DS    CL8                 RB EYECATCHER                                
         DS    XL2                                                              
RDLRB    DS    CL8                 RB                                           
         DS    XL2                                                              
RDLRE    DS    CL8                 RE                                           
         DS    XL2                                                              
RDLRERB  DS    CL6                 RE-RB                                        
         DS    XL2                                                              
RDLRF    DS    CL8                 RF                                           
         DS    XL2                                                              
RDLR1    DS    CL8                 R1                                           
*                                                                               
CRAP     RSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST WHETHER CORRECT TRAP HAS FIRED                      *         
***********************************************************************         
         SPACE 1                                                                
TSTFIRE  NTR1  ,                                                                
         CLI   TYPE,1              CHAIN?                                       
         BNE   TFR02               NO                                           
         L     R7,ACHANBLK                                                      
         USING CHAIND,R7           R7=A(CHANBLK)                                
         CLI   CHHIT,C'Y'          GOT HIT ON TRAP?                             
         BE    EXITOK              YES                                          
         MVI   FERN,14                                                          
         B     EXITL                                                            
*                                                                               
TFR02    L     R7,ACRAPBLK                                                      
         USING CRAPPERD,R7         R7=A(CRAPBLK)                                
         CLI   CRHIT,C'Y'          GOT HIT ON TRAP?                             
         BE    EXITOK              YES                                          
         MVI   FERN,9                                                           
         B     EXITL                                                            
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* SET ALL FIELDS TO TRANSMIT/MODIFIED                                 *         
***********************************************************************         
         SPACE 1                                                                
SETALL   NTR1  ,                                                                
         LA    R2,CRPMSGH                                                       
         USING FHD,R2                                                           
         XR    RF,RF                                                            
SALL02   ICM   RF,1,FHLN                                                        
         BZ    EXITOK                                                           
         TM    FHAT,FHATPR                                                      
         BO    SALL04                                                           
         OI    FHAT,FHATMO                                                      
         OI    FHOI,FHOITR+FHOIMO                                               
         MVC   FHOL,FHIL                                                        
SALL04   BXH   R2,RF,SALL02                                                     
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE YES/NO INPUT ON SCREEN                          *         
* NTRY: R2    = A(FIELD)                                              *         
* EXIT: BYTE  = C'Y' OR C'N' FOR YES OR NO OR 0 FOR INVALID/NO INPUT  *         
*       CC EQ = FIELD VALIDATED OK                                    *         
*       CC NE = FIELD INVALID                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING FHD,R2                                                           
VALIYN   NTR1  ,                                                                
         XC    BYTE,BYTE                                                        
         CLI   FHIL,0              INPUT TO FIELD?                              
         BE    EXITOK              NO                                           
*                                                                               
         MVC   FHOL,FHIL                                                        
         XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
*                                                                               
         MVI   BYTE,C'Y'           VALIDATE 'YES'                               
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FHDA(0),UYES                                                     
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FHDA(0),LYES                                                     
*                                                                               
         MVI   BYTE,C'N'           VALIDATE 'NO'                                
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FHDA(0),UNO                                                      
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FHDA(0),LNO                                                      
*                                                                               
         MVI   BYTE,0                                                           
         MVI   FERN,5              YES OR NO PLEASE                             
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AN 8 BYTE HEX INPUT FIELD                       *         
* NTRY: R2    = A(FIELD)                                              *         
* EXIT: FULL  = INPUT                                                 *         
*       CC EQ = FIELD VALIDATED OK                                    *         
*       CC NE = FIELD INVALID + MESSAGE SET                           *         
***********************************************************************         
         SPACE 1                                                                
         USING FHD,R2                                                           
VALIHEX  NTR1  ,                                                                
         XC    FULL,FULL                                                        
         CLI   FHIL,0              INPUT TO FIELD?                              
         BNE   *+12                NO                                           
         MVI   FERN,6                                                           
         B     EXITL                                                            
*                                                                               
         TM    FHII,FHIIHE         VALID HEX?                                   
         BO    *+12                YES                                          
         MVI   FERN,6                                                           
         B     EXITL                                                            
*                                                                               
         CLI   FHIL,8              1-8 DIGITS ONLY                              
         BNH   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         MVC   DUB,ZEROS                                                        
         LHI   RF,8                                                             
         XR    RE,RE                                                            
         IC    RE,FHIL                                                          
         SR    RF,RE                                                            
         LA    RF,DUB(RF)          RF = A(MOVE POINT)                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FHDA        JUSTIFY DATA                                 
         GOTO1 HEXIN,DMCB,DUB,FULL,L'DUB                                        
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A NUMERIC FIELD                                 *         
* NTRY: R2    = A(FIELD)                                              *         
* EXIT: FULL  = INPUT                                                 *         
*       CC EQ = FIELD VALIDATED OK                                    *         
*       CC NE = FIELD INVALID + MESSAGE SET                           *         
***********************************************************************         
         SPACE 1                                                                
         USING FHD,R2                                                           
VALINUM  NTR1  ,                                                                
         XC    FULL,FULL                                                        
         CLI   FHIL,0              INPUT TO FIELD?                              
         BNE   *+12                NO                                           
         MVI   FERN,10                                                          
         B     EXITL                                                            
*                                                                               
         TM    FHII,FHIINU         VALID HEX?                                   
         BO    *+12                YES                                          
         MVI   FERN,10                                                          
         B     EXITL                                                            
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FHIL                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FHDA(0)         PACK NUMERIC DATA                            
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY ERROR MESSAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISERR   NTR1  ,                                                                
         LA    R2,CRPMSGH                                                       
         USING FHD,R2                                                           
         MVC   CRPMSG,SPACES                                                    
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
         BNZ   DERR02              NO                                           
         XR    RF,RF                                                            
         ICM   RF,7,FERNA                                                       
         MVC   0(45,R4),0(RF)                                                   
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
         LA    R2,CRPMSGH                                                       
         USING FHD,R2                                                           
         MVC   CRPMSG,SPACES                                                    
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
         LA    RF,CRPLOADH                                                      
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     RF,ASYSFACS                                                      
         USING SYSFACD,RF                                                       
         MVC   ACRAPBLK,VCRAPBLK                                                
         MVC   ACHANBLK,VCHANBLK                                                
         MVC   ASSB,VSSB                                                        
         MVC   CALLOV,VCALLOV                                                   
         MVC   ALNKTAB,VLNKTAB                                                  
*                                                                               
         L     RF,ASSB                                                          
         USING SSBD,RF                                                          
         MVC   SYSNAME,SSBSYSN4                                                 
         MVC   LOCORE,SSBLOADR                                                  
         MVC   HICORE,SSBHIADR                                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   HEXIN,CHEXIN                                                     
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   DICTATE,CDICTATE                                                 
*                                                                               
         L     RF,ATIOB            LOAD A(TIOB)                                 
         USING TIOBD,RF                                                         
         XR    R0,R0                                                            
         IC    R0,TIOBAID          USE PFKEYS 1-12 ONLY                         
         CHI   R0,12                                                            
         BNH   *+8                                                              
         AHI   R0,-12                                                           
         STC   R0,PFKEY            SAVE PFKEY PRESSED                           
         DROP  RF                                                               
*                                                                               
         L     R0,=A(ERRMSGS)                                                   
         A     R0,RELO                                                          
         ST    R0,AERRMSGS                                                      
         L     R0,=A(OKMSGS)                                                    
         A     R0,RELO                                                          
         ST    R0,AOKMSGS                                                       
*                                                                               
         BRAS  RE,SETALL                                                        
*                                                                               
         MVI   HDRN,1              SET DEFAULT MESSAGE                          
         L     RF,AUTL                                                          
         TM    TSVCREQ-UTLD(RF),X'01'                                           
         BZ    *+8                                                              
         MVI   HDRN,10             SET FIRST DEFAULT MESSAGE                    
*                                                                               
         L     RF,ACRAPBLK                                                      
         CLI   CRTRAP-CRAPPERD(RF),C'Y'                                         
         BNE   INIT02                                                           
         MVI   HDRN,5                                                           
*                                                                               
         L     RF,ACHANBLK                                                      
         CLI   CHTRAP-CHAIND(RF),C'Y'                                           
         BNE   EXITOK                                                           
         MVI   HDRN,7                                                           
         B     EXITOK                                                           
*                                                                               
INIT02   L     RF,ACHANBLK                                                      
         CLI   CHTRAP-CHAIND(RF),C'Y'                                           
         BNE   EXITOK                                                           
         MVI   HDRN,6                                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS AND HANDY ROUTINES                                      *         
***********************************************************************         
         SPACE 1                                                                
XMOD     LA    RF,DISERR           DISPLAY APPROPRIATE MESSAGE                  
         CLI   FERN,0                                                           
         BNE   *+8                                                              
         LA    RF,DISOK                                                         
         BASR  RE,RF                                                            
         L     RD,SAVERD           AND EXIT                                     
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
*                                                                               
LOADFE   LR    R0,RE                                                            
         GOTO1 CALLOV,DMCB,(X'FE',CRPLOADH),0                                   
         LR    RE,R0                                                            
         CLI   4(R1),0                                                          
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
EMSGL    EQU   45                                                               
IMSGL    EQU   45                                                               
         LTORG                                                                  
*                                                                               
UYES     DC    CL4'YES '                                                        
UNO      DC    CL4'NO  '                                                        
LYES     DC    CL4'Yes '                                                        
LNO      DC    CL4'No  '                                                        
TABS     DC    CL4'TABS'                                                        
DMGR     DC    CL4'DMGR'                                                        
QUERY    DC    16C'?'                                                           
ZEROS    DC    16C'0'                                                           
SPACES   DC    80C' '                                                           
STARS    DC    80C'*'                                                           
*                                                                               
VF1CHN   DC    CL20'Maximum Displacement'                                       
         EJECT                                                                  
***********************************************************************         
* TRANSLATE TABLES                                                    *         
***********************************************************************         
         SPACE 1                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B8182838485868788894B4B4B4B4B4B'     80-8F                    
         DC    X'4B9192939495969798994B4B4B4B4B4B'     90-9F                    
         DC    X'4B4BA2A3A4A5A6A7A8A94B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
         EJECT                                                                  
***********************************************************************         
* MESSAGES                                                            *         
***********************************************************************         
         SPACE 1                                                                
OKMSGS   DS    0CL(IMSGL)                                                       
OK01     DC    CL(IMSGL)'Enter input fields as required'                        
OK02     DC    CL(IMSGL)'Help displayed - choose wisely'                        
OK03     DC    CL(IMSGL)'Hope it was good for you too'                          
OK04     DC    CL(IMSGL)'You''re the boss, Boss'                                
OK05     DC    CL(IMSGL)'*WARNING* Crapper Tracing Active'                      
OK06     DC    CL(IMSGL)'*WARNING* Chain Tracing Active'                        
OK07     DC    CL(IMSGL)'*WARNING* Crapper and Chain Tracing Active'            
OK08     DC    CL(IMSGL)'Let''s get chained up again soon'                      
OK09     DC    CL(IMSGL)'You''re on top of the world (Ok - the screen)'         
OK10     DC    CL(IMSGL)'Be careful'                                            
OK11     DC    CL(IMSGL)'As requested'                                          
*                                                                               
ERRMSGS  DS    0CL(EMSGL)                                                       
ERR01    DC    CL(EMSGL)'Choices are "(Cr)apper" and "(Ch)ain"'                 
ERR02    DC    CL(EMSGL)'Enter appropriate keyword or ? for help'               
ERR03    DC    CL(EMSGL)'Input field is not a valid keyword'                    
ERR04    DC    CL(EMSGL)'Choices are "(On)" "(Of)f and "(S)tatus"'              
ERR05    DC    CL(EMSGL)'Surely you can pick either "Yes" or "No"'              
ERR06    DC    CL(EMSGL)'Field requires valid Hex input'                        
ERR07    DC    CL(EMSGL)'1 - 8 digits of Hex only please'                       
ERR08    DC    CL(EMSGL)'Only "(D)MGR" and "(T)ABS" are valid'                  
ERR09    DC    CL(EMSGL)'Sorry - still constipated (no crap in sight)'          
ERR10    DC    CL(EMSGL)'Field requires a numerical length'                     
ERR11    DC    CL(EMSGL)'Field requires input'                                  
ERR12    DC    CL(EMSGL)'Chain appears corrupt - unable to display'             
ERR13    DC    CL(EMSGL)'"DMGR" and "TABS" are the only choices here'           
ERR14    DC    CL(EMSGL)'Sorry - still in bondage (chains not broken)'          
ERR15    DC    CL(EMSGL)'Start of chain is bad - unable to display'             
ERR16    DC    CL(EMSGL)'Field length > length of screen line'                  
ERR17    DC    CL(EMSGL)'Field length less than 9'                              
ERR18    DC    CL(EMSGL)'Screen position overlaps last field'                   
ERR19    DC    CL(EMSGL)'Field starts beyond screen end'                        
ERR20    DC    CL(EMSGL)'Field ends beyond screen end'                          
ERR21    DC    CL(EMSGL)'Input length is too big'                               
ERR22    DC    CL(EMSGL)'Output length is too big'                              
ERR23    DC    CL(EMSGL)'Trying to modify protected field'                      
ERR24    DC    CL(EMSGL)'Field goes over end of screen line'                    
ERR25    DC    CL(EMSGL)'Field extends beyond TWA max length'                   
ERR26    DC    CL(EMSGL)'Protected field cannot use 1st column'                 
ERR27    DC    CL(EMSGL)'You haven''t input anything I can resolve'             
ERR28    DC    CL(EMSGL)'Just type something will you'                          
*                                                                               
ERRBIG   DC    C'Value input is too large - Max is:'                            
         EJECT                                                                  
***********************************************************************         
* TABLE OF FACPAK ADDRESSES                                           *         
***********************************************************************         
         SPACE 1                                                                
FACTAB   DS    0D                                                               
         DC    CL08'ADDAFT  ',A(VADDAFT-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'ADDAY   ',A(CADDAY-COMFACSD),AL1(COMFTABQ,0,0,0)            
         DC    CL08'ADRBUFF ',A(VADRBUFF-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'ADRFILE ',A(VADRFILE-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'ARREDIT ',A(VARREDIT-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'BILLIT  ',A(CBILLIT-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'BINSRCH ',A(CBINSRCH-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'BLDCUR  ',A(CBLDCUR-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'CALLOV  ',A(VCALLOV-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'CARDS   ',A(LCARDS-LNKTABD),AL1(LNKTABQ,0,0,0)              
         DC    CL08'CASHVAL ',A(CCASHVAL-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&UK*&& DC    CL08'CASHVALA',A(CCASHVA-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'CHANBLK ',A(VCHANBLK-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'CHKOUT  ',A(VCHKOUT-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'CHKPT1  ',A(VCHKPT1-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'CHKPT1X ',A(VCHKPT1X-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'CHKPT2  ',A(VCHKPT2-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'CHKPT2X ',A(VCHKPT2X-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'CLSESYS ',A(VCLSESYS-SYSFACD),AL1(SYSFTABQ,0,0,0)           
*&&UK*&& DC    CL08'CONVERT ',A(CCONVERT-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'CRAPBLK ',A(VCRAPBLK-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'CUREDIT ',A(CCUREDIT-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'DABACK  ',A(VDABACK-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'DACPUID ',A(VDACPUID-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'DADDS   ',A(VDADDS-SYSFACD),AL1(SYSFTABQ,0,0,0)             
         DC    CL08'DARPT   ',A(VDARPT-SYSFACD),AL1(SYSFTABQ,0,0,0)             
         DC    CL08'DATAMGR ',A(CDATAMGR-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'DATAMGR ',A(VDATAMGR-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'DATCON  ',A(CDATCON-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'DATRNS  ',A(VDATRNS-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'DATTIM  ',A(CDATTIM-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'DATVAL  ',A(CDATVAL-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'DB2IO   ',A(CDB2IO-COMFACSD),AL1(COMFTABQ,0,0,0)            
*&&US*&& DC    CL08'DDLINK  ',A(CDDLINK-COMFACSD),AL1(COMFTABQ,0,0,0)           
*&&UK*&& DC    CL08'DDLINK  ',A(CDDLINK-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'DDS00900',A(LDS00900-LNKTABD),AL1(LNKTABQ,0,0,0)            
         DC    CL08'DDS00901',A(LDS00901-LNKTABD),AL1(LNKTABQ,0,0,0)            
         DC    CL08'DECBLST ',A(VDECBLST-SYSFACD),AL1(SYSFTABQ,0,0,0)           
*&&US*&& DC    CL08'DEFINE  ',A(CDEFINE-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'DEJAVU  ',A(CDEJAVU-COMFACSD),AL1(COMFTABQ,0,0,0)           
*&&US*&& DC    CL08'DEMADDR ',A(CDEMADDR-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&US*&& DC    CL08'DEMAINT ',A(CDEMAINT-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&US*&& DC    CL08'DEMAND  ',A(CDEMAND-COMFACSD),AL1(COMFTABQ,0,0,0)           
*&&US*&& DC    CL08'DEMAND1 ',A(CDEMAND1-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&US*&& DC    CL08'DEMEL   ',A(CDEMEL-COMFACSD),AL1(COMFTABQ,0,0,0)            
*&&US*&& DC    CL08'DEMOMTH ',A(CDEMOMTH-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&US*&& DC    CL08'DEMOUT  ',A(CDEMOUT-COMFACSD),AL1(COMFTABQ,0,0,0)           
*&&US*&& DC    CL08'DEMOVAL ',A(CDEMOVAL-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&US*&& DC    CL08'DEMOCON ',A(CDEMOCON-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&US*&& DC    CL08'DEMTABS ',A(CDEMTABS-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&US*&& DC    CL08'DEMTBOF ',A(CDEMTBOF-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'DICTATE ',A(CDICTATE-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&US*&& DC    CL08'DLFLD   ',A(CDLFLD-COMFACSD),AL1(COMFTABQ,0,0,0)            
*&&UK*&& DC    CL08'DLFLD   ',A(CDLFLD-COMFACSD),AL1(COMFTABQ,0,0,0)            
         DC    CL08'DMACCEMU',A(LACCEMU-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'DMDALINK',A(LDALINK-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'DMDANDX ',A(LDANDX-LNKTABD),AL1(LNKTABQ,0,0,0)              
         DC    CL08'DMDAPTRS',A(LDAPTRS-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'DMDTFIOA',A(LDTFIOA-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'DMDTFS  ',A(LDTFS-LNKTABD),AL1(LNKTABQ,0,0,0)               
         DC    CL08'DMDYNDD ',A(LDYNDD-LNKTABD),AL1(LNKTABQ,0,0,0)              
         DC    CL08'DMENQCTL',A(LENQCTL-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'DMGRFLS ',A(VDMGRFLS-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'DMISDDS ',A(LISDDS-LNKTABD),AL1(LNKTABQ,0,0,0)              
         DC    CL08'DMLOCKER',A(LLOCKER-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'DMOD000 ',A(VDMOD000-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'DMPFILE ',A(VDMPFILE-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'DMPRTQ  ',A(LDMPRTQ-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'DMPRTQO ',A(LDMPRTQO-LNKTABD),AL1(LNKTABQ,0,0,0)            
         DC    CL08'DMRCVR  ',A(VDMRCVR-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'DMWRKF  ',A(LDMWRKF-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'DMWRKR  ',A(LDMWRKR-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'DTCNV   ',A(LDTCNV-LNKTABD),AL1(LNKTABQ,0,0,0)              
         DC    CL08'DTFIOA  ',A(VDTFIOA-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'ECBLST  ',A(VECBLST-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'EDITOR  ',A(CEDITOR-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'EKEY    ',A(VEKEY-SYSFACD),AL1(SYSFTABQ,0,0,0)              
         DC    CL08'ENQDEQ  ',A(VENQDEQ-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'EUREKA  ',A(CEUREKA-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'FAABEND ',A(LABEND-LNKTABD),AL1(LNKTABQ,0,0,0)              
         DC    CL08'FABSAM  ',A(CFABSAM-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'FALOAD  ',A(LFALOAD-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FAMNTR  ',A(LFAMNTR-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FAMSGQIN',A(LMSGQIN-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FASCRNCH',A(LSCRNCH-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FASECRET',A(LSECRET-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FATAB   ',A(LFATAB-LNKTABD),AL1(LNKTABQ,0,0,0)              
         DC    CL08'FATASKER',A(LTASKER-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FATIADDS',A(LTIADDS-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FATISTR ',A(LFATISTR-LNKTABD),AL1(LNKTABQ,0,0,0)            
         DC    CL08'FATI3270',A(LTI3270-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FATOADDS',A(LTOADDS-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FATOSCRP',A(LTOSCRP-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FATOSTR ',A(LTOSTR-LNKTABD),AL1(LNKTABQ,0,0,0)              
         DC    CL08'FATO3270',A(LTO3270-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FATWASVR',A(LTWASVR-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'FINDSYS ',A(VFINDSYS-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'FNDEOF  ',A(VFNDEOF-SYSFACD),AL1(SYSFTABQ,0,0,0)            
*&&US*&& DC    CL08'GENERAL ',A(CGENERAL-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&UK*&& DC    CL08'GENERAL ',A(CGENERAL-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&US*&& DC    CL08'GENNEW  ',A(CGENNEW-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'GETCUR  ',A(CGETCUR-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'GETDAY  ',A(CGETDAY-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'GETFACT ',A(CGETFACT-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'GETHELP ',A(CGETHELP-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'GETMSG  ',A(CGETMSG-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'GETNAR  ',A(CGETNAR-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'GETPROF ',A(CGETPROF-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&UK*&& DC    CL08'GETRAD  ',A(CGETRAD-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'GETRET  ',A(CGETRET-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'GETTXT  ',A(CGETTXT-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'GLOBBER ',A(CGLOBBER-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'HELEN   ',A(CHELEN-COMFACSD),AL1(COMFTABQ,0,0,0)            
         DC    CL08'HELLO   ',A(CHELLO-COMFACSD),AL1(COMFTABQ,0,0,0)            
         DC    CL08'HEXIN   ',A(CHEXIN-COMFACSD),AL1(COMFTABQ,0,0,0)            
         DC    CL08'HEXOUT  ',A(CHEXOUT-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'ISCPUID ',A(VISCPUID-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'JESMAIL ',A(VJESMAIL-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'LCBUFFS ',A(VLCBUFFS-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'LCBUFFX ',A(VLCBUFFX-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'LCLFACS ',A(CLCLFACS-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'LCWRITE ',A(VLCWRITE-SYSFACD),AL1(SYSFTABQ,0,0,0)           
*&&UK*&& DC    CL08'LIMACC  ',A(CLIMACC-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'LINKIO  ',A(CLINKIO-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'LNKTABD  ',A(VLNKTAB-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'LOADER  ',A(LLOADER-LNKTABD),AL1(LNKTABQ,0,0,0)             
         DC    CL08'LOCKER  ',A(VLOCKER-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'LOCKET  ',A(CLOCKET-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'LOCKSPC ',A(VLOCKSPC-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'LOCKTAB ',A(VLOCKTAB-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'LOCKUP  ',A(CLOCKUP-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'LOGGER  ',A(VLOGGER-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'MASTC   ',A(CMASTC-COMFACSD),AL1(COMFTABQ,0,0,0)            
*&&UK*&& DC    CL08'MEDFACS ',A(CMEDFACS-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'MINIO   ',A(CMINIO-COMFACSD),AL1(COMFTABQ,0,0,0)            
         DC    CL08'MQIO    ',A(CMQIO-COMFACSD),AL1(COMFTABQ,0,0,0)             
         DC    CL08'MQIPARM ',A(VMQIPARM-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'MQRPT   ',A(CMQRPT-COMFACSD),AL1(COMFTABQ,0,0,0)            
         DC    CL08'OFFLAL  ',A(COFFLAL-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'OPEN    ',A(VOPEN-SYSFACD),AL1(SYSFTABQ,0,0,0)              
         DC    CL08'OPENIS  ',A(VOPENIS-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'OPENSYS ',A(VOPENSYS-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'PARSNIP ',A(CPARSNIP-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'PERSON  ',A(CPERSON-COMFACSD),AL1(COMFTABQ,0,0,0)           
*&&US*&& DC    CL08'PERVAL  ',A(CPERVAL-COMFACSD),AL1(COMFTABQ,0,0,0)           
*&&UK*&& DC    CL08'PERVAL  ',A(CPERVAL-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'PERVERT ',A(CPERVERT-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'PHLIST  ',A(VPHLIST-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'POWWOW  ',A(VPOWWOW-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'PQPROF  ',A(CPQPROF-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'PRGMS   ',A(VPRGMS-SYSFACD),AL1(SYSFTABQ,0,0,0)             
*&&UK*&& DC    CL08'PRORATA ',A(CPRORATA-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'PROTOFF ',A(CPROTOFF-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'PROTON  ',A(CPROTON-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'PRQ     ',A(VPRQ-SYSFACD),AL1(SYSFTABQ,0,0,0)               
         DC    CL08'PRTQUE  ',A(VPRTQUE-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'RCTYPE  ',A(VRCTYPE-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'RDID    ',A(VRDID-SYSFACD),AL1(SYSFTABQ,0,0,0)              
         DC    CL08'READ    ',A(VREAD-SYSFACD),AL1(SYSFTABQ,0,0,0)              
         DC    CL08'RECUP   ',A(CRECUP-COMFACSD),AL1(COMFTABQ,0,0,0)            
         DC    CL08'REPORT  ',A(CREPORT-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'RKEY    ',A(VRKEY-SYSFACD),AL1(SYSFTABQ,0,0,0)              
         DC    CL08'RUNIT   ',A(VRUNIT-SYSFACD),AL1(SYSFTABQ,0,0,0)             
         DC    CL08'SCANNER ',A(CSCANNER-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'SCRIPT  ',A(CSCRIPT-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'SCUNKEY ',A(CSCUNKEY-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&US*&& DC    CL08'SEARCH  ',A(CSEARCH-COMFACSD),AL1(COMFTABQ,0,0,0)           
*&&UK*&& DC    CL08'SEARCH  ',A(CSEARCH-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'SECRET  ',A(CSECRET-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'SELIST  ',A(VSELIST-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'SHIPIT  ',A(VSHIPIT-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'SKIPA   ',A(VSKIPA-SYSFACD),AL1(SYSFTABQ,0,0,0)             
*&&US*&& DC    CL08'SOFDAT  ',A(CSOFDAT-COMFACSD),AL1(COMFTABQ,0,0,0)           
*&&UK*&& DC    CL08'SOFDAT  ',A(CSOFDAT-COMFACSD),AL1(COMFTABQ,0,0,0)           
*&&US*&& DC    CL08'SPGTIUN ',A(CSPGTIUN-COMFACSD),AL1(COMFTABQ,0,0,0)          
*&&UK*&& DC    CL08'SRCHCAL ',A(CSRCHCAL-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'SSB     ',A(VSSB-SYSFACD),AL1(SYSFTABQ,0,0,0)               
         DC    CL08'SWITCH  ',A(CSWITCH-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'SYSFAC0 ',A(VSYSFAC0-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'SYSFAC1 ',A(VSYSFAC1-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'SYSFAC2 ',A(VSYSFAC2-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'SYSFAC3 ',A(VSYSFAC3-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'SYSFAC4 ',A(VSYSFAC4-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'TCB     ',A(VTCB-SYSFACD),AL1(SYSFTABQ,0,0,0)               
         DC    CL08'TEMPSTR ',A(VTEMPSTR-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'TEMPTRC ',A(VTEMPTRC-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'TERMVAL ',A(CTERMVAL-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'TICTOC  ',A(VTICTOC-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'TICTOCT ',A(VTICTOCT-SYSFACD),AL1(SYSFTABQ,0,0,0)           
*&&UK*&& DC    CL08'TOBACCO ',A(CTOBACCO-COMFACSD),AL1(COMFTABQ,0,0,0)          
         DC    CL08'TSTRCVR ',A(VTSTRCVR-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'TSTTAB  ',A(VTSTTAB-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'TWASVR  ',A(VTWASVR-SYSFACD),AL1(SYSFTABQ,0,0,0)            
*&&US*&& DC    CL08'T00AD0  ',A(CT00AD0-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'UNSCAN  ',A(CUNSCAN-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'UPDTAB  ',A(VUPDTAB-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'VEMAIL  ',A(CVEMAIL-COMFACSD),AL1(COMFTABQ,0,0,0)           
         DC    CL08'VIRTRM  ',A(VVIRTRM-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'VRSNTAB ',A(VVRSNTAB-SYSFACD),AL1(SYSFTABQ,0,0,0)           
         DC    CL08'VUTL    ',A(VUTL-SYSFACD),AL1(SYSFTABQ,0,0,0)               
         DC    CL08'WCTYPE  ',A(VWCTYPE-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'WKEY    ',A(VWKEY-SYSFACD),AL1(SYSFTABQ,0,0,0)              
         DC    CL08'WKFILE  ',A(VWKFILE-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'WRITE   ',A(VWRITE-SYSFACD),AL1(SYSFTABQ,0,0,0)             
         DC    CL08'WRTAFT  ',A(VWRTAFT-SYSFACD),AL1(SYSFTABQ,0,0,0)            
         DC    CL08'WSSVR   ',A(VWSSVR-SYSFACD),AL1(SYSFTABQ,0,0,0)             
         DC    CL08'WTCKD   ',A(VWTCKD-SYSFACD),AL1(SYSFTABQ,0,0,0)             
         DC    CL08'WTID    ',A(VWTID-SYSFACD),AL1(SYSFTABQ,0,0,0)              
         DC    CL08'ZIPFAC  ',A(VZIPFAC-SYSFACD),AL1(SYSFTABQ,0,0,0)            
FACTABN  EQU   (*-FACTAB)/FACTABL                                               
         DC    X'FF'                                                            
*                                                                               
LNKTABQ  EQU   1                                                                
SYSFTABQ EQU   2                                                                
COMFTABQ EQU   3                                                                
*                                                                               
FACTABD  DSECT                                                                  
FACNAME  DS    CL8                                                              
FACADDR  DS    A                                                                
FACFLAG1 DS    X                                                                
FACFLAG2 DS    X                                                                
FACFLAG3 DS    X                                                                
FACFLAG4 DS    X                                                                
FACTABL  EQU   *-FACTABD                                                        
*                                                                               
CRAP     RSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
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
RELO     DS    F                                                                
SAVERD   DS    F                                                                
HEXIN    DS    A                                                                
HEXOUT   DS    A                                                                
DICTATE  DS    A                                                                
ALNKTAB  DS    A                                                                
*                                                                               
CALLOV   DS    A                                                                
ACRAPBLK DS    A                                                                
ACHANBLK DS    A                                                                
ASSB     DS    A                                                                
*                                                                               
START    DS    A                   START ADDRESS FOR HEX DISPLAY                
LENGTH   DS    F                   LENGTH OF BUFFER                             
DISPLACE DS    F                   DISPLACEMENT FROM START                      
OFFSET   DS    F                   REAL START ADDRESS (FOR CORRECTION)          
*                                                                               
                                                                                
FADRH    DS    F                   ADDR OF ERROR FIELD HEADER                   
FERN     DS    X                   ERROR NUMBER (FOR MESSAGE)                   
FERNA    DS    XL3                 ADDR OF ERROR MSG IF FERN IS 255             
FERRDSP  DS    X                   DISPLACEMENT TO ERROR IN FIELD               
HDRN     DS    X                   HEADER MESSAGE NUMBER (NO ERROR)             
*                                                                               
AERRMSGS DS    A                                                                
AOKMSGS  DS    A                                                                
LOCORE   DS    A                                                                
HICORE   DS    A                                                                
SYSNAME  DS    CL4                                                              
*                                                                               
FULL     DS    F                                                                
FULL1    DS    F                                                                
DMCB     DS    6F                                                               
DISP     DS    F                                                                
LSTSCRP  DS    H                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
TYPE     DS    X                                                                
SAMIND   DS    X                                                                
VERB     DS    X                                                                
OVSYS    DS    X                                                                
PFKEY    DS    X                                                                
TEMP     DS    CL16                                                             
WORK     DS    CL256                                                            
*                                                                               
         DS    0D                                                               
NDSVRCNT EQU   144                                                              
NDSVREG  DS    (NDSVRCNT)XL72      SR(X) REGISTERS FROM TCB CHAIN               
NDSVREGL EQU   *-NDSVREG                                                        
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECTS                                                       *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE SRCRPFFD                                                       
         ORG   CRPLOADH                                                         
       ++INCLUDE SRCRPFED                                                       
         ORG   CRPLOADH                                                         
       ++INCLUDE SRCRPFDD                                                       
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* DDCRAPPERD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCRAPPERD                                                     
         PRINT ON                                                               
* DDCHAIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCHAIND                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FALNKTAB                                                                      
         PRINT OFF                                                              
       ++INCLUDE FALNKTAB                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'215SRCRP00   02/23/09'                                      
         END                                                                    
