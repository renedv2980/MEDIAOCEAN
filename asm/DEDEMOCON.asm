*          DATA SET DEDEMOCON  AT LEVEL 032 AS OF 07/17/19                      
*PHASE T00AE0C                                                                  
*INCLUDE BINSR31                                                                
         TITLE 'DEMOCON - CONVERT DEMO EXPRESSIONS'                             
DEMOCON  RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 DEMWORKL,**DEMCON,CLEAR=YES,RR=R2                                
         USING DEMWORK,RC          RC=A(W/S)                                    
         LAM   AR0,ARF,=16F'0'                                                  
         ST    R2,RELO                                                          
         LR    R2,R1                                                            
         USING PARMD,R2            R2=A(PARM LIST)                              
         LM    R7,R9,AINPUT        R7=A(INPUT),R8=A(OUTPUT)                     
         USING DBLOCKD,R9          R9=A(DBLOCK)                                 
         L     RA,DBCOMFCS         RA=A(COMMON FACILITIES)                      
         USING COMFACSD,RA                                                      
*                                                                               
         L     RF,=V(BINSRCH)                                                   
         A     RF,RELO                                                          
         ST    RF,VBINSRCH                                                      
*                                                                               
         BAS   RE,SETPLDTB         SET PERSONAL LANG. TABLE ADDRESSES           
         MVI   CSFLAGS,0           COMSCORE DEMO FLAGS                          
*                                                                               
         CLI   TOUTPUT,DEMOCON_20  SPECIAL COMSCORE LOOKUP                      
         JNE   DEM1A                                                            
         CLI   TINPLEN,C'C'        RETREIVE COMSCORE INFO BY CATEGORY?          
         JE    *+12                                                             
         CLI   TINPLEN,C'N'        RETRIEVE COMSCORE INFO BY DEMO NUM?          
         JNE   DEMERR                                                           
         BAS   RE,GETTAB                                                        
         BRAS  RE,GETRTK           TEST/GET FULL COMSCORE ENTRY                 
         JNE   DEMERR                                                           
         J     DEMX                                                             
*                                                                               
DEM1A    CLI   TOUTPUT,DEMOCON_14  RETURN A(PERSONAL LANGUAGE TABLES)?          
         BE    DEM1E               YES: ONLY P2 IS SET                          
*                                                                               
         CLI   TINPLEN,X'FF'       TEST OLD SPOT DEMO NUMBER                    
         BNE   DEM1E                                                            
         MVI   TINPLEN,1           RESET LEN                                    
         ZIC   RE,0(R7)            GET NUMBER                                   
         BCTR  RE,0                                                             
         AR    RE,RE               X2                                           
         LARL  R0,SPDEMTAB                                                      
         AR    RE,R0                                                            
         MVC   DEMOEXP+1(2),0(RE)  MOVE EXPRESSION                              
         LA    R7,DEMOEXP          AND RESET INPUT POINTER                      
*                                                                               
DEM1E    SR    R0,R0                                                            
         ICM   R0,1,TINPLEN                                                     
         BNZ   *+8                                                              
         LA    R0,1                DEFAULT TO ONE INPUT FIELD                   
         STH   R0,INPCOUNT         AND SAVE                                     
         MVI   TINPLEN,0           CLEAR INPUT LEN                              
         CLI   TOUTPUT,MOUTPUT     TEST OUTPUT TYPE VALID                       
         BH    DEMERR                                                           
*                                                                               
         CLI   TOUTPUT,DEMOCON_14  RETURN A(PLD TABLES)                         
         BE    DEM5                THERE IS NO INPUT ADDRESS IN P1!             
*                                                                               
* GO TO OUTPUT ROUTINE                                                          
*                                                                               
DEM2     BRAS  RE,TSTCS            TEST COMSCORE DEMO                           
         JNE   *+8                                                              
         BRAS  RE,GETCS            GET COMSCORE DEMO NAME                       
*                                                                               
         MVC   DEMOEXP,0(R7)       MOVE INPUT DATA                              
         MVI   DEMOGRP,0                                                        
*                                                                               
         CLI   DBDEMTYP,C'4'       REFORMAT 4 CHAR INPUT                        
         BNE   DEM2A                                                            
         MVC   DEMONUM,3(R7)                                                    
         MVC   DEMOGRP,2(R7)       4 CHAR DEMO GROUP                            
         B     DEM2B                                                            
*                                                                               
DEM2A    CLI   DBDEMTYP,C'P'       REFORMAT 3 BYTE PACKED INPUT                 
         BNE   DEM3                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,1(R7)          GET 2 BYTE PACKED PORTION                    
         SLDL  RE,21               DIG OUT MODIFIER                             
         LA    R1,CTRTAB-1(RE)     CONVERT MODIFIER                             
         MVC   DEMOMOD,0(R1)       SET MODIFIER                                 
         SR    RE,RE                                                            
         SLDL  RE,4                DIG OUT GROUP CODE                           
         STC   RE,DEMOGRP          SAVE GROUP CODE                              
         SRL   RF,25               SAVE DEMO CODE                               
         STC   RF,DEMONUM                                                       
*                                                                               
DEM2B    CLI   DEMOGRP,1           0 AND 1 ARE OLD NUMBERS                      
         BNE   DEM3                                                             
         LLC   R1,DEMONUM          NUMBERS RUN FROM 1-255                       
         LA    R1,128(R1)                                                       
         STC   R1,DEMONUM                                                       
         MVI   DEMOGRP,0           NO GROUP FOR NAME LOOKUP                     
*                                                                               
DEM3     DS    0H                                                               
         LLC   RF,DEMOMOD          EXTRACT EBCDIC PLD CHARACTER                 
         A     RF,APLPLDTB                                                      
         MVC   PLDCHAR,0(RF)       SAVE IT (COULD BE A SPACE)                   
         LLC   RF,DEMOMOD          EXTRACT EBCDIC MODIFIER                      
         A     RF,APLMODTB                                                      
         MVC   DEMOMOD,0(RF)       REPLACE ENCODED W/ DECODED MODIFIER          
*                                                                               
         BRAS  RE,FINDCAT          FIND THE CATEGORY FOR NAD                    
*                                                                               
DEM5     DS    0H                                                               
         ZIC   R1,TOUTPUT                                                       
         MHI   R1,4                                                             
         B     *+4(R1)             GO TO BRANCH INSTRUCTION                     
*                                                                               
         B     DEM10               OUTPUT TYPE 0                                
         B     DEM20                           1                                
         B     DEM30                           2                                
         B     DEM40                           3                                
         B     DEM50                           4                                
         B     DEM60                           5                                
         B     DEM70                           6                                
         B     DEM80                           7                                
         B     DEM90                           8                                
         B     DEM100                          9                                
         B     DEM110                          10                               
         B     DEM120                          11                               
         B     DEM110                          12                               
         B     DEM110                          13                               
         B     DEM140                          14 (DEMOCON_14)                  
         B     DEM150                          15 (DEMOCON_15)                  
         B     DEM160                          16 (DEMOCON_16)                  
         B     DEM170                          17 (DEMOCON_17)                  
         B     DEM180                          18 (DEMOCON_18)                  
         EJECT                                                                  
DEMX     SAC   0                                                                
         LAM   AR0,ARF,=16F'0'                                                  
*                                                                               
         CLI   TOUTPUT,DEMOCON_20  SPECIAL COMSCORE LOOKUP?                     
         BE    EXIT                YES: NOTHING ELSE TO DO                      
         CLI   TOUTPUT,DEMOCON_14  RETURN A(PERSONAL LANGUAGE TABLES)?          
         BE    EXIT                YES: NOTHING ELSE TO DO                      
*                                                                               
         AHI   R7,3                NEXT INPUT FIELD                             
         CLI   DBDEMTYP,C'4'       4 BYTE DEMO INPUT                            
         BNE   *+8                                                              
         LA    R7,1(R7)                                                         
*                                                                               
         LH    R0,INPCOUNT                                                      
         SHI   R0,1                                                             
         BZ    EXIT                                                             
*                                                                               
         CLI   TOUTPUT,DEMOCON_16  FOR PLD ENCODE/DECODE...                     
         BE    DEMX05              ...PROCESS THE ENTIRE LIST!                  
         CLI   TOUTPUT,DEMOCON_17                                               
         BE    DEMX05                                                           
*                                                                               
         CLI   DBDEMTYP,C'4'       4 BYTE DEMO INPUT                            
         BNE   DEMX02                                                           
         CLI   0(R7),X'FF'         END OF LIST?                                 
         BE    EXIT                                                             
         OC    1(3,R7),1(R7)                                                    
         JZ    EXIT                                                             
         J     DEMX05                                                           
*                                                                               
DEMX02   CLI   1(R7),0             TEST MORE INPUT                              
         BE    EXIT                NO                                           
*                                                                               
DEMX05   DS    0H                                                               
         CLI   TOUTPUT,9           SCREEN OUTPUT                                
         BNE   DEMX10                                                           
         MVI   0(R8),C','          SEPARATE ENTRIES WITH COMMA                  
         LA    R8,1(R8)                                                         
         ICM   R0,1,TINPLEN                                                     
         LA    R0,1(R0)                                                         
         STC   R0,TINPLEN                                                       
*                                                                               
DEMX10   LH    R0,INPCOUNT                                                      
         SHI   R0,1                                                             
         STH   R0,INPCOUNT                                                      
         BNZ   DEM2                                                             
*                                                                               
DEMERR   MVI   TINPLEN,FF          RETURN ERROR                                 
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INTERNAL FORMAT - 3BYTE INTERNAL FORMAT                             *         
***********************************************************************         
*                                                                               
DEM10    MVC   0(3,R8),DEMOEXP                                                  
         LA    R8,3(R8)                                                         
         B     DEMX                                                             
*                                                                               
***********************************************************************         
* INTERNAL FORMAT - 1BYTE SPOT DEMO NUMBER                            *         
***********************************************************************         
*                                                                               
DEM20    LARL  RE,SPDEMTAB         RE=A(SPOT DEMO TABLE)                        
         LA    R1,1                                                             
*                                                                               
DEM22    CLI   0(RE),FF            TEST E-O-T                                   
         BE    DEMERR                                                           
         CLC   0(2,RE),DEMOEXP+1                                                
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    RE,L'SPDEMTAB(RE)                                                
         B     DEM22                                                            
*                                                                               
DEM24    STC   R1,0(R8)            RETURN DEMO NUMBER                           
         LA    R8,1(R8)                                                         
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* EXTERNAL FORMAT - SPOTPAK EDITED                                    *         
***********************************************************************         
*                                                                               
         USING MODDTAD,R3                                                       
         USING NAMDTAD,R4                                                       
*                                                                               
DEM30    TM    CSFLAGS,CSFDEMO     COMSCORE DEMO?                               
         JO    DEM115                                                           
*                                                                               
         CLI   1(R7),63            TEST WEIGHTED DEMO                           
         BE    DEM30A                                                           
         CLI   1(R7),X'21'         TEST USER DEMO                               
         BNE   DEM31                                                            
*                                                                               
* USER DEMO - SET DEFAULT NAME                                                  
*                                                                               
         MVC   0(4,R8),=C'USER'                                                 
         MVC   4(1,R8),2(R7)       MOVE NUMBER                                  
         CLI   2(R7),C'A'          TEST EXTENDED USER DEMOS                     
         BNL   *+8                                                              
         OI    4(R8),X'F0'         MAKE EBCDIC                                  
         B     DEM30B                                                           
*                                                                               
* WEIGHTED DEMO - SET DEFAULT NAME                                              
*                                                                               
DEM30A   MVC   0(7,R8),=C'WEIGHTD'                                              
*                                                                               
DEM30B   CLI   TCNTRL,C'S'         TEST SPOTPAK REQUEST                         
         BNE   DEM35                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,2(R7)          GET DEMO NUMBER                              
         CLI   1(R7),63                                                         
         BNE   *+8                                                              
         LA    RE,5                WEIGHTED DEMO IS FIFTH USER NAME             
         BCTR  RE,0                                                             
         MHI   RE,7                                                             
         CLI   2(R7),C'A'          TEST EXTENDED USER DEMO                      
         BL    DEM30C                                                           
         BRAS  RE,GETEUD                                                        
         MVC   0(6,R8),0(R1)                                                    
         MVI   6(R8),C' '                                                       
         B     DEM35                                                            
*                                                                               
DEM30C   XR    RF,RF                                                            
         ICM   RF,7,AUSRNMS+1                                                   
         BZ    DEM35                                                            
         AR    RE,RF               POINT TO USER NAME                           
         MVC   0(7,R8),0(RE)       MOVE 7 CHARS                                 
         CLI   AUSRNMS,C'Y'        TEST USER DEMO SEQNUM PRESENT                
         BNE   *+14                NO -                                         
         MVC   0(6,R8),1(RE)       ELSE PRINT LAST 6 CHARS                      
         MVI   6(R8),C' '                                                       
         B     DEM35                                                            
*                                                                               
DEM31    BAS   RE,GETTAB                                                        
         CLI   TCNTRL,C'S'         TEST FOR SPOTPAK CALL                        
         BE    DEM32                                                            
         CLI   DEMOMOD,DEMO_MODIFIER_T  TEST FOR REP IMPS 'T'                   
         BNE   DEM32                                                            
         TM    DEMOSTAT,X'20'      SUPPRESS THE MODIFIER ON OUTPUT              
         BNZ   *+14                                                             
         MVC   0(7,R8),NAM7                                                     
         B     DEM35                                                            
         MVI   0(R8),C'$'                                                       
         MVC   1(6,R8),NAM6                                                     
         B     DEM35                                                            
*                                                                               
DEM32    TM    DEMOSTAT,X'20'                                                   
         BNZ   DEM34                                                            
         MVC   0(1,R8),MODMOD                                                   
         MVC   1(6,R8),NAM6                                                     
         CLI   DEMOMOD,DEMO_MODIFIER_I TEST FOR SPOT IMP                        
         BNE   DEM35                                                            
         MVC   0(6,R8),NAM6        SUPPRESS THE MODIFIER ON OUTPUT              
         MVI   6(R8),C' '                                                       
         B     DEM35                                                            
*                                                                               
DEM34    MVI   0(R8),C'$'                                                       
         MVC   1(1,R8),MODMOD                                                   
         MVC   2(5,R8),NAM5                                                     
         CLI   DEMOMOD,DEMO_MODIFIER_I TEST FOR SPOT IMP                        
         BNE   DEM35                                                            
         MVC   1(6,R8),NAM6        SUPPRESS THE MODIFIER                        
*                                                                               
DEM35    LA    R8,7(R8)            POINT TO END OF STRING                       
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* EXTERNAL FORMAT - 5/5/5 (WOMEN18-49(SHR))                           *         
***********************************************************************         
*                                                                               
DEM40    BAS   RE,GETTAB                                                        
         MVC   0(10,R8),NAM5BY5                                                 
         TM    CSFLAGS,CSFDEMO     COMSCORE DEMO?                               
         JZ    *+16                                                             
         MVC   0(8,R8),CSDEMNAM    COMSCORE DEMO NAME                           
         XC    8(2,R8),8(R8)                                                    
*                                                                               
         CLI   TCNTRL,C'D'         DEMO NAME ONLY?                              
         BNE   DEM42                                                            
         AHI   R8,10                                                            
         B     DEMX                                                             
*                                                                               
DEM42    MVC   10(5,R8),MODNAME                                                 
         AHI   R8,15                                                            
         B     DEMX                                                             
*                                                                               
***********************************************************************         
* EXTERNAL FORMAT - 4/4 ( WM 1849)                                    *         
***********************************************************************         
*                                                                               
DEM50    TM    CSFLAGS,CSFDEMO     COMSCORE DEMO?                               
         JO    DEM115                                                           
*                                                                               
         BAS   RE,GETTAB                                                        
         MVC   0(8,R8),NAM4BY4                                                  
         AHI   R8,8                                                             
         B     DEMX                                                             
*                                                                               
***********************************************************************         
* EXTERNAL FORMAT - 5/5 (W1849SHARE)                                  *         
***********************************************************************         
*                                                                               
DEM60    BAS   RE,GETTAB                                                        
         MVC   0(5,R8),NAM5                                                     
         TM    CSFLAGS,CSFDEMO     COMSCORE DEMO?                               
         JZ    *+10                                                             
         MVC   0(5,R8),CSDEMNAM    COMSCORE DEMO NAME                           
*                                                                               
         CLI   TCNTRL,C'D'         DEMO NAME ONLY?                              
         BNE   DEM62                                                            
         AHI   R8,5                                                             
         B     DEMX                                                             
DEM62    MVC   5(5,R8),MODNAME                                                  
         AHI   R8,10                                                            
         B     DEMX                                                             
***********************************************************************         
* EXTERNAL FORMAT - 6 (SW1849)                                        *         
***********************************************************************         
*                                                                               
DEM70    TM    CSFLAGS,CSFDEMO     COMSCORE DEMO?                               
         JO    DEM115                                                           
*                                                                               
         BAS   RE,GETTAB                                                        
         MVC   0(1,R8),MODMOD                                                   
         MVC   1(5,R8),NAM5                                                     
         CLI   DEMOMOD,DEMO_MODIFIER_I                                          
         BNE   *+10                                                             
         MVC   0(6,R8),NAM6                                                     
         CLI   DEMOMOD,DEMO_MODIFIER_T                                          
         BNE   *+10                                                             
         MVC   0(6,R8),NAM6                                                     
         AHI   R8,6                                                             
         B     DEMX                                                             
*                                                                               
***********************************************************************         
* EXTERNAL FORMAT - 7/5 (WM18-49SHARE)                                *         
***********************************************************************         
*                                                                               
DEM80    BAS   RE,GETTAB                                                        
         MVC   0(7,R8),NAM7                                                     
         TM    CSFLAGS,CSFDEMO     COMSCORE DEMO?                               
         JZ    *+10                                                             
         MVC   0(7,R8),CSDEMNAM                                                 
*                                                                               
         CLI   TCNTRL,C'D'         DEMO NAME ONLY                               
         BNE   *+12                                                             
         AHI   R8,7                                                             
         B     DEMX                                                             
         MVC   7(5,R8),MODNAME                                                  
         AHI   R8,12                                                            
         CLI   DEMOCAT,0           IF THERE IS A NO DEMO CATEGORY               
         BE    DEMX                JUST EXIT                                    
         MVC   0(7,R8),CATNAME     OTHERWISE SEND CAT. NAME                     
         AHI   R8,7                                                             
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* EXTERNAL FORMAT - SCREEN INPUT FORMAT                               *         
***********************************************************************         
*                                                                               
DEM90    TM    CSFLAGS,CSFDEMO     TEST COMSCORE DEMO                           
         JO    DEM115                                                           
*                                                                               
         BAS   RE,GETTAB                                                        
         XR    R1,R1               R1=L'OUTPUT                                  
         TM    DEMOSTAT,X'80'                                                   
         BZ    *+16                                                             
         MVI   0(R8),C'-'                                                       
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
         TM    DEMOMOD,X'40'                                                    
         BO    *+16                                                             
         MVI   0(R8),C'*'                                                       
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
*                                                                               
         CLI   DEMOCAT,0           IF THERE IS NO CATEGORY                      
         BE    *+18                LEAVE ALONE                                  
         MVC   0(3,R8),CATNUM      OTHERWISE SET CAT. NUMBER                    
         LA    R8,3(R8)                                                         
         LA    R1,3(R1)                                                         
*                                                                               
         CLI   DEMOCAT,100         4 POS CATEGORY                               
         BL    *+18                                                             
         MVC   0(1,R8),CATNUM+3    SET CAT. NUMBER                              
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
*                                                                               
         MVC   0(1,R8),MODMOD                                                   
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
         MVC   0(5,R8),MODNAME                                                  
         LA    R8,5(R8)                                                         
         LA    R1,5(R1)                                                         
         TM    DEMOSTAT,X'40'                                                   
         BZ    *+16                                                             
         MVI   0(R8),C'P'                                                       
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
         SR    R0,R0                                                            
         ICM   R0,1,TINPLEN                                                     
         AR    R1,R0               ADD TO PREVIOUS LEN                          
         STC   R1,TINPLEN          AND RETURN TOTAL                             
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* EXTERNAL FORMAT - 9                                                 *         
***********************************************************************         
*                                                                               
DEM100   TM    CSFLAGS,CSFDEMO     COMSCORE DEMO?                               
         JO    DEM115                                                           
*                                                                               
         BAS   RE,GETTAB                                                        
         XR    R1,R1               R1=L'OUTPUT                                  
*                                                                               
         TM    DEMOSTAT,X'80'                                                   
         BZ    *+16                                                             
         MVI   0(R8),C'-'                                                       
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
*                                                                               
         TM    DEMOSTAT,X'20'                                                   
         BZ    *+16                                                             
         MVI   0(R8),C'$'                                                       
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
*                                                                               
         TM    DEMOMOD,X'40'                                                    
         BO    *+16                                                             
         MVI   0(R8),C'*'                                                       
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
*                                                                               
         CLI   DEMOCAT,0           IS THERE A CATEGORY NUMBER?                  
         BNE   DEM103              YES: CAT# & PLD CHR ALREADY INSERTED         
         CLI   PLDCHAR,C' '        NO: IS THERE A PERSONAL LANGUAGE?            
         BNH   DEM104              NO                                           
         MVC   0(1,R8),PLDCHAR     YES: PREFIX THE DEMO W/ PLD CHAR             
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
         B     DEM104                                                           
*                                                                               
DEM103   DS    0H                                                               
         MVC   0(3,R8),CATNUM      OTHERWISE SET CAT. NUMBER                    
         LA    R8,3(R8)                                                         
         LA    R1,3(R1)                                                         
*                                                                               
         CLI   DEMOCAT,100         4 POS CATEGORY                               
         BL    DEM104                                                           
         MVC   0(1,R8),CATNUM+3    SET CAT. NUMBER                              
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
*                                                                               
DEM104   DS    0H                                                               
         MVC   0(1,R8),MODMOD                                                   
         MVC   1(5,R8),NAM5                                                     
*                                                                               
         CLI   DEMOMOD,DEMO_MODIFIER_I                                          
         BNE   *+10                                                             
         MVC   0(6,R8),NAM6                                                     
         CLI   DEMOMOD,DEMO_MODIFIER_T                                          
         BNE   *+10                                                             
         MVC   0(6,R8),NAM6                                                     
         LA    R8,5(R8)                                                         
         LA    R1,5(R1)                                                         
*                                                                               
         LA    RE,6                FIND TRUE LENGTH OF NAME                     
DEM105   CLI   0(R8),C' '                                                       
         BNE   DEM107                                                           
         BCTR  R8,0                                                             
         BCTR  R1,0                                                             
         BCT   RE,DEM105                                                        
*                                                                               
DEM107   LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
*                                                                               
         TM    DEMOSTAT,X'40'                                                   
         BZ    *+16                                                             
         MVI   0(R8),C'P'                                                       
         LA    R8,1(R8)                                                         
         LA    R1,1(R1)                                                         
         SR    R0,R0                                                            
         ICM   R0,1,TINPLEN                                                     
         AR    R1,R0               ADD TO PREVIOUS LEN                          
         STC   R1,TINPLEN          AND RETURN TOTAL                             
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* EXTERNAL FORMAT - SPOTPAK EDITED                                    *         
***********************************************************************         
*                                                                               
DEM110   TM    CSFLAGS,CSFDEMO     COMSCORE DEMO?                               
         JO    DEM115                                                           
*                                                                               
         CLI   1(R7),63            TEST WEIGHTED DEMO                           
         BE    DEM110A                                                          
         CLI   1(R7),X'21'         TEST USER DEMO                               
         BNE   DEM111                                                           
*                                                                               
* USER DEMO - SET DEFAULT NAME *                                                
*                                                                               
         MVC   0(4,R8),=C'USER'                                                 
         MVC   4(1,R8),2(R7)       MOVE NUMBER                                  
         OI    4(R8),X'F0'         MAKE EBCDIC                                  
         B     DEM110B                                                          
*                                                                               
* WEIGHTED DEMO - SET DEFAULT NAME *                                            
*                                                                               
DEM110A  MVC   0(7,R8),=C'WEIGHTD'                                              
*                                                                               
DEM110B  CLI   TCNTRL,C'S'         TEST SPOTPAK REQUEST                         
         BNE   DEM119                                                           
*                                                                               
         ZIC   RE,2(R7)            GET DEMO NUMBER                              
         CLI   1(R7),63                                                         
         BNE   *+8                                                              
         LA    RE,5                WEIGHTED DEMO IS FIFTH USER NAME             
         BCTR  RE,0                                                             
         MHI   RE,7                                                             
         XR    RF,RF                                                            
         ICM   RF,7,AUSRNMS+1                                                   
         BZ    DEM119                                                           
         AR    RE,RF               POINT TO USER NAME                           
         MVC   0(7,R8),0(RE)                                                    
         B     DEM119                                                           
*                                                                               
DEM111   LR    R5,R8                                                            
         XC    0(10,R5),0(R5)                                                   
*                                                                               
         CLI   DBDEMTYP,C'4'       REFORMAT 4 CHAR INPUT                        
         BE    *+12                                                             
         CLI   2(R7),0             RENTRAK IS X'00##00'                         
         BE    DEM115                                                           
*                                                                               
         CLI   DBSELMED,C'R'       ONLY RADIO SHOWS THE CATEGORY NAME           
         BNE   DEM111NR            NOT RADIO, SHOW NUMBER INSTEAD               
*                                                                               
         CLI   TOUTPUT,12          NAME OF CATEGORY                             
         BE    DEM111N                                                          
         CLI   TOUTPUT,13          NAME OF CATEGORY, COMPRESSED                 
         BE    DEM111N                                                          
*                                                                               
DEM111NR DS    0H                                                               
         CLI   DEMOCAT,0           IS THERE A CATEGORY NUMBER?                  
         BNE   DEM111NU            YES: CAT# & PLD CHR ALREADY INSERTED         
         CLI   PLDCHAR,C' '        NO: IS THERE A PERSONAL LANGUAGE?            
         BNH   DEM111X             NO                                           
         MVC   0(1,R5),PLDCHAR     YES: PREFIX THE DEMO W/ PLD CHAR             
         LA    R5,1(R5)                                                         
         B     DEM111X                                                          
*                                                                               
DEM111NU DS    0H                                                               
         MVC   0(3,R5),CATNUM      OTHERWISE SET CAT. NUMBER                    
         LA    R5,3(R5)                                                         
         CLI   DEMOCAT,100         IF THERE IS NO CATEGORY                      
         BL    *+14                LEAVE ALONE                                  
         MVC   0(1,R5),CATNUM+3    OTHERWISE SET CAT. NUMBER                    
         LA    R5,1(R5)                                                         
         B     DEM111X                                                          
*                                                                               
DEM111N  CLI   CATNAMEL,0                                                       
         BZ    DEM111X                                                          
         ZIC   R1,CATNAMEL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),CATNAME                                                  
         LA    R5,1(R1,R5)                                                      
         MVI   0(R5),C'.'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
DEM111X  BAS   RE,GETTAB                                                        
         CLI   TCNTRL,C'S'         TEST FOR SPOTPAK CALL                        
         BE    DEM112                                                           
         CLI   DEMOMOD,DEMO_MODIFIER_T  TEST FOR REP IMPS 'T'                   
         BNE   DEM112                                                           
         TM    DEMOSTAT,X'20'      SUPPRESS THE MODIFIER ON OUTPUT              
         BNZ   *+14                                                             
         MVC   0(7,R5),NAM7                                                     
         B     DEM119                                                           
         MVI   0(R5),C'$'                                                       
         MVC   1(6,R5),NAM6                                                     
         B     DEM119                                                           
*                                                                               
DEM112   TM    DEMOSTAT,X'20'                                                   
         BNZ   DEM114                                                           
         MVC   0(1,R5),MODMOD                                                   
         MVC   1(6,R5),NAM6                                                     
         CLI   DEMOMOD,DEMO_MODIFIER_I TEST FOR SPOT IMP                        
         BNE   DEM119                                                           
         MVC   0(6,R5),NAM6        SUPPRESS THE MODIFIER ON OUTPUT              
         MVI   6(R5),C' '                                                       
         B     DEM119                                                           
*                                                                               
DEM114   MVI   0(R5),C'$'                                                       
         MVC   1(1,R5),MODMOD                                                   
         MVC   2(5,R5),NAM5                                                     
         CLI   DEMOMOD,DEMO_MODIFIER_I TEST FOR SPOT IMP                        
         BNE   DEM119                                                           
         MVC   1(6,R5),NAM6        SUPPRESS THE MODIFIER                        
         B     DEM119                                                           
*                                                                               
***********************************************************************         
* RETURN COMSCORE DEMO NAME                                                     
***********************************************************************         
DEM115   CLI   TOUTPUT,2                                                        
         BNE   DEM115B                                                          
         MVC   0(7,R8),CSDEMNAM    COMSCORE DEMO NAME                           
         AHI   R8,7                                                             
         B     DEMX                                                             
*                                                                               
DEM115B  CLI   TOUTPUT,3                                                        
         BNE   DEM115C                                                          
         MVC   0(L'CSDEMNAM,R8),CSDEMNAM                                        
         AHI   R8,15                                                            
         B     DEMX                                                             
*                                                                               
DEM115C  CLI   TOUTPUT,4                                                        
         BNE   DEM115D                                                          
         MVC   0(8,R8),CSDEMNAM    COMSCORE DEMO NAME                           
         AHI   R8,8                                                             
         B     DEMX                                                             
*                                                                               
DEM115D  CLI   TOUTPUT,5                                                        
         BNE   DEM115E                                                          
         MVC   0(L'CSDEMNAM,R8),CSDEMNAM                                        
         AHI   R8,10                                                            
         B     DEMX                                                             
*                                                                               
DEM115E  CLI   TOUTPUT,6                                                        
         BNE   DEM115F                                                          
         MVC   0(6,R8),CSDEMNAM    COMSCORE DEMO NAME                           
         AHI   R8,6                                                             
         B     DEMX                                                             
*                                                                               
DEM115F  CLI   TOUTPUT,7                                                        
         BNE   DEM115G                                                          
         MVC   0(7,R8),CSDEMNAM    COMSCORE DEMO NAME                           
         AHI   R8,7                                                             
         B     DEMX                                                             
*                                                                               
DEM115G  MVC   0(L'CSDEMNAM,R8),CSDEMNAM                                        
         AHI   R8,10               POINT TO END OF STRING                       
         CLI   TOUTPUT,11                                                       
         BNH   DEMX                                                             
         AHI   R8,1                11 POS. FOR 12 AND 13                        
         B     DEMX                                                             
*                                                                               
DEM119   CLI   TOUTPUT,11                                                       
         BH    *+12                                                             
         AHI   R8,10               POINT TO END OF STRING                       
         B     DEMX                                                             
*                                                                               
         AHI   R8,11               11 POS. FOR 12 AND 13                        
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* EXTERNAL FORMAT - 5/5/5 (WOMEN18-49(SHR))                           *         
*                   MOD + 5 CHAR DESC                                 *         
***********************************************************************         
*                                                                               
DEM120   TM    CSFLAGS,CSFDEMO     COMSCORE DEMO?                               
         JO    DEM115                                                           
*                                                                               
         BAS   RE,GETTAB                                                        
         MVC   0(1,R8),MODMOD                                                   
         MVC   1(5,R8),NAM5                                                     
         CLI   DEMOMOD,DEMO_MODIFIER_I                                          
         BNE   *+14                                                             
         MVC   0(5,R8),NAM5                                                     
         MVI   5(R8),C' '                                                       
         CLI   DEMOMOD,DEMO_MODIFIER_T                                          
         BNE   *+14                                                             
         MVC   0(5,R8),NAM5                                                     
         MVI   5(R8),C' '                                                       
         LA    R8,6(R8)                                                         
         B     DEMX                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* RETURN A(TR TABLES) FOR PERSONAL LANGUAGE DEMOS                     *         
***********************************************************************         
*                                                                               
DEM140   DS    0H                                                               
         XC    0(L'APLDTABS,R8),0(R8)                                           
*                                                                               
         MVC   0(4,R8),APLMODTB    A(MODIFIER TABLE)                            
         MVC   4(4,R8),APLPLDTB    A(PERSONAL LANG. ATTRIBUTE TABLE)            
         MVC   8(4,R8),APLENCTB    A(MODIFIER BYTE ENCODING TABLE)              
         MVC   12(4,R8),APLPRNTB   A(PERS. LANG. PRINTABLE DESCRIPS.)           
*                                                                               
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* RETURN LEFT-JUSTIFIED DECODED MODIFIER AND PERSONAL LANG. INDICATOR *         
***********************************************************************         
*                                                                               
DEM150   DS    0H                                                               
         LLC   RE,0(R7)            EXTRACT EBCDIC MODIFIER                      
         A     RE,APLMODTB                                                      
         LLC   RF,0(R7)            EXTRACT EBCDIC PLD CHARACTER                 
         A     RF,APLPLDTB                                                      
*                                                                               
         MVC   0(1,R8),0(RE)       MODIFIER (ASSUME NO PERSONAL LANG.)          
         LHI   R1,1                ASSUME RETURNED LENGTH OF 1                  
         CLI   0(RF),C' '          ANY PERSONAL LANGUAGE DEMO?                  
         BNH   DEM150E             NO                                           
*                                                                               
         MVC   0(1,R8),0(RF)       YES: FORMAT PERS. LANG AND MODIFIER          
         MVC   1(1,R8),0(RE)                                                    
         AHI   R1,1                INCREMENT RETURNED LENGTH                    
*                                                                               
DEM150E  DS    0H                                                               
         STC   R1,TINPLEN          AND RETURN TOTAL                             
*                                                                               
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* DECODE AN ENCODED DEMO LIST (INPUT) INTO TWO PARALLEL DECODED       *         
* LISTS (OUTPUT).                                                     *         
*                                                                     *         
* 1ST OUTPUT LIST CONTAINS UNENCODED (EBCDIC) MODIFIERS.              *         
* 2ND OUTPUT LIST IS A PARALLEL ARRAY OF PERSONAL LANGUAGE ATTRIBUTE  *         
*     CHARACTERS (IF THE CHARACTER IS A SPACE OR NULL, THEN THE       *         
*     INPUT MODIFIER WAS NOT ENCODED).                                *         
*                                                                     *         
* NOTE: THE IDEA IS THAT IF AN INPUT MODIFIER IS NOT ENCODED TO START *         
*       WITH, THEN THE OUTPUT MODIFIER WILL BE IDENTICAL TO THE INPUT *         
*       MODIFIER. IT IS THEREFORE SAFE TO DECODE THE MODIFIER WITHOUT *         
*       CHECKING TO SEE IF IT IS ENCODED.                             *         
*                                                                     *         
***********************************************************************         
*                                                                               
DEM160   DS    0H                                                               
         OC    APLDPTR,APLDPTR     IS PLD ARRAY POINTER INITIALIZED?            
         BNZ   *+10                                                             
         MVC   APLDPTR,APLDLST     NO: DO IT NOW                                
*                                                                               
*                                  THIS COULD BE AN OVERLAPPING MOVE            
         MVC   0(3,R8),0(R7)       COPY INPUT ENTRY TO OUTPUT LIST              
         CLI   DBDEMTYP,C'4'       4 BYTE DEMO INPUT?                           
         BNE   *+10                                                             
         MVC   3(1,R8),3(R7)       YES: COPY THE LAST BYTE                      
*                                                                               
         LLC   RE,1(R7)            EXTRACT EBCDIC MODIFIER                      
         A     RE,APLMODTB                                                      
         LLC   RF,1(R7)            EXTRACT EBCDIC PLD CHARACTER                 
         A     RF,APLPLDTB                                                      
*                                                                               
         MVC   1(1,R8),0(RE)       UNENCODED MODIFIER                           
*                                                                               
DEM162   LA    R8,3(R8)            BUMP OUTPUT POINTER                          
         CLI   DBDEMTYP,C'4'       4 BYTE DEMO INPUT?                           
         BNE   *+8                 NO                                           
         LA    R8,1(R8)            BUMP OUTPUT POINTER BY ONE MORE              
*                                                                               
         L     RE,APLDPTR          CURRENT PLD ARRAY POINTER                    
         MVC   0(1,RE),0(RF)       UNENCODED PLD ATTRIBUTE CHARACTER            
*                                                                               
         LA    RE,1(RE)            BUMP OUTPUT POINTER                          
         ST    RE,APLDPTR          SAVE CURRENT POINTER                         
*                                                                               
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* ENCODE TWO UNENCODED PARALLEL DEMO LISTS (INPUT) INTO A SINGLE      *         
* ENCODED DEMO LIST (OUTPUT).                                         *         
*                                                                     *         
* 1ST INPUT LIST CONTAINS UNENCODED (EBCDIC) MODIFIERS.               *         
* 2ND INPUT LIST IS A PARALLEL ARRAY OF PLD ATTRIBUTE CHARS (A SPACE  *         
*     REPRESENTS THE ABSENCE OF A PERSONAL LANGUAGE ATTRIBUTE).       *         
*                                                                     *         
* NOTE: IF THE PERSONAL LANGUAGE ATTRIBUTE CHARACTER IS A SPACE,      *         
*       THEN THE ENCODED MODIFIER WILL BE IDENTICAL TO THE UNENCODED  *         
*       MODIFIER. IT IS THEREFORE SAFE TO ENCODE THE TWO LISTS        *         
*       WITHOUT REGARD FOR WHETHER THE DEMO HAS A PERSONAL LANGUAGE   *         
*       OR DOES NOT.                                                  *         
*                                                                     *         
***********************************************************************         
*                                                                               
DEM170   DS    0H                                                               
         OC    APLDPTR,APLDPTR     IS PLD ARRAY POINTER INITIALIZED?            
         BNZ   *+10                                                             
         MVC   APLDPTR,APLDLST     NO: DO IT NOW                                
*                                                                               
*                                  THIS COULD BE AN OVERLAPPING MOVE            
         MVC   0(3,R8),0(R7)       COPY INPUT ENTRY TO OUTPUT LIST              
         CLI   DBDEMTYP,C'4'       4 BYTE DEMO INPUT?                           
         BNE   *+10                                                             
         MVC   3(1,R8),3(R7)       YES: COPY THE LAST BYTE                      
*                                                                               
* TRANSLATE THE MODIFIER/PLD IF NECESSARY                                       
*                                                                               
         L     RE,APLDPTR          A(CURRENT PLD CHARACTER)                     
         CLI   0(RE),C' '          ALL PLD CHARS ARE > C' '                     
         JNH   DEM170M             NO NEED TO SEARCH THE TABLE                  
*                                                                               
         L     RF,APLENCTB         MODIFIER ENCODING TABLE                      
         LH    R0,0(RF)            L'ENTRY                                      
         AHI   RF,2                START OF TABLE ENTRIES                       
         USING PLD_MOD_ENCODE_TABLED,RF                                         
*                                                                               
DEM170E  DS    0H                                                               
         CLC   0(3,RF),=XL3'00'    EOT?                                         
         JE    DEM170M             MODIFIER/PLD NOT FOUND: NOT ENCODED          
*                                                                               
         CLC   PLD_MODIFIER_CHAR,1(R7)   MATCH ON MODIFIER CHARACTER?           
         BNE   *+14                      NO: TRY NEXT TABLE ENTRY               
         CLC   PLD_ATTRIBUTE_CHAR,0(RE)  MATCH ON PLD CHARACTER?                
         BE    DEM170H                   YES: ENCODE THE MODIFIER               
*                                                                               
         AR    RF,R0               BUMP TO NEXT ENTRY                           
         B     DEM170E                                                          
*                                                                               
DEM170H  MVC   1(1,R8),PLD_ENCODED_MODIFIER RETURN THE ENCODED MODIFIER         
         DROP  RF                                                               
*                                                                               
DEM170M  DS    0H                                                               
         LA    R8,3(R8)            BUMP OUTPUT POINTER                          
         CLI   DBDEMTYP,C'4'       4 BYTE DEMO INPUT?                           
         BNE   *+8                 NO                                           
         LA    R8,1(R8)            BUMP OUTPUT POINTER BY ONE MORE              
*                                                                               
         L     RE,APLDPTR          CURRENT PLD ARRAY POINTER                    
         LA    RE,1(RE)            BUMP OUTPUT POINTER                          
         ST    RE,APLDPTR          SAVE CURRENT POINTER                         
*                                                                               
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* RETURN THE ADDRESS OF THE PERSONAL LANGUAGE DEMO DESCRIPTION TABLE  *         
* FOR THE GIVEN MODIFIER. IF THE MODIFIER DOES NOT HAVE A PERSONAL    *         
* LANGUAGE ENCODED, RETURN 0.                                                   
*                                                                     *         
***********************************************************************         
*                                                                               
DEM180   DS    0H                                                               
         XC    0(4,R8),0(R8)       ASSUME NOT PERSONAL LANGUAGE                 
*                                                                               
         LLC   RF,1(R7)            EXTRACT EBCDIC PLD CHARACTER                 
         A     RF,APLPLDTB                                                      
         CLI   0(RF),C' '          PERSONAL LANGUAGE IS ENCODED?                
         BNH   DEMX                NO                                           
*                                                                               
* SEARCH THE PRINTABLE DESCRIPTION TABLE                                        
*                                                                               
         LARL  RE,DEMO_PLD_PRINT                                                
         USING PLDD_DESC_TABLED,RE                                              
*                                                                               
DEM180E  DS    0H                                                               
         CLC   =X'FFFF',0(RE)      EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                YES: UNKNOWN PLD CHARACTER                   
*                                                                               
         CLC   PLDD_INPUT_CHAR,0(RF)                                            
         BE    *+12                WE FOUND THE CHARACTER                       
         AHI   RE,PLDD_DESC_LENQ   BUMP TO NEXT ENTRY                           
         B     DEM180E                                                          
         DROP  RE                                                               
*                                                                               
         ST    RE,0(R8)            RETURN A(TABLE ENTRY)                        
         LA    R8,4(R8)            PREPARE FOR NEXT IN DEMO LIST                
*                                                                               
         B     DEMX                                                             
         EJECT                                                                  
***********************************************************************         
* MISC. ROUTINES                                                      *         
***********************************************************************         
*                                                                               
FINDCAT  DS    0H                                                               
         MVI   DEMOCAT,0           INITIALIZE THE VARIABLES                     
         MVI   CATNAMEL,0                                                       
         MVC   CATNUM,NODEMO+1                                                  
         MVC   CATNAME,NODEMO+1                                                 
         CLI   DEMOSTAT,0          IF NO CATEGORY                               
         BE    FINDCATX                                                         
*                                                                               
         CLI   DBSELMED,C'R'       HAVE CATS FOR RADIO                          
         BE    FINDCAT0                                                         
         CLC   DBFILE,=C'NAD'      IF FILE NOT NAD                              
         BE    FINDCAT0                                                         
         CLC   DBFILE,=C'NTI'      AND FILE NOT NTI                             
         BNE   FINDCATX            GET OUT                                      
*                                                                               
FINDCAT0 DS    0H                                                               
         MVC   DEMOCAT,DEMOSTAT    SAVE CATEGORY NUMBER                         
         MVI   DEMOSTAT,0          KILL THE STATUS BIT OPTIONS                  
         LARL  RF,PFXTAB           SETUP FOR DECODING                           
         LA    R1,PREFIXES                                                      
*                                                                               
         CLI   DBSELMED,C'R'       RADIO                                        
         BNE   FINDCAT1                                                         
         LARL  RF,PFXRAD           SETUP FOR DECODING                           
         LA    R1,PRERADES                                                      
*                                                                               
FINDCAT1 CLC   DEMOCAT,0(RF)       MATCH INPUT NUMBER TO CAT. TABLE             
         BE    FINDCAT2                                                         
         LA    RF,L'PFXTAB(RF)                                                  
         BCT   R1,FINDCAT1                                                      
         B     FINDCATX            EXIT IF NOT FOUND                            
*                                                                               
FINDCAT2 MVC   CATNAME,1(RF)       SET CATEGORY NAME                            
         LA    R1,7                                                             
         LA    R3,CATNAME+6                                                     
CTNMLP   CLI   0(R3),C' '                                                       
         BH    CTNMLPX                                                          
         BCTR  R1,0                                                             
         BCTR  R3,0                                                             
         B     CTNMLP                                                           
CTNMLPX  STC   R1,CATNAMEL                                                      
         CLI   TOUTPUT,13                                                       
         BNE   FINDCT2A                                                         
         CLC   PREVCATN,DEMOCAT                                                 
         BNE   *+8                                                              
         MVI   CATNAMEL,0                                                       
FINDCT2A MVC   PREVCATN,DEMOCAT                                                 
*                                                                               
         MVI   BYTE,C'.'           ASSUME STANDARD NAD DELIMITER                
         CLI   PLDCHAR,C' '        IS THERE A PERSONAL LANGUAGE?                
         BNH   *+10                NO                                           
         MVC   BYTE,PLDCHAR        YES: USE PLD INDICATOR AS DELIMITER          
*                                                                               
         ZIC   R1,0(RF)            AND 2 BYTE NUMBER FOLLOWED BY                
         CVD   R1,DUB              A PERIOD.                                    
         CHI   R1,99                                                            
         BH    FINDCAT3                                                         
         UNPK  CATNUM(2),DUB+6(2)                                               
         OI    CATNUM+1,X'F0'                                                   
         MVC   CATNUM+2(1),BYTE    NAD DELIMITER                                
         CLI   CATNUM,C'0'                                                      
         BNE   *+8                                                              
         MVI   CATNUM,C'0'                                                      
         B     FINDCATX                                                         
*                                                                               
FINDCAT3 UNPK  CATNUM(3),DUB+6(2)                                               
         OI    CATNUM+2,X'F0'                                                   
         MVC   CATNUM+3(1),BYTE    NAD DELIMITER                                
*                                                                               
FINDCATX DS    0H                                                               
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET ADDRESSES OF SYSTEM TABLE ENTRIES                               *         
***********************************************************************         
*                                                                               
GETTAB   ST    RE,SAVERE                                                        
         MVC   NOMOD,=C'******'                                                 
         MVC   NOMOD(1),DEMOMOD                                                 
*                                                                               
         TM    CSFLAGS,CSFDEMO     TEST COMSCORE DEMO                           
         JZ    *+10                                                             
         MVC   NOMOD(1),CSDEMMOD                                                
*                                                                               
         OI    NOMOD,X'40'                                                      
         CLI   NOMOD,DEMO_MODIFIER_X                                            
         BNE   *+8                                                              
         MVI   NOMOD,DEMO_MODIFIER_T                                            
*                                                                               
         LA    R3,NOMOD            R3=DEFAULT MODIFIER NAME                     
         LA    R4,NODEMO           R4=DEFAULT DEMO NAME                         
*                                                                               
         XC    DUB,DUB             SPECIAL CALL FOR SYSFACS                     
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         ICM   RF,15,CSWITCH                                                    
         BNZ   *+16                                                             
         ICM   RF,15,CMASTC                                                     
         ICM   RF,15,MCSSB-MASTD(RF)  RF=ASSB                                   
         B     GETTAB0                                                          
*                                                                               
         GOTO1 (RF),DUB            V(SWITCH)                                    
         L     RF,0(R1)            RF=V(SYSFACS)                                
         L     RF,VSSB-SYSFACD(RF) RF=V(SSB)                                    
*                                                                               
GETTAB0  DS    0H                                                               
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
         XC    DUB,DUB                                                          
*                                  GET A(DEMO TABLES)                           
         L     R6,=A(DEMTABCL)     A(LIST OF TABLES WITHIN PROGRAM)             
         A     R6,RELO                                                          
         USING DEMTABCL,R6                                                      
         MVC   TABLIST,TABLISTL    SET W/S FROM LIST WITHIN CSEC                
         OC    AMODTAB+1(3),AMODTAB+1  DID WE GET A(DCODE) ALREADY?             
         BNZ   GETTAB1             YES                                          
*                                                                               
         GOTO1 CDEMADDR,DMCB,(X'FF',TABLIST),COMFACSD                           
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                                                                
*                                                                               
         ICM   RF,15,CPROTOFF                                                   
         JZ    *+6                                                              
         BASR  RE,RF               *** TURN STORAGE PROTECTION OFF ***          
*                                                                               
         MVC   TABLISTL(TABLISTQ),TABLIST     SAVE A(TABLES) IN CSECT           
*                                                                               
         ICM   RF,15,CPROTON                                                    
         JZ    *+6                                                              
         BASR  RE,RF               *** STORAGE PROTECTION BACK ON ***           
*                                                                               
         DROP  R6                                                               
*                                                                               
GETTAB1  MVC   DUB(3),DBFILE                                                    
         MVC   DUB+3(1),DBSELMED                                                
         ICM   RE,15,AFMTAB                                                     
         BZ    GETTABX                                                          
         LAM   ARE,ARE,ALET                                                     
         SAC   512                                                              
*                                                                               
GETTAB2  CLI   0(RE),FF            TEST E-O-T                                   
         BNE   GETTAB4             NO                                           
         LAM   ARE,ARE,=F'0'                                                    
         B     GETTABX                                                          
*                                                                               
GETTAB4  CLC   DUB(4),0(RE)                                                     
         BE    *+12                                                             
         AHI   RE,6                                                             
         B     GETTAB2                                                          
*                                                                               
         MVC   INTFIL,4(RE)                                                     
         LAM   ARE,ARE,=F'0'                                                    
*                                  BUILD KEY FOR MODIFIER TABLE LOOK-UP         
         XC    DUB,DUB                                                          
         MVC   DUB(2),INTFIL                                                    
         OC    DUB+2(2),DBSELAGY                                                
         BNZ   *+10                                                             
         MVC   DUB+2(2),EFFS                                                    
         MVI   DUB+4,FF                                                         
*                                                                               
         L     R1,AMODTAB                                                       
         USING MODHDRD,R1                                                       
         LAM   AR1,AR1,ALET                                                     
*                                  LOOK-UP FOR FILE/SUBFILE/AGENCY              
GETTAB6  CLC   MODFILE(2),=XL2'00' TEST E-O-T                                   
         BE    GETTAB12                                                         
         CLC   MODFILE(2),DUB                                                   
         BNE   GETTAB8                                                          
         CLC   MODAGY,DUB+2                                                     
         BE    GETTAB10                                                         
         BL    GETTAB8                                                          
         CLC   DUB+2(2),EFFS                                                    
         BE    GETTAB12                                                         
         MVC   DUB+2(2),EFFS                                                    
         B     GETTAB6                                                          
*                                                                               
GETTAB8  ICM   R1,7,MODAET                                                      
         AHI   R1,1                                                             
         B     GETTAB6                                                          
*                                  LOOK-UP TABLE FOR MODIFIER                   
GETTAB10 XR    RE,RE               SET-UP BXLE REGISTERS                        
         ICM   RE,3,MODLDE                                                      
         XR    RF,RF                                                            
         ICM   RF,7,MODAET                                                      
         BCTR  RF,0                                                             
         AHI   R1,MODHDRLN                                                      
         USING MODDTAD,R1                                                       
*                                                                               
         CLC   MODMOD,NOMOD                                                     
         BE    GETTAB11                                                         
         BXLE  R1,RE,*-10                                                       
         B     GETTAB12                                                         
*                                                                               
GETTAB11 LR    R3,R1               SET R3 TO ENTRY                              
         LAM   AR3,AR3,ALET                                                     
*                                  BUILD KEY FOR DEMO LOOK-UP                   
GETTAB12 LAM   AR1,AR1,=F'0'                                                    
         MVC   DUB(2),INTFIL                                                    
         CLI   TOUTPUT,8                                                        
         BNE   *+10                                                             
         MVC   DUB(2),EFFS                                                      
         MVC   DUB+2(2),DBSELAGY                                                
         CLI   TOUTPUT,8                                                        
         BNE   *+10                                                             
         MVC   DUB+2(2),EFFS                                                    
         MVC   DUB+4(1),DEMOGRP                                                 
         CLI   DUB+4,0                                                          
         BNE   *+8                                                              
         MVI   DUB+4,FF                                                         
*                                                                               
GETTAB13 L     R1,ANAMTAB                                                       
         LAM   AR1,AR1,ALET                                                     
         USING NAMHDRD,R1                                                       
*                                  FIND ENTRY FOR FILE/SUBFILE/AGENCY           
GETTAB14 CLC   NAMFILE(2),=XL2'00' TEST E-O-T                                   
         BE    GETTABX             TRY FOR DEFAULT                              
         CLC   NAMFILE(2),DUB                                                   
         BE    GETTAB16                                                         
         BL    GETTAB20                                                         
         CLC   DUB(2),EFFS                                                      
         BE    GETTABX             NO DEFAULT-ERROR                             
         MVC   DUB(2),EFFS         SET TO TRY FOR DEFAULT                       
         B     GETTAB14                                                         
*                                                                               
GETTAB16 CLC   NAMAGY,DUB+2                                                     
         BE    GETTAB18                                                         
         BL    GETTAB20                                                         
         CLC   DUB+2(2),EFFS                                                    
         BE    GETTABX                                                          
         MVC   DUB+2(2),EFFS                                                    
         B     GETTAB14                                                         
*                                                                               
GETTAB18 CLC   NAMCODE,DUB+4                                                    
         BE    GETTAB22                                                         
         BL    GETTAB20                                                         
         CLC   DUB+4(1),EFFS                                                    
         BE    GETTABX                                                          
         CLI   DUB+4,0             GROUP ZERO IS OLD NUMBERS                    
         BNE   GETTABX             NEW NUMBERS CAN'T DEFUALT                    
         MVC   DUB+4(1),EFFS                                                    
         B     GETTAB14                                                         
*                                                                               
GETTAB20 ICM   R1,7,NAMAET                                                      
         LA    R1,1(R1)                                                         
         B     GETTAB14                                                         
*                                  SET-UP BXLE REGISTERS                        
GETTAB22 XR    RE,RE                                                            
         ICM   RE,3,NAMLDE                                                      
         XR    RF,RF                                                            
         ICM   RF,7,NAMAET                                                      
         BCTR  RF,0                                                             
         AHI   R1,NAMHDRLN                                                      
         USING NAMDTAD,R1                                                       
*                                  SEARCH TABLE FOR DEMO NUMBER                 
         CLC   NAMDEMO,DEMONUM                                                  
         BE    GETTAB24                                                         
         BRXLE R1,RE,*-10                                                       
*                                                                               
         CLC   DUB+2(2),EFFS                                                    
         BE    *+14                                                             
         MVC   DUB+2(2),EFFS                                                    
         B     GETTAB13                                                         
         CLC   DUB(2),EFFS                                                      
         BE    GETTABX                                                          
         MVC   DUB(2),EFFS                                                      
         B     GETTAB13                                                         
*                                  SET A(NAME TABLE ENTRY)                      
GETTAB24 LR    R4,R1                                                            
         LAM   AR4,AR4,ALET                                                     
*                                  EXIT FROM ROUTINE                            
GETTABX  LAM   AR1,AR1,=F'0'                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
* SET PERSONAL LANGUAGE DEMO TABLE ADDRESSES.                                   
*                                                                               
SETPLDTB ST    RE,SAVERE                                                        
*                                                                               
         LARL  RE,DEMO_MODIFIERS                                                
         ST    RE,APLMODTB                                                      
         LARL  RE,DEMO_PLD_CHARS                                                
         ST    RE,APLPLDTB                                                      
         LARL  RE,DEMO_MODIFIER_ENCODE_TABLE                                    
         ST    RE,APLENCTB                                                      
         LARL  RE,DEMO_PLD_PRINT                                                
         ST    RE,APLPRNTB                                                      
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
*                                                                               
S        EQU   DEMO_MODIFIER_S                                                  
R        EQU   DEMO_MODIFIER_R                                                  
X        EQU   DEMO_MODIFIER_X                                                  
*                                                                               
FF       EQU   X'FF'                                                            
MOUTPUT  EQU   DEMOCON_18          MAXIMUM ALLOWABLE OUTPUT TYPE                
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 3                                                                
DEMTABCL CSECT                                                                  
*                                                                               
TABLISTL DC    X'D6',3X'00'        DCODE TABLE                                  
         DC    X'D5',3X'00'        DNAME TABLE                                  
         DC    X'E2',3X'00'        FILE/MEDIA TABLE                             
         DC    X'E8',3X'00'        RENTRAK TABLE                                
         DC    X'EB',3X'00'        CSLON (COMSCORE NUMBER)                      
         DC    X'FF'                                                            
TABLISTQ EQU   *-TABLISTL                                                       
         SPACE 3                                                                
DEMOCON  RSECT                                                                  
*                                                                               
EFFS     DC    X'FFFF'                                                          
NODEMO   DC    X'00',36C'*'                                                     
*                                                                               
CTRTAB   DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ',X'00'                              
         EJECT                                                                  
***********************************************************************         
* SPOT DEMO CONVERSION TABLE (INDEX BY SPOT DEMO-1*2).                *         
***********************************************************************         
*                                                                               
* BYTE 0 = MODIFIER CODE, BYTE 1 = DEMO NUMBER                                  
*                                                                               
         DS    0H                                                               
SPDEMTAB DS    0XL2                                                             
         DC    AL1(S,001,R,002,R,001,R,078,R,025,R,128,R,040,R,041)             
         DC    AL1(R,042,R,045,R,047,R,050,R,057,R,054,R,028,R,048)             
         DC    AL1(R,049,R,091,R,092,R,095,R,097,R,101,R,121,R,122)             
         DC    AL1(R,123,R,098,R,125,R,028,R,099,R,084,R,141,R,142)             
         DC    AL1(R,145,R,154,R,172,R,129,R,154,R,104,R,100,R,147)             
         DC    AL1(R,148,R,150,X,075,R,182,X,079,X,084,X,090,X,091)             
         DC    AL1(X,092,X,095,X,096,X,097,X,100,X,101,X,104,X,107)             
         DC    AL1(X,098,X,099,X,000,0,000,0,000,0,000,X,063,X,001)             
         DC    AL1(X,078,0,000,X,025,X,034,R,029,X,029,R,043,X,043)             
         DC    AL1(R,052,X,052,0,000,X,028,X,040,X,041,X,042,X,045)             
         DC    AL1(X,046,X,047,X,050,X,051,X,054,X,057,X,048,X,049)             
         DC    AL1(X,053,S,003,R,003,X,103,R,103,X,153,R,153,R,065)             
         DC    AL1(X,065,X,121,X,122,X,123,R,090,X,125,R,053,X,172)             
         DC    AL1(X,127,X,140,X,182,X,179,X,185,X,128,X,129,X,130)             
         DC    AL1(R,140,R,093,X,093,X,141,X,142,X,145,X,146,X,147)             
         DC    AL1(X,148,R,127,X,151,X,154,X,157,R,149,X,149,FF,FF)             
         EJECT                                                                  
         DS    0H                                                               
       ++INCLUDE DERADCATS                                                      
         SPACE 2                                                                
         DS    0H                                                               
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
*================================================================               
* TEST IF DEMO IS COMSCORE                                                      
* R7 = A(INPUT DEMO CATEGORY EQUATE)                                            
* OUTPUT:                                                                       
*        CSFDEMO WILL BE TURN ON IN CSFLAGS                                     
*================================================================               
TSTCS    NTR1  BASE=*,LABEL=*                                                   
         NI    CSFLAGS,X'FF'-CSFDEMO                                            
         XC    CSDEMNAM,CSDEMNAM                                                
*                                                                               
         CLI   DBDEMTYP,C'4'       REFORMAT 4 CHAR INPUT                        
         BNE   TSTCS10                                                          
         OC    1(2,R7),1(R7)       COMSCORE? (##0000##)                         
         JNZ   TSTCSN                                                           
         J     TSTCSY                                                           
*                                                                               
TSTCS10  CLI   2(R7),0             COMSCORE? (00##00)                           
         BNE   TSTCSN                                                           
*                                                                               
TSTCSY   OI    CSFLAGS,CSFDEMO     COMSCORE DEMO                                
         SR    RC,RC                                                            
TSTCSN   LTR   RC,RC                                                            
         J     EXIT                                                             
*================================================================               
* RETURN EXTERNAL FORMAT FOR COMSCORE                                           
* R7 = A(INPUT DEMO CATEGORY EQUATES)                                           
* R8 = A(OUTPUT)                                                                
*================================================================               
GETCS    NTR1  BASE=*,LABEL=*                                                   
         OC    AINNTDM,AINNTDM     TEST VALID CS DEMO NAME LIST                 
         JZ    GETCSX                                                           
*                                                                               
         ZIC   RE,1(R7)            THE N'TH DEMO IN THE LIST                    
         CLI   DBDEMTYP,C'4'       REFORMAT 4 CHAR INPUT                        
         JNE   *+10                                                             
         ZIC   RE,3(R7)                                                         
         ICM   RF,15,AINNTDM       COMSCORE - A(DEMO NAME LIST)                 
*                                                                               
         SHI   RE,1                                                             
         MH    RE,=H'8'            DISPLACEMENT INTO CS DEMO NAME LIST          
         AR    RF,RE               POINT TO CORRECT CS DEMO                     
         LA    RE,8                L'DEMO NAME                                  
         LA    R8,CSDEMNAM         COMSCORE DEMO NAME                           
*                                                                               
         MVI   CSDEMMOD,DEMO_MODIFIER_T    DEFAULT TO T                         
         CLI   0(RF),C'X'                                                       
         JE    GETCS10                                                          
         CLI   0(RF),DEMO_MODIFIER_I                                            
         JE    GETCS10                                                          
         MVC   CSDEMMOD,0(RF)      SET DEMO MODIFIER                            
*                                                                               
GETCS10  CLI   0(RF),C' '                                                       
         JE    GETCSX                                                           
         CLI   0(RF),0                                                          
         JE    GETCSX                                                           
         MVC   0(1,R8),0(RF)                                                    
         AHI   R8,1                                                             
         AHI   RF,1                                                             
         BCT   RE,GETCS10                                                       
*                                                                               
GETCSX   J     EXIT                                                             
*================================================================               
* RETURN ENTIRE RENTRAK DEMO ENTRY FROM TABLE                                   
*================================================================               
GETRTK   NTR1  BASE=*,LABEL=*                                                   
         MVC   INPCOUNT,=H'1'      SET ONLY 1 DEMO CAT FOR THIS CALL            
*                                                                               
         L     RF,AINPUT                                                        
         CLI   TINPLEN,C'N'        RETRIEVE COMSCORE INFO BY DEMO NUM?          
         JE    GTRTK05                                                          
         MVC   CSDEMNAM(7),0(RF)   DEFAULT TO "M1849"                           
         CLI   0(RF),C'X'          XM1849?                                      
         JNE   *+10                                                             
         MVC   CSDEMNAM(7),1(RF)                                                
         CLI   1(RF),C'X'          IXM1849?  (I IS MODIFIER)                    
         JNE   *+10                                                             
         MVC   CSDEMNAM(7),2(RF)                                                
         OC    CSDEMNAM,GCSPACES                                                
         J     GTRTK10                                                          
*                                                                               
GTRTK05  MVC   CSDEMNUM,0(RF)                                                   
         OC    CSDEMNUM,GCSPACES   COMSCORE NUMBER                              
*                                                                               
GTRTK10  LA    R1,BPARAMS                                                       
         USING BSPARA,R1                                                        
*                                                                               
         LA    RF,CSDEMNAM                                                      
         ICM   RE,15,ARTKTAB       A(RENTRAK TABLE)                             
         CLI   TINPLEN,C'N'        RETRIEVE CS INFO BY DEMO NUM?                
         JNE   GTRTK20             NO                                           
         LA    RF,CSDEMNUM                                                      
         ICM   RE,15,ACSLTAB       A(CSLON TABLE)                               
*                                                                               
GTRTK20  STCM  RF,15,BSPAREC       A(INPUT FIELD)                               
         LAM   ARE,ARE,ALET                                                     
         SAC   512                                                              
*                                                                               
         SAM31                                                                  
         MVC   BSPSTRT(24),BSPSTRT-BSPAREC(RE)   BINSRCH PARAMETERS             
*                                                                               
         GOTO1 VBINSRCH,BPARAMS                                                 
*                                                                               
         IF (TM,BPARAMS,X'80',Z)    IF DEMO FOUND THEN                          
           ICM   R1,15,BSPAREC      SET, A(REC IN TABLE) FROM BINSRCH           
           LAM   AR1,AR1,ALET                                                   
           SAC   512                RESET SAC SINCE BINSRCH MAKES IT 0          
         ELSE ,                    ELSE, NOT FOUND THEN                         
           BRAS  RE,GETCCS          VALIDATE & GET CUSTOM COMSCORE DEMO         
           JNE   GTRTKNO             NO ACCESS OR NOT VALID                     
         ENDIF ,                                                                
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,AOUTPUT+1      A(OUTPUT FIELD)                              
*                                                                               
         IF (CLI,TINPLEN,EQ,C'N')  IF RETRIEVE CS INFO BY DEMO NUM?             
           MVC   0(CSDLNQ,RF),0(R1)   RETURN CSDTABD                            
         ELSE ,                    ELSE RETRIEVE CS INFO BY DEMO NAME           
           MVC   0(RENLNQ,RF),0(R1)   RETURN RENTABD                            
         ENDIF ,                                                                
*                                                                               
GTRTKYES SR    RE,RE               SET CC EQUAL                                 
GTRTKNO  LTR   RE,RE               SET CC NOT EQUAL                             
*                                                                               
GETRTKX  LAM   AR0,ARF,=16F'0'                                                  
         SAC   0                                                                
         SAM24                                                                  
*                                                                               
         XIT1                                                                   
*================================================================               
* VALIDATE COMSCORE ACCESS AND GET CUSTOM COMSCORE DEMOS                        
*                                                                               
* ON ENTRY : AEXTBLK = C'DEXB' & CS LICENSE                                     
*          : (TINPLEN = C'N' & CSDEMNUM) OR (TINPLEN = C'C' & CSDEMNAM)         
*                                                                               
* ON EXIT  : TINPLEN= C'N' THEN R1= A(CSDTABD)                                  
*          : TINPLEN= C'C' THEN R1= A(RENTABD)                                  
*================================================================               
         USING GETCCSD,R5                                                       
GETCCS   NTR1  LABEL=*,WORK=(R5,GETCCSL)                                        
*                                                                               
         L     RE,AEXTBLK                                                       
         USING DEXBD,RE                                                         
         IF (CLC,DEXBID,NE,=C'DEXB') DEMOCON EXTENDED BLOCK?                    
           J     GETCCSNO             NONE, EXIT                                
         ENDIF ,                                                                
*                                                                               
         IF (ICM,R3,15,DEXBLIC,Z),OR,         IF NO LICENSE PASSED OR           
             (CLC,0(L'CSFLIC,R3),NH,GCSPACES)  EMPTY LICENSE THEN               
           J     GETCCSNO                      EXIT                             
         ENDIF ,                                                                
*                                                                               
*********                                                                       
* READ COMSCORE LICENSE REC TO CONFIRM LICENSE IS VALID                         
*********                                                                       
         XC    KEY,KEY             LOOK FOR LICENSE IN GENFIL                   
         LA    RE,KEY                                                           
         USING CSLKEY,RE           X'00'                                        
         MVI   CSLKMIN,CSLKMIQ     C'D' - DEMO SYSTEM                           
         MVI   CSLKREC,CSLKREQ     C'L' - LICENSE RECORD                        
         MVC   CSLKLIC,0(R3)       FIRST 10 CHARS OF LICENSE                    
         DROP  RE                                                               
                                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 CDATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY                  
         IF (CLC,KEY(CSLKLIC+L'CSLKLIC-CSLKEY),NE,KEYSAVE)                      
           J     GETCCSNO          EXIT, LICENSE IS NOT FOUND                   
         ENDIF ,                                                                
*                                                                               
*********                                                                       
* WE'RE ONLY CHECKING ONE PHYSICAL LICENSE RECORD FOR NOW, BUT THERE            
* COULD BE MORE LICENSE RECORDS FOR THE SAME FIRST 10 CHARACTERS                
*********                                                                       
         GOTO1 CDATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,GCIOAREA,             
           DMWORK                                                               
         JNE   GETCCSNO            EXIT, LICENSE IS NOT FOUND                   
*                                                                               
         LA    RE,GCIOAREA                                                      
         USING CSLKEY,RE                                                        
         LLH   RF,CSLRLEN          RECORD LENGTH                                
         LA    RF,0(RF,RE)         RF=A(EOR)                                    
         LA    RE,CSLFIRST(RE)     POINT TO THE FIRST ELEMENT                   
         DROP  RE                                                               
*********                                                                       
* SCAN LICENSE ELEMENTS, X'0F', IN LICENSE RECORD TO CONFIRM FULL               
* LICENSE (32 BYTES) MATCHES                                                    
*********                                                                       
         DO INF                                                                 
           IF (CR,RE,NL,RF),OR,      BEYOND EOR OR                              
               (CLI,0(RE),EQ,0) ,    END OF RECORD?                             
             J     GETCCSNO          EXIT, LICENSE IS NOT FOUND                 
           ENDIF ,                                                              
*                                                                               
           USING CSFLD,RE                                                       
           IF (CLI,0(RE),EQ,CSFLELQ),AND,  HAVE X'0F'-LICENSE ELEM              
               (CLC,CSFLIC,EQ,0(R3)) ,     AND MATCH ON LICENSE?                
             MVC   HALF,CSFLSEQ        YES, STORE SEQ# IN HALF                  
             ASMLEAVE ,                DONE, FOUND LICENSE                      
           ENDIF ,                                                              
           DROP  RE                                                             
*                                                                               
           LLC   R1,1(RE)            GET L'(CURR ELEM) AND                      
           LA    RE,0(R1,RE)         BUMP TO NEXT ELEM                          
         ENDDO ,                                                                
*                                                                               
*********                                                                       
* IF RETRIEVE BY CS DEMO NUMBER, READ CUSTOM COMSCORE DEMO CATEGORY             
* PASSIVE (NUMBER), TO GET CS DEMO NAME (OXCODE)                                
*********                                                                       
         IF (CLI,TINPLEN,EQ,C'N')  RETRIEVE CS INFO BY DEMO NUM?                
           XC    KEY,KEY           YES, READ CS DEMO CAT PASSIVE                
           LA    RE,KEY                                                         
           USING CDCKEY,RE         X'00'                                        
           MVI   CDCKMIN,CDCKMIQ   C'D' - DEMO SYSTEM                           
           MVI   CDCKREC,CDCKREQ   C'C' - CUSTOM DEMO CAT RECORD                
           PACK  DUB,CSDEMNUM                                                   
           CVB   R1,DUB                                                         
           STCM  R1,15,CDCKNUM                                                  
*          MVC   CDCKNUM,CSDEMNUM  CS DEMO NUMBER                               
           MVC   KEYSAVE,KEY                                                    
           GOTO1 CDATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY                
           IF (CLC,KEY(CDCKOXC-CDCKEY),NE,KEYSAVE)                              
             J     GETCCSNO        EXIT, NOT A CUSTOM DEMO                      
           ENDIF ,                                                              
           LA    RE,KEY                                                         
           MVC   CSDEMNAM,CDCKOXC  SET CS DEMO NAME (OXCODE)                    
           DROP  RE                                                             
         ENDIF ,                                                                
*                                                                               
*********                                                                       
* CHECK IF LICENSE HAS ACCESS TO CUSTOM COMSCORE OXCODE                         
*********                                                                       
         XC    KEY,KEY             LOOK FOR LICENSE/OXCODE IN GENFIL            
         LA    RE,KEY                                                           
         USING LDCKEY,RE           X'00'                                        
         MVI   LDCKMIN,LDCKMIQ     C'D' - DEMO SYSTEM                           
         MVI   LDCKREC,LDCKREQ     C'D' - LICENSE DEMO CAT RECORD               
         MVC   LDCKLIC,0(R3)       1ST 10 CHARACTERS OF LICENSE                 
         MVC   LDCKSEQ,HALF        LICENSE SEQ#                                 
         MVC   LDCKOXC,CSDEMNAM    OXCODE                                       
         DROP  RE                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 CDATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY                  
         IF (CLC,KEY(LDCKEYL),NE,KEYSAVE)                                       
           J     GETCCSNO          EXIT, NOT IN TABSDEM NOR CUSTOM DEMO         
         ENDIF ,                                                                
*                                                                               
*********                                                                       
* GET CUSTOM COMSCORE DEMO CATEGORY (OXCODE) RECORD                             
*********                                                                       
         XC    KEY,KEY             GET THE OXCODE IN GENFIL                     
         LA    RE,KEY                                                           
         USING CDCKEY,RE           X'00'                                        
         MVI   CDCKMIN,CDCKMIQ     C'D' - DEMO SYSTEM                           
         MVI   CDCKREC,CDCKREQ     C'C' - CUSTOM DEMO CAT RECORD                
         MVC   CDCKOXC,CSDEMNAM    CS DEMO NAME / OXCODE                        
         DROP  RE                                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 CDATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY                  
         IF (CLC,KEY(CDCKEYL),NE,KEYSAVE)                                       
           J     GETCCSNO          NOT IN TABSDEM NOR CUSTOM DEMO               
         ENDIF ,                                                                
*                                                                               
         GOTO1 CDATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,GCIOAREA,             
           DMWORK                                                               
         JNE   GETCCSNO            NOT IN TABSDEM NOR CUSTOM DEMO               
*                                                                               
*********                                                                       
* GET SYSTEM EQUATE                                                             
*********                                                                       
         IF (ICM,RE,15,CMASTC,NZ)          IF OFFLINE THEN                      
           MVC   GCOVSYS,MCOVSYS-MASTD(RE)  SAVE SYSTEM EQUATE                  
         ELSE ,                            ELSE ONLINE THEN                     
           IF (ICM,RE,15,CGETFACT,NZ)       CALL GETFACT TO GET                 
             GOTO1 CGETFACT,DMCB,(X'80',GCOVSYS),F#BOVSYS SYSTEM EQUATE         
           ENDIF ,                                                              
         ENDIF ,                                                                
*                                                                               
*********                                                                       
* CONFIRM REQUESTED SYS(NATIONAL/NET OR LOCAL/SPOT) ACCESS IS ON                
* VIA THE X'0A' CUSTOM ELEMENT CDCFLAG                                          
*********                                                                       
         LA    RE,GCIOAREA                                                      
         USING CDCKEY,RE                                                        
         LLH   RF,CDCRLEN          RECORD LENGTH                                
         LA    RF,0(RF,RE)         RF=A(EOR)                                    
         LA    RE,CDCFIRST(RE)     POINT TO THE FIRST ELEMENT                   
         DROP  RE                                                               
         DO INF                                                                 
           IF (CR,RE,NL,RF),OR,    BEYOND EOR OR                                
               (CLI,0(RE),EQ,0) ,  END OF RECORD?                               
             J     GETCCSNO        DON'T KNOW IF LOCAL OR NAT'L                 
           ENDIF ,                                                              
           USING CDCELD,RE                                                      
           IF (CLI,CDCEL,EQ,CDCELQ)       IF X'0A'-CUSTOM DEMOELEM              
             IF (CLI,GCOVSYS,EQ,3),AND,         IF NAT'L REQ'D THEN             
                 (TM,CDCFLAG,CDCFNAT,O),ORIF, ,   IS NAT'L ON?  -ORIF-          
                (CLI,GCOVSYS,EQ,2),AND,       , IF LOCAL REQ'D THEN             
                 (TM,CDCFLAG,CDCFLOC,O) ,         IS LOCAL ON?                  
               ASMLEAVE ,          YES ACCESS ON, RE=A(CUSTOM DEMOELEM)         
             ENDIF                                                              
           ENDIF ,                                                              
           LLC   R1,1(RE)          GET L'(CURR ELEM) AND                        
           LA    RE,0(R1,RE)       BUMP TO NEXT ELEM                            
         ENDDO ,                   LOOP CUSTOM DEMO CAT ELEMS                   
*********                                                                       
* ACCESS IS ON, BUILD AND RETURN RENTABD BLOCK                                  
*********                                                                       
         ICM   R0,15,CDCNUM                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  GCDESC,DUB            SET TAG# - HAS LEADING 0'S                 
*                                                                               
         LA    RF,GCIOAREA                                                      
         USING CDCKEY,RF                                                        
         XC    GCOUTTAB,GCOUTTAB                                                
         LA    R1,GCOUTTAB           BUILD OUR OUTPUT TABLE                     
         IF (CLI,TINPLEN,EQ,C'N')    RETRIEVE CS INFO BY DEMO NUM?              
           USING CSDTABD,R1                                                     
           MVC   CSDDESC,GCDESC      SET TAG# - HAS LEADING 0'S                 
           MVC   CSDCODE,CDCKOXC     MOVE CS DEMO NAME (OXCODE)                 
           DROP  R1                                                             
         ELSE ,                      NO, RETRIVE BY NAME                        
           USING RENTABD,R1                                                     
           MVC   RENCODE,CDCKOXC     MOVE CS DEMO NAME (OXCODE)                 
           MVC   RENNAME,CDCNAME     MOVE COMSCORE NAME                         
           LA    R0,L'GCDESC                                                    
           DO FROM=(R0),WHILE=(CLI,GCDESC,EQ,C'0') LEFT JUSTIFY                 
             MVC   GCDESC(L'GCDESC),GCDESC+1      & REM LEADING ZEROS           
             MVI   GCDESC+L'GCDESC-1,C' '         & ADD TRAILING SPACES         
           ENDDO ,                                                              
           MVC   RENDESC,GCDESC      SET TAG# - LEFT JUSTIFIED                  
           DROP  R1                                                             
         ENDIF ,                                                                
         DROP  RE,RF                                                            
*                                                                               
GETCCSYS SR    RE,RE               SET CC EQUAL                                 
GETCCSNO LTR   RE,RE               SET CC NOT EQUAL                             
         XIT1  REGS=(R1)                                                        
GCSPACES DC    CL32' '                                                          
         LTORG                                                                  
*                                                                               
GETCCSD  DSECT                                                                  
GCDESC   DS    CL(L'CSDDESC)       COMSCORE NUMBER                              
GCOUTTAB DS    XL(RENLNQ)          RENTAB OUTPUT TABLE                          
GCOVSYS  DS    X                                                                
GCIOAREA DS    XL2000              GCIOAREA FOR GENFIL                          
GETCCSL  EQU   *-GETCCSD                                                        
DEMOCON  RSECT                                                                  
*================================================================               
* LOOKUP/TABLES FOR EXTENDED USER DEMOS                                         
* ON ENTRY R7 POINTS TO X'0021'USRDEM                                           
* ON EXIT R1 POINTS TO USER DEMO NAME                                           
*================================================================               
*                                                                               
GETEUD   NTR1  BASE=*,LABEL=*                                                   
         SR    RE,RE                                                            
         IC    RE,2(R7)            GET DEMO CHARACTER                           
         SHI   RE,C'A'             X'C1' IS FIRST ENTRY                         
         LA    RE,CHARDSP(RE)      GIVES INDEX INTO NAME TABLE                  
         SR    R1,R1                                                            
         IC    R1,0(RE)                                                         
         BCTR  R1,0                FIRST ENTRY IS NUMBER 1                      
         MHI   R1,6                                                             
         LA    R1,UMSCK(R1)                                                     
         XIT1  REGS=(R1)                                                        
         LTORG                                                                  
*                                                                               
* TABLE GIVES DISPLACEMENTS OF ALPHA CHARACTERS TO ENTRIES                      
* IN A-Z, 0-9 TABLE                                                             
* E.G., 0 POINTS TO THE 27TH ENTRY IN THE NAME TABLE                            
*  BECAUSE IT FOLLOWS THE 26 ENTRIES FOR A-Z                                    
*                                                                               
CHARDSP  DS    0H                                                               
         DC    AL1(01,02,03,04,05,06,07,08,09)    C1(A) - C9(I)                 
         DC    7X'00'                             CA  -  D0                     
         DC    AL1(10,11,12,13,14,15,16,17,18)    D1(J) - D9(R)                 
         DC    8X'00'                             DA  -  E1                     
         DC    AL1(19,20,21,22,23,24,25,26)       E2  -  E9(Z)                  
         DC    6X'00'                             EA-EF                         
         DC    AL1(27,28,29,30,31,32,33,34,35,36) F0(0) - F9(9)                 
*                                                                               
* TABLE BELOW IS FOR MINDSHARE/COKE - SO FAR ONLY USER                          
* 0-9, A-H ARE RATINGS                                                          
* THAT FACT IS HARDCODED IN SPOT/BUY                                            
*                                                                               
UMSCK    DS    0D                                                               
         DC    CL6'RUSERA'         A                                            
         DC    CL6'RUSERB'         B                                            
         DC    CL6'RUSERC'         C                                            
         DC    CL6'RUSERD'         D                                            
         DC    CL6'RUSERE'         E                                            
         DC    CL6'RUSERF'         F                                            
         DC    CL6'RUSERG'         G                                            
         DC    CL6'RUSERH'         H                                            
         DC    CL6'AD1224'         I                                            
         DC    CL6'AD1234'         J                                            
         DC    CL6'WM2549'         K                                            
         DC    CL6'AD1249'         L                                            
         DC    CL6'AD3549'         M                                            
         DC    CL6'USERN '         N                                            
         DC    CL6'USERO '         O                                            
         DC    CL6'USERP '         P                                            
         DC    CL6'USERQ '         Q                                            
         DC    CL6'USERR '         R                                            
         DC    CL6'USERS '         S                                            
         DC    CL6'USERT '         T                                            
         DC    CL6'USERU '         U                                            
         DC    CL6'USERV '         V                                            
         DC    CL6'USERW '         W                                            
         DC    CL6'USERX '         X                                            
         DC    CL6'USERY '         Y                                            
         DC    CL6'USERZ '         Z                                            
         DC    CL6'EA1224'         0                                            
         DC    CL6'EA1234'         1                                            
         DC    CL6'EW2549'         2                                            
         DC    CL6'EA1249'         3                                            
         DC    CL6'EA3549'         4                                            
         DC    CL6'RUSER5'         5                                            
         DC    CL6'RUSER6'         6                                            
         DC    CL6'RUSER7'         7                                            
         DC    CL6'RUSER8'         8                                            
         DC    CL6'RUSER9'         9                                            
         EJECT                                                                  
         DS    0H                                                               
DEMO_PLD_PRINT  DS    0C                                                        
*                                                                               
         DC    AL1(PLD_SPANISH_ONLY)                                            
         DC    CL2'SO'                  2                                       
         DC    CL8'SPANONLY'            4X4                                     
         DC    CL10'SPAN ONLY '         5X5                                     
         DC    CL5'SONLY'               5                                       
         DC    CL6'SPONLY'              6                                       
         DC    CL7'SPAONLY'             7                                       
         DC    CL14'SPANISH ONLY  '     7X7                                     
*                                                                               
         DC    AL1(PLD_MOSTLY_SPANISH)                                          
         DC    CL2'MS'                  2                                       
         DC    CL8'MOSTSPAN'            4X4                                     
         DC    CL10'MOST SPAN '         5X5                                     
         DC    CL5'MSTSP'               5                                       
         DC    CL6'MOSTSP'              6                                       
         DC    CL7'MOSTSPA'             7                                       
         DC    CL14'MOSTLY SPANISH'     7X7                                     
*                                                                               
         DC    AL1(PLD_ENGLISH_ONLY)                                            
         DC    CL2'EO'                  2                                       
         DC    CL8'ENGLONLY'            4X4                                     
         DC    CL10'ENGL ONLY '         5X5                                     
         DC    CL5'EONLY'               5                                       
         DC    CL6'ENONLY'              6                                       
         DC    CL7'ENGONLY'             7                                       
         DC    CL14'ENGLISH ONLY  '     7X7                                     
*                                                                               
         DC    AL1(PLD_MOSTLY_ENGLISH)                                          
         DC    CL2'ME'                  2                                       
         DC    CL8'MOSTENGL'            4X4                                     
         DC    CL10'MOST ENGL '         5X5                                     
         DC    CL5'MSTEN'               5                                       
         DC    CL6'MOSTEN'              6                                       
         DC    CL7'MOSTENG'             7                                       
         DC    CL14'MOSTLY ENGLISH'     7X7                                     
*                                                                               
         DC    AL1(PLD_ENGLISH_SPANISH)                                         
         DC    CL2'ES'                  2                                       
         DC    CL8'ENGLSPAN'            4X4                                     
         DC    CL10'ENGL SPAN '         5X5                                     
         DC    CL5'ENGSP'               5                                       
         DC    CL6'ENGSPA'              6                                       
         DC    CL7'ENGSPAN'             7                                       
         DC    CL14'ENG/SPA EQUAL '     7X7                                     
*                                                                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
       ++INCLUDE DEPLDXLATE                                                     
         EJECT                                                                  
       ++INCLUDE DEPLDCODTB                                                     
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER PARM LIST                                            *         
***********************************************************************         
*                                                                               
PARMD    DSECT                                                                  
TINPLEN  DS    0XL1                NUMBER IN INPUT LIST (DEFLT=1)               
AINPUT   DS    A                   A(INPUT EXPRESSON)                           
TOUTPUT  DS    0XL1                OUTPUT TYPE                                  
AOUTPUT  DS    A                   A(OUTPUT EXPRESSION)                         
TCNTRL   DS    0XL1                C'S'=SPOTPAK,C'D'=DEMO NAME ONLY             
ABLOCK   DS    A                   A(DBLOCK)                                    
AUSRNMS  DS    A                   A(USER NAME LIST) (TCNTRL=S)                 
         ORG   AUSRNMS                                                          
APLDLST  DS    A                   A(PERSONAL LANGUAGE DEMO ARRAY)              
*                                   (WITH RELEVANT OUTPUT TYPES)                
         ORG   AUSRNMS                                                          
AEXTBLK  DS    A                   A(EXTENDED BLOCK)                            
*                                                                               
AINNTDM  DS    A                   A(NON-TRAD DEMO INDEXED LIST)                
TCNTRL2  DS    0XL1                C'R'=RETRIEVE RENTRAK ENTRY                  
AOUTNTRY DS    A                   A(OUTPUT FOR TABLE ENTRY)                    
         SPACE 2                                                                
***********************************************************************         
* DEMOCON EXTENDED BLOCK                                                        
***********************************************************************         
DEXBD    DSECT                                                                  
DEXBID   DS    CL4                 C'DEXB' - UNIQUE TABLE EYE CATCHER           
DEXBLIC  DS    A                   A(32 BYTE COMSCORE LICENSE)                  
         DS    9A                  SPARE                                        
DEXBDLNQ EQU   *-DEXBD                                                          
*                                                                               
***********************************************************************         
* DSECT TO COVER W/S                                                  *         
***********************************************************************         
*                                                                               
DEMWORK  DSECT                                                                  
DUB      DS    D                                                                
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
SAVERE   DS    A                                                                
RELO     DS    F                                                                
APLDPTR  DS    A                   A(CURRENT PERSONAL LANGUAGE ENTRY)           
ALET     DS    A                                                                
INPCOUNT DS    H                                                                
INTFIL   DS    H                                                                
*                                                                               
CSFLAGS  DS    XL1                 COMSCORE FLAGS                               
CSFDEMO  EQU   X'80'               COMSCORE DEMO                                
CSFCAT   EQU   X'40'               LOOK UP BY CATEGORY                          
CSFNUM   EQU   X'20'               LOOK UP BY NUMBER                            
CSDEMMOD DS    CL1                 COMSCORE DEMO MODIFIER                       
CSDEMNAM DS    CL8                 COMSCORE DEMO NAME                           
CSDEMNUM DS    CL12                COMSCORE DEMO NUMBER                         
*                                                                               
         DS    0A                                                               
TABLIST  DS    XL(TABLISTQ)        MUST MAP TO TABLISTL                         
         ORG   TABLIST                                                          
AMODTAB  DS    A                                                                
ANAMTAB  DS    A                                                                
AFMTAB   DS    A                                                                
ARTKTAB  DS    A                                                                
ACSLTAB  DS    A                                                                
         DS    X                   NEEDED FOR EOL INDICATOR                     
         ORG                                                                    
*                                                                               
* PERSONAL LANGUAGE DEMO MODIFIER TRANSLATION TABLES                            
*                                                                               
         DS    0A                                                               
APLDTABS DS    0XL(6*4)            6 ADDRESSES                                  
APLMODTB DS    A                   A(MODIFIER TABLE)                            
APLPLDTB DS    A                   A(PERSONAL LANG. ATTRIBUTE TABLE)            
APLENCTB DS    A                   A(MODIFIER BYTE ENCODING TABLE)              
APLPRNTB DS    A                   A(PERSONAL LANG. PRINTABLE DESCRIPS)         
         DS    A                   SPARE                                        
         DS    A                   SPARE                                        
*                                                                               
DEMOGRP  DS    X                                                                
DEMOCAT  DS    X                                                                
DEMOEXP  DS    0XL3                                                             
DEMOSTAT DS    X                                                                
DEMOMOD  DS    X                                                                
DEMONUM  DS    X                                                                
CATNAME  DS    CL7                                                              
CATNUM   DS    CL4                                                              
CATNAMEL DS    X                   ACTUAL LENGTH OF CATNAME                     
NOMOD    DS    CL6                                                              
PREVCATN DS    X                   PREVIOUS CATEGORY NUMBER                     
R3OK     DS    X                                                                
R4OK     DS    X                                                                
PLDCHAR  DS    C                   DECODED PLD CHARACTER                        
BYTE     DS    X                                                                
HALF     DS    H                                                                
FULL     DS    F                                                                
*                                                                               
VBINSRCH DS    A                   A(BINSRCH)                                   
BPARAMS  DS    XL32                                                             
*                                                                               
KEY      DS    XL48                                                             
KEYSAVE  DS    XL48                                                             
*                                                                               
DEMWORKL EQU   *-DEMWORK                                                        
         EJECT                                                                  
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
*                                                                               
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
*                                                                               
* DEDEMEQUS2                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEDEMEQUS2                                                     
         PRINT ON                                                               
*                                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
*                                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
*                                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*                                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         PRINT ON                                                               
         PRINT OFF                                                              
       ++INCLUDE DERENTABD                                                      
         PRINT ON                                                               
         PRINT OFF                                                              
       ++INCLUDE GEGENCDC                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032DEDEMOCON 07/17/19'                                      
         END                                                                    
