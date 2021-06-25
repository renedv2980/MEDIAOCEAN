*          DATA SET DENTIMRK   AT LEVEL 007 AS OF 05/01/02                      
*PHASE DENTIMRA DENTIMRK                                                        
NUMCATS  EQU   10                                                               
DENTIMRK CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DENTIMRK,RA,R3,R4,RR=RE                                        
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         SPACE 1                                                                
         ST    RE,RELO                                                          
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC POINTS TO RATING SERVICE RECORD           
         USING MIREC,RC                                                         
         SPACE 1                                                                
         L     R2,AIREC            R2 POINTS TO INTERIM RECORD - SORT           
         USING INTERD,R2           KEY, VALUES, AND DEMO VALUES                 
         SPACE 1                                                                
         B     *+4(R1)             ROUTINE HOOK                                 
         SPACE 1                                                                
         B     READ                PROCESS INPUT TAPE                           
         B     EXIT                SORT RECORD HOOK                             
         B     ENDJOB              E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
READ     DS    0H                                                               
         CLI   INTAPESW,1          TEST FIRST TIME IN                           
         BE    ENDJOB              NO                                           
         LA    RE,COMWRK           SAVE ADDR OF COMMON WRK AREA BETWN           
         ST    RE,ACOMWRK            INPUT AND OUTPUT PHASES                    
         CLI   RELOFRST,1          TEST FOR RELOCATED DTF ADDRESS               
         BNE   ENDJOB                                                           
         MVI   RELOFRST,0                                                       
         L     RE,ARREC                                                         
         XC    0(4,RE),0(RE)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,RE),=H'401'     HEADER                                       
         MVI   INTAPESW,X'40'      DROP RECORD                                  
                                                                                
         BAS   RE,MARK             READIN BITMAP,MARK #'S,WRITOUT MAP           
                                                                                
ENDJOB   DS    0H                                                               
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
*********************************************************************           
*MARK -  READ IN BITMAP FROM LIVE FILE.  MARK DDS NUMBER.  WRITE OUT            
*  NEW MAP TO FILE.                                                             
*********************************************************************           
MARK     NTR1                                                                   
         L     R5,VBITMAP1                                                      
         ICM   R5,8,=C'S'          FOR NSS PCKTPC -> SYND MAP                   
         GOTO1 VNTIPRG,DMCB,=C'BLDM',(R5),0,RR=RELO                             
                                                                                
*&&DO                                                                           
         DS    0H         COMMENT IN TO WRITE OUT ORIGINAL BITMAP               
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,VBITMAP1),0                             
*&&                                                                             
                                                                                
         LA    R3,NUMTAB           DDS PROGRAM NUMBERS TO SET                   
MARKMAP  CLI   0(R3),X'FF'                                                      
         BE    RELMAP                                                           
         MVC   DDSNUM,0(R3)                                                     
         GOTO1 VNTIPRG,DMCB,=C'MARK',(0,VBITMAP1),DDSNUM,RR=RELO                
         LA    R3,L'NUMTAB(R3)                                                  
         B     MARKMAP                                                          
*                                                                               
RELMAP   DS    0H                  RELEASE UPDATED BITMAP                       
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,VBITMAP1),0                             
         XIT1                                                                   
***************************************************                             
*NUMTAB - TABLE OF PROGRAM NUMBERS TO SET IN MAP                                
***************************************************                             
NUMTAB   DS    0XL2                DDS# 2501-2505                               
         DC    X'09C5'             102555                                       
         DC    X'09C6'             103199                                       
         DC    X'09C7'             103203                                       
         DC    X'09C8'             103205                                       
         DC    X'09C9'             103206                                       
         DC    X'FF'                                                            
*                                                                               
* LITERAL POOL                                                                  
         LTORG                                                                  
                                                                                
**********************************************************************          
* ACOMWRK- COMMON WORK AREA BETWEEN INPUT AND OUTPUT PHASE                      
*          DO NOT MESS AROUND WITH ORDER OF VARIABLES--MAKE SURE                
*          OUTPUT PHASE AGREES WITH ANY CHANGES                                 
**********************************************************************          
COMWRK   DS    0H                                                               
AVGWKS   DC    X'00'               # WEEKS IN AVG: '00','01','04','05'          
STDATE   DS    CL6                 START DATE OF IREC                           
**********************************************************************          
         EJECT                                                                  
                                                                                
**********************************************************************          
* WORKING STORAGE                                                               
**********************************************************************          
         DS    0D                                                               
RELO     DS    A                                                                
RELOFRST DC    X'01'                                                            
BYPASS01 DC    X'00'               BYPASS SINGLE DAY AVERAGES                   
DDSNUM   DC    XL2'0000'                                                        
                                                                                
**********************************************************************          
* FILE DCB AND BUFFER AREA                                                      
**********************************************************************          
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=401,                                              X        
               BLKSIZE=16842,                                          X        
               MACRF=GM,                                               X        
               EODAD=ENDJOB                                                     
                                                                                
         DS    0F                                                               
*        DENTHID                                                                
       ++INCLUDE DENTHID                                                        
         EJECT                                                                  
*        DEINTD                                                                 
       ++INCLUDE DEINTD                                                         
         SPACE 1                                                                
*        DEINTNTID                                                              
       ++INCLUDE DEINTNT3D                                                      
         EJECT                                                                  
*        DECALVPHD                                                              
       ++INCLUDE DECALVPHD                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
*        DEDEMCNVD                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DENTIMRK  05/01/02'                                      
         END                                                                    
**********************************************************************          
*              NAD DEMO GROUPS                                                  
**********************************************************************          
                                                                                
**********************************************************************          
* RI..=  INTERIM RECD SLOTS FOR DEMOS  (ALMOST THE SAME AS OUTPUT)              
**********************************************************************          
RIF25    EQU   0                   W2-5                                         
RIF68    EQU   1                   W6-8                                         
RIF911   EQU   2                   W9-11                                        
RIF1214  EQU   3                   W12-14                                       
RIF1517  EQU   4                   W15-17                                       
RIF1820  EQU   5                   W18-20                                       
RIF2124  EQU   6                   W21-24                                       
RIF2529  EQU   7                   W25-29                                       
RIF3034  EQU   8                   W30-34                                       
RIF3539  EQU   9                   W35-39                                       
RIF4044  EQU   10                  W40-44                                       
RIF4549  EQU   11                  W45-49                                       
RIF5054  EQU   12                  WW50-54                                      
RIF5564  EQU   13                  WW55-64                                      
RIF65O   EQU   14                  WW65+                                        
*                                                                               
RIM25    EQU   15                  M2-5                                         
RIM68    EQU   16                  M6-8                                         
RIM911   EQU   17                  M9-11                                        
RIM1214  EQU   18                  M12-14                                       
RIM1517  EQU   19                  M15-17                                       
RIM1820  EQU   20                  M18-20                                       
RIM2124  EQU   21                  M21-24                                       
RIM2529  EQU   22                  M25-29                                       
RIM3034  EQU   23                  M30-34                                       
RIM3539  EQU   24                  M35-39                                       
RIM4044  EQU   25                  M40-44                                       
RIM4549  EQU   26                  M45-49                                       
RIM5054  EQU   27                  M50-54                                       
RIM5564  EQU   28                  M55-64                                       
RIM65O   EQU   29                  M65+                                         
*                                                                               
RIF1217  EQU   30                  W1217                                        
RIM1217  EQU   31                  M1217                                        
RIHOMES  EQU   32                  HOMES                                        
RIUSA    EQU   33                  USA HOMES                                    
RIL1849  EQU   34                  LOH1849   (LADY OF HOUSE)                    
RIL50O   EQU   35                  LOH50+                                       
RIWW1849 EQU   36                  WW1849                                       
RIWW50O  EQU   37                  WW50+                                        
RIPW1849 EQU   38                  PTWW1849  (PART TIME WORKING WOMEN)          
RIPW50O  EQU   39                  PTWW50+                                      
RIMOMS   EQU   40                  WMOMS  ** NOT USED ***                       
RIF611   EQU   41                  W6-11                                        
RIM611   EQU   42                  M6-11                                        
RIWW1820 EQU   43                  WW1820                                       
RIWW2124 EQU   44                  WW2124                                       
RIWW2534 EQU   45                  WW2534                                       
RIWW3544 EQU   46                  WW3544                                       
RIWW4549 EQU   47                  WW4549                                       
RIWW5054 EQU   48                  WW5054                                       
RIWW55O  EQU   49                  WW55O                                        
RIL1824  EQU   50                  L1824                                        
RIL2534  EQU   51                  L2534                                        
RIL3544  EQU   52                  L3544                                        
RIL4549  EQU   53                  L4549                                        
RIL5054  EQU   54                  L5054                                        
RIL55O   EQU   55                  L55+                                         
         EJECT                                                                  
**********************************************************************          
* RT..=  INPUT TAPE DISPLACEMENTS FOR  --TOTAL SAMPLE--  DEMOS                  
**********************************************************************          
*                                                                               
RTF25    EQU   1                                                                
RTM25    EQU   16                                                               
RTF68    EQU   2                                                                
RTM68    EQU   17                                                               
RTF911   EQU   3                                                                
RTM911   EQU   18                                                               
RTF1214  EQU   4                                                                
RTM1214  EQU   19                                                               
RTF1517  EQU   5                                                                
RTM1517  EQU   20                                                               
RTF1820  EQU   6                                                                
RTM1820  EQU   21                                                               
RTF2124  EQU   7                                                                
RTM2124  EQU   22                                                               
RTF2529  EQU   8                                                                
RTM2529  EQU   23                                                               
RTF3034  EQU   9                                                                
RTM3034  EQU   24                                                               
RTF3539  EQU   10                                                               
RTM3539  EQU   25                                                               
RTF4044  EQU   11                                                               
RTM4044  EQU   26                                                               
RTF4549  EQU   12                                                               
RTM4549  EQU   27                                                               
RTF5054  EQU   13                                                               
RTM5054  EQU   28                                                               
RTF5564  EQU   14                                                               
RTM5564  EQU   29                                                               
RTF65O   EQU   15                                                               
RTM65O   EQU   30                                                               
*                                                                               
RTWW1820 EQU   33                                                               
RTWW2124 EQU   34                                                               
RTWW2534 EQU   35                                                               
RTWW3544 EQU   36                                                               
RTWW4549 EQU   37                                                               
RTWW5054 EQU   38                                                               
RTWW55O  EQU   39                                                               
RTPW1849 EQU   41                                                               
RTPW50O  EQU   42                                                               
RTL1824  EQU   43                                                               
RTL2534  EQU   44                                                               
RTL3544  EQU   45                                                               
RTL4549  EQU   46                                                               
RTL5054  EQU   47                                                               
RTL55O   EQU   48                                                               
         EJECT                                                                  
**********************************************************************          
* RG..=  INPUT TAPE DISPLACEMENTS FOR  --GROUP--  DEMOS                         
**********************************************************************          
*                                                                               
RGF25    EQU   501                                                              
RGM25    EQU   516                                                              
RGF611   EQU   502                                                              
RGM611   EQU   517                                                              
RGF1217  EQU   504                                                              
RGM1217  EQU   519                                                              
RGF1820  EQU   506                                                              
RGM1820  EQU   521                                                              
RGF2124  EQU   507                                                              
RGM2124  EQU   522                                                              
RGF2529  EQU   508                                                              
RGM2529  EQU   523                                                              
RGF3034  EQU   509                                                              
RGM3034  EQU   524                                                              
RGF3539  EQU   510                                                              
RGM3539  EQU   525                                                              
RGF4044  EQU   511                                                              
RGM4044  EQU   526                                                              
RGF4549  EQU   512                                                              
RGM4549  EQU   527                                                              
RGF5054  EQU   513                                                              
RGM5054  EQU   528                                                              
RGF5564  EQU   514                                                              
RGM5564  EQU   529                                                              
RGF65O   EQU   515                                                              
RGM65O   EQU   530                                                              
RGWW1849 EQU   533                                                              
RGWW50O  EQU   534                                                              
RGL1849  EQU   535                                                              
RGL50O   EQU   536                                                              
         EJECT                                                                  
