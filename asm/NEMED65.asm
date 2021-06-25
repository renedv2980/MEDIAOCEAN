*          DATA SET NEMED65    AT LEVEL 040 AS OF 05/01/02                      
*PHASE T31E65A                                                                  
         TITLE 'T31E65 - PRINT MODULE FOR EVALUATION SUMMARY'                   
T31E65   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NTSU                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,ANETWS2                                                       
         USING SUMWS,R7                                                         
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
*                                                                               
         MVI   NBDATA,C'B'         SELECT PACKAGE AND UNIT RECORDS              
         MVI   NBRESUME,NBPROCPK   MAKE SURE WE GET FIRST PACKAGE               
*        MVI   NBESTOPT,C'Y'       GET ESTIMATED DEMOS                          
         MVI   NBESTOPT,X'04'      GET ESTIMATED DEMOS                          
         MVI   NBSEQ,C'P'          READ IN PROGRAM ORDER                        
         MVI   NBHUNOPT,C'Y'       PROGRAM CAN HANDLE HUNDREDS                  
         OI    NBSPLOPT,X'80'      SPLIT PROD                                   
         EJECT                                                                  
*              CONTROL I/O                                                      
         SPACE 3                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK     GET PACKAGES                           
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST     IF LAST ONE                                  
         BE    LASTONE                                                          
         CLI   NBMODE,NBPROCPK     IF A PACKAGE                                 
         BE    GOTPKG                                                           
         CLI   NBMODE,NBPROCUN     IF A UNIT                                    
         BE    GOTUN                                                            
         B     GETUNIT                                                          
         SPACE 1                                                                
GOTPKG   BAS   RE,PROCPAK                                                       
         B     GETUNIT                                                          
         SPACE 1                                                                
GOTUN    BAS   RE,POSTUNIT         NOW POST THE CURRENT UNIT                    
         B     GETUNIT                                                          
         SPACE 1                                                                
LASTONE  BAS   RE,PRINTEM                                                       
         XIT1                                                                   
         SPACE 1                                                                
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
*              PROCESS A PACKAGE RECORD                                         
         SPACE 3                                                                
PROCPAK  NTR1                                                                   
         LA    R6,NTESD            PUT DETAILS INTO LIST                        
         USING ACBLOCK,R6                                                       
         SPACE 1                                                                
         LA    R2,NUMLINES-1                                                    
         SPACE 1                                                                
NETLOOP  CLC   ACNET,NBACTNET                                                   
         BNE   NEXTNET                                                          
         CLC   ACNO,NBACTPAK       FIND PACKAGE FOR THIS NET                    
         BE    PUTHERE                                                          
         SPACE 1                                                                
NEXTNET  OC    ACNET,ACNET         CK IF END OF LIST                            
         BZ    PUTHERE                                                          
         LA    R6,LINELEN(R6)                                                   
         BCT   R2,NETLOOP                                                       
         SPACE 1                                                                
         MVC   ACNAME,=CL16'OTHERS'     TOO MANY PACKAGES                       
         B     PUT2                                                             
         SPACE 1                                                                
PUTHERE  MVC   ACNO,NBACTPAK       MATCH                                        
         MVC   ACNET,NBACTNET                                                   
         MVC   ACNAME,NBPAKNAM                                                  
         MVC   ACGCPM,NBPAKCPM                                                  
         SPACE 1                                                                
PUT2     MVC   ACDP,NBDPNAM        DAYPART                                      
         L     R1,ACCOST                                                        
         L     R3,NBPAKCST                                                      
         AR    R1,R3                                                            
         ST    R1,ACCOST                                                        
         SPACE 1                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*              POST A UNIT                                                      
         SPACE 3                                                                
POSTUNIT NTR1                                                                   
         LA    R6,NTESD                                                         
         USING ACBLOCK,R6                                                       
         SPACE 1                                                                
         LA    R2,NUMLINES-1                                                    
PULOOP   CLC   ACNET,NBACTNET                                                   
         BNE   PUNEXT                                                           
         CLC   ACNO,NBPACK         FIND PACKAGE FOR THIS NET                    
         BE    GOTIT                                                            
PUNEXT   OC    ACNET,ACNET         CK IF END OF LIST                            
         BZ    NEWPAK                                                           
         LA    R6,LINELEN(R6)                                                   
         BCT   R2,PULOOP                                                        
         B     GOTIT               LIST FULL, USE OTHERS                        
         SPACE 1                                                                
NEWPAK   MVC   ACNO,NBPACK         IF GET HERE, MEANS NO PKG                    
         MVC   ACNET,NBACTNET      RECORD FOR THIS PACKAGE!!!                   
         SPACE 1                                                                
GOTIT    L     R1,ACUNITS          ADD IN UNITS                                 
         AH    R1,NBESTUN                                                       
         ST    R1,ACUNITS                                                       
         LH    R1,NBESTHOM+2       ADD IN HOME GRPS                             
         A     R1,ACHOMGRP                                                      
         ST    R1,ACHOMGRP                                                      
         L     R1,NBESTHOM+4       HOME IMPS                                    
         A     R1,ACHOMIMP                                                      
         ST    R1,ACHOMIMP                                                      
         SPACE 1                                                                
         LA    R2,ACDEMOS          START OF OTHER DEMOS                         
         LA    R3,NDESTDEM                                                      
         ZIC   R4,NDNDEMOS                                                      
         LTR   R4,R4                                                            
         BZ    XITPU                                                            
         LA    R1,NUMDEMS          NO MORE THAN MAX NUM OF DEMOS                
         CR    R4,R1                                                            
         BNH   DEMLOOP                                                          
         LA    R4,NUMDEMS                                                       
         SPACE 1                                                                
*                                  POST DEMOS TO PROPER LINE                    
DEMLOOP  LH    R1,2(R3)            GRPS                                         
         A     R1,0(R2)                                                         
         ST    R1,0(R2)                                                         
         L     R1,4(R3)            IMPS                                         
         A     R1,4(R2)                                                         
         ST    R1,4(R2)                                                         
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,DEMLOOP                                                       
         SPACE 1                                                                
XITPU    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINES TO PRINT ALL PACKAGES FOR A NETWORK                     
*                                AND NETWORK TOTALS                             
*                                                                               
         SPACE 3                                                                
PRINTEM  NTR1                                                                   
         LA    R6,NTESD                                                         
         USING ACBLOCK,R6                                                       
         SPACE 1                                                                
         LA    R3,NUMLINES                                                      
         XC    SAVENET,SAVENET                                                  
         B     EV35                SKIP NET TOTALS FOR FIRST TIME               
         SPACE 1                                                                
EV34     CLC   SAVENET,ACNET                                                    
         BE    EV36                                                             
         BAS   RE,PRINNET                                                       
EV35     XC    NETTOT,NETTOT                                                    
         MVC   SAVENET,ACNET       NEW NETWORK                                  
         MVC   P+2(4),SAVENET                                                   
         SR    R4,R4               COUNT OF PAKS PER NETWORK                    
EV36     OC    ACLINE,ACLINE                                                    
         BZ    ALLDONE                                                          
         LA    R2,NETTOT                                                        
         BAS   RE,ADDLINES         ADD LINE TO NETWORK TOTALS                   
         LA    R2,ESTTOT                                                        
         BAS   RE,ADDLINES         ADD LINE TO ESTIMATE TOTALS                  
         MVC   WORK,SPACES                                                      
         EDIT  (1,ACNO),(3,WORK),WRK=DMCB                                       
         MVC   WORK+4(7),ACDP                                                   
         MVC   WORK+12(16),ACNAME                                               
         OC    ACGCPM,ACGCPM       POSSIBLE GUARANTEED CPM                      
         BZ    EV38                                                             
         MVC   WORK+30(9),=C'GUAR.CPM='                                         
         EDIT  (4,ACGCPM),(8,WORK+39),2,FLOAT=$,ALIGN=LEFT,WRK=DMCB             
EV38     GOTO1 SQUASHER,DMCB,WORK,48                                            
         GOTO1 CHOPPER,DMCB,(48,WORK),(16,P+8),(C'P',3)                         
         BAS   RE,FORMAT                                                        
         LA    R6,LINELEN(R6)                                                   
         LA    R4,1(R4)            COUNT PCKAGES                                
         BCT   R3,EV34                                                          
         SPACE 1                                                                
ALLDONE  MVC   P+8(15),=C'ESTIMATE TOTALS'                                      
         LA    R6,ESTTOT                                                        
         BAS   RE,FORMAT                                                        
         XIT1                                                                   
         EJECT                                                                  
*              ADD LINES TOGETHER (R6 TO R2)                                    
         SPACE 3                                                                
ADDLINES NTR1                                                                   
         LA    R2,ACCOFF(R2)       START OF ACCUMULATORS                        
         LA    R3,ACCOFF(R6)                                                    
         LA    R4,NUMACCS                                                       
         SPACE 1                                                                
ADDL2    L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,ADDL2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              PRINT NETWORK TOTALS                                             
         SPACE 3                                                                
*              INPUT               (R4) CURRENT PACKAGE COUNT                   
         SPACE 1                                                                
PRINNET  NTR1                                                                   
         CH    R4,=H'2'            SUPPRESS TOTALS IF ONLY 1 PACKAGE            
         BL    NTT2                                                             
         LA    R6,NETTOT                                                        
         MVC   P+12(6),=C'TOTALS'                                               
         BAS   RE,FORMAT                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
NTT2     MVC   P,SPACES                                                         
         SPACE 1                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINES TO FORMAT NUMBERS                                       
         SPACE 3                                                                
*              INPUT               R6=A(LINE TO FORMAT)                         
         SPACE 1                                                                
         USING ACBLOCK,R6                                                       
FORMAT   NTR1                                                                   
         EDIT  (4,ACCOST),(8,P+24)                                              
         EDIT  (4,ACUNITS),(8,P2+24)                                            
         ZIC   R4,NDNDEMOS                                                      
         LA    R1,NUMDEMS          CK FOR MAX NUM DEMOS                         
         CR    R4,R1                                                            
         BNH   FM1                                                              
         LA    R4,NUMDEMS                                                       
         SPACE 1                                                                
FM1      LA    R4,1(R4)                                                         
         LA    R2,ACHOMGRP                                                      
         LA    R3,P+34                                                          
         LA    R5,25                                                            
         CH    R4,=H'4'            SET UP FOR 3 DEMO FORMAT                     
         BL    FORMAT2                                                          
         MVI   SPACING,2                                                        
         LA    R5,15                                                            
         CH    R4,=H'6'            OR 5 DEMO                                    
         BL    FORMAT2                                                          
         LA    R3,P+38             ELSE DO THE MULTI VERSION                    
         LA    R5,8                                                             
         MVC   P1+34(4),=C'GRPS'                                                
         MVC   P2+34(4),=C'CPP '                                                
         MVC   P3+34(4),=C'IMPS'                                                
         MVC   P4+34(4),=C'CPM '                                                
         SPACE 1                                                                
FORMAT2  BAS   RE,FORMAT4                                                       
         LA    R2,8(R2)                                                         
         AR    R3,R5               COLUMNAR DISPLACEMENT IN R5                  
         BCT   R4,FORMAT2                                                       
         XC    ACLINE,ACLINE                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO EDIT NUMBERS                                         
         SPACE 3                                                                
FORMAT4  NTR1                                                                   
         SR    R4,R4               GET CPP IN R4                                
         L     R1,ACCOST                                                        
         M     R0,=F'20'                                                        
         OC    0(4,R2),0(R2)                                                    
         BZ    FORMAT6                                                          
         D     R0,0(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R4,R1                                                            
         SPACE 1                                                                
FORMAT6  SR    R5,R5               AND CPM IN R5                                
         L     R1,ACCOST                                                        
         M     R0,=F'2000'                                                      
         OC    4(4,R2),4(R2)                                                    
         BZ    FORMAT8                                                          
         D     R0,4(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R5,R1                                                            
         SPACE 1                                                                
FORMAT8  CLI   NDNDEMOS,3                                                       
         BL    FORMAT10                                                         
         CLI   NDNDEMOS,5                                                       
         BL    FORMAT14                                                         
*                                                   MULTI                       
         NETGO NVPRDEM,DMCB,(C'R',0),0(R2),1(R3)    GRPS                        
         EDIT  (R4),(8,132(R3)),FLOAT=$             CPP                         
         NETGO NVPRDEM,DMCB,(C'I',0),4(R2),265(R3)  IMPS                        
         EDIT  (R5),(8,396(R3)),2,FLOAT=$           CPM                         
         B     XIT                                                              
         SPACE 1                                                                
FORMAT10 EDIT  (4,0(R2)),(5,1(R3)),1                GRPS (3 DEMO)               
         L     R1,0(R2)                                                         
         C     R1,=F'9999'                                                      
         BL    FORMAT12                                                         
         SR    R0,R0                                                            
         AH    R1,=H'5'                                                         
         D     R0,=F'10'                                                        
         EDIT  (R1),(6,0(R3))                                                   
         SPACE 1                                                                
FORMAT12 EDIT  (R4),(5,6(R3))                      CPP                          
         NETGO NVPRDEM,DMCB,(C'I',0),4(R2),12(R3)  IMPS                         
         EDIT  (R5),(6,19(R3)),2                   CPM                          
         B     XIT                                                              
         SPACE 1                                                                
FORMAT14 EDIT  (4,0(R2)),(6,0(R3)),1               GRPS (5 DEMO)                
         L     R1,0(R2)                                                         
         C     R1,=F'99999'                                                     
         BL    FORMAT16                                                         
         SR    R0,R0                                                            
         AH    R1,=H'5'                                                         
         D     R0,=F'10'                                                        
         EDIT  (R1),(6,0(R3))                                                   
         SPACE 1                                                                
FORMAT16 EDIT  (R4),(6,132(R3)),FLOAT=$            CPP                          
         NETGO NVPRDEM,DMCB,(C'I',0),4(R2),7(R3)   IMPS                         
         EDIT  (R5),(8,138(R3)),2,FLOAT=$          CPM                          
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H5+14(20),SPLPRON                                                
         MVC   H6+10(3),SPLEST                                                  
         MVC   H6+14(20),SPLESTN                                                
         CLI   SPLDPTH+5,0                                                      
         BE    HOOK2                                                            
         CLC   SPLDPTN(3),=C'ALL'                                               
         BE    HOOK2                                                            
         MVC   H6+75(8),SPLDPTN                                                 
         MVC   H6+83(09),=C' PACKAGES'                                          
*                                                                               
         OC    H6,SPACES                                                        
         GOTO1 SQUASHER,DMCB,H6+75,20                                           
         SPACE 1                                                                
HOOK2    LA    R2,NDDEMOS                                                       
         SH    R2,=H'3'            SET BACK FOR LOOP                            
         LA    R3,H10+34                                                        
         ZIC   R4,NDNDEMOS                                                      
         LA    R4,1(R4)                                                         
         CH    R4,=H'5'                                                         
         BH    HOOK14                                                           
         CH    R4,=H'3'                                                         
         BH    HOOK8                                                            
         EJECT                                                                  
*              ROUTINES TO PUT DEMOS INTO HEADLINES                             
         SPACE 3                                                                
         MVC   2(23,R3),=C'---------HOMES---------'                             
         B     HOOK6                                                            
         SPACE 1                                                                
HOOK4    MVC   2(23,R3),=23C'-'    HOMES + 2 FLAVOR                             
         BAS   RE,HOOKDEM                                                       
         MVC   10(7,R3),WORK                                                    
         CLI   15(R3),C' '                                                      
         BNE   *+8                                                              
         MVI   15(R3),C'-'                                                      
         CLI   16(R3),C' '                                                      
         BNE   *+8                                                              
         MVI   16(R3),C'-'                                                      
         SPACE 1                                                                
HOOK6    MVC   134(23,R3),=C'GRPS  CPP   (000)   CPM'                           
         LA    R2,3(R2)                                                         
         LA    R3,25(R3)                                                        
         BCT   R4,HOOK4                                                         
         B     XIT                                                              
         SPACE 1                                                                
HOOK8    MVC   2(12,R3),=C'---HOMES----'                                        
         B     HOOK12                                                           
         SPACE 1                                                                
HOOK10   MVC   2(12,R3),=12C'-'    HOMES + 4 FLAVOR                             
         BAS   RE,HOOKDEM                                                       
         MVC   5(7,R3),WORK                                                     
         CLI   10(R3),C' '                                                      
         BNE   *+8                                                              
         MVI   10(R3),C'-'                                                      
         CLI   11(R3),C' '                                                      
         BNE   *+8                                                              
         MVI   11(R3),C'-'                                                      
         SPACE 1                                                                
HOOK12   MVC   134(12,R3),=C'GRPS   (000)'                                      
         MVC   266(12,R3),=C' CPP     CPM'                                      
         LA    R2,3(R2)                                                         
         LA    R3,15(R3)                                                        
         BCT   R4,HOOK10                                                        
         B     XIT                                                              
         SPACE 1                                                                
HOOK14   MVC   H10+34(12),=C'DATA   HOMES'                                      
         MVC   H11+34(12),=C'----   -----'                                      
         LA    R3,H10+39                                                        
         B     HOOK20                                                           
         SPACE 1                                                                
HOOK16   BAS   RE,HOOKDEM          HOMES + 8 FLAVOR                             
         MVC   2(5,R3),WORK                                                     
         CLC   WORK+5(2),SPACES                                                 
         BE    HOOK18                                                           
         MVC   1(6,R3),WORK                                                     
         CLI   WORK+6,C' '                                                      
         BE    HOOK18                                                           
         MVC   0(7,R3),WORK                                                     
         SPACE 1                                                                
HOOK18   GOTO1 UNDERLIN,DMCB,(7,(R3)),132(R3)                                   
         SPACE 1                                                                
HOOK20   LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,HOOK16                                                        
         B     XIT                                                              
         SPACE 2                                                                
HOOKDEM  NTR1                                                                   
**** TRANSLATES R2 TO FORMAT FOR CALLS TO NEMED00 VERSION OF DEMOCON            
****  THIS IS TO SUPPORT USER DEMOS                                             
****  ON INPUT R2 POINTS TO THE DEMO DESCRIPTOR IN NDDEMOS                      
****  OUTPUT TO NEMED00: R3 IS RELATIVE DEMO NUMBER IN NDDEMOS                  
         LA    R3,NDDEMOS                                                       
         SR    R2,R3               OFFSET FROM NDDEMOS                          
         LR    R1,R2               AND DIVIDE BY 3                              
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LR    R3,R1               PUT BACK IN R3                               
         NETGO NVDEMCON,DMCB,((R3),NDDEMBLK),DBLOCK,(7,WORK)                    
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              DSECT TO COVER MODULE STORAGE                                    
*                                                                               
SUMWS    DSECT                                                                  
*              NETDEMOD HERE                                                    
*              AND DEDBLOCK                                                     
         PRINT OFF                                                              
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
SAVENET  DS    CL4                 SAVED NETWORK NAME                           
NETTOT   DS    CL(LINELEN)         CURRENT NETWORK TOTALS                       
ESTTOT   DS    CL(LINELEN)         ESTIMATE TOTALS                              
* LOCAL W/S                                                                     
NTESD    DS    0H                  START OF ACCUMULATOR LINES                   
*                                                                               
LINELEN  EQU   112                 LENGTH OF EACH LINE                          
NUMLINES EQU   40                  NUMBER OF LINES(NOT INCLUDING TOTALS         
NUMDEMS  EQU   8                   MAX NUM DEMOS PER LINE                       
*                                                                               
*                                                                               
         ORG   NTESD+(NUMLINES*LINELEN)                                         
*                                                                               
ENDLIN   EQU   *            *****  MAKE SURE THIS DOESNT EXCEED MEMORY          
*                                                                               
*                                                                               
ACBLOCK  DSECT                     DSECT FOR 1 LINE                             
*                                                                               
ACLINE   DS    0CL(LINELEN)                                                     
ACNET    DS    CL4                 NETWORK                                      
ACNO     DS    CL1                 PACKAGE NUMBER                               
ACNAME   DS    CL16                PACKAGE NAME                                 
ACDP     DS    CL7                 DAYPART CODE                                 
ACGCPM   DS    CL4                 GUARANTEED CPM                               
*                                                                               
ACACCSTR EQU   *                   BEGINNING OF ACCUMULATED NUMBERS             
ACCOST   DS    F                   PACKAGE COST                                 
ACHOMGRP DS    F                   HOMES GRP                                    
ACHOMIMP DS    F                         IMPS                                   
ACDEMOS  DS    (NUMDEMS*2)F        GRP AND IMPS FOR EACH DEMO                   
ACUNITS  DS    F                   UNITS                                        
ACACCEND EQU   *                                                                
*                                                                               
NUMACCS  EQU   (ACACCEND-ACACCSTR)/4    NUMBER OF FULLWORDS TO ADD              
ACCOFF   EQU   ACACCSTR-ACBLOCK         OFFSET TO 1ST ACCUMULATOR WORD          
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE5D                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040NEMED65   05/01/02'                                      
         END                                                                    
