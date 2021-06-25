*          DATA SET NEMED21    AT LEVEL 014 AS OF 07/22/02                      
*PHASE T31E21A                                                                  
         TITLE 'T31E21 - WEEKLY STEWARDSHIP REPORT'                             
T31E21   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NTST**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2                                                       
         USING WEEKD,R7                                                         
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
*                                                                               
         EJECT                                                                  
*              INITIALIZE NETBLOCK                                              
         SPACE 3                                                                
         MVI   NBHUNOPT,C'Y'       DEALING IN HUNDREDS                          
         CLI   SPLFLAV,C'A'        USE ACTUALS                                  
         BNE   IN6                                                              
         MVI   NBSELUOP,C'A'                                                    
         MVI   NBACTOPT,C'Y'                                                    
         B     IN8                                                              
IN6      MVI   NBESTOPT,C'Y'       USE ESTIMATE DEMOS                           
         MVI   NBSELUOP,C'E'                                                    
         SPACE 1                                                                
IN8      MVI   NBDATA,C'U'         PROCESS UNITS                                
         SPACE 1                                                                
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK   PROCESS DATE                             
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GOTDATE                                                          
         B     PROCDAT                                                          
         SPACE 1                                                                
GOTDATE  MVC   PERTYPE(1),DATOPT   WEEK OR MONTH                                
         MVI   PERTYPE+1,1         SET SO MONS USED IF TOO MANY WKS             
         MVI   PERTYPE+2,0         DONT USE QUARTERS                            
         LA    R4,MAXMONTS                                                      
         ST    R4,NUMMONS          MAX NUMBER OF MONTHS                         
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE  GET LIST                   
         SPACE 1                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK    NOW DO UNIT RECORDS                     
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST     LAST ONE                                     
         BE    LASTONE                                                          
         CLI   NBMODE,NBPROCUN     IF A UNIT                                    
         BE    GOTUNIT                                                          
         B     GETUNIT                                                          
         SPACE 1                                                                
GOTUNIT  BAS   RE,POST                                                          
         B     GETUNIT                                                          
         SPACE 1                                                                
LASTONE  BAS   RE,REPTOTS                                                       
         XIT1                                                                   
         SPACE 1                                                                
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
*              POST INTO ACCUMULATORS                                           
*              PROCESS A UNIT RECORD                                            
         SPACE 3                                                                
POST     NTR1                                                                   
         MVC   DATE,NBACTDAT       SAVE OFF DATE                                
         L     R1,NBACTUAL                                                      
         M     R0,=F'1'            GET ACTUAL DOLLARS (IGNORE CENTS)            
         D     R0,=F'100'                                                       
         ST    R1,DOLLARS                                                       
         SPACE 1                                                                
         CLI   SPLFLAV,C'E'        IF ESTIMATED DEMOS                           
         BNE   USEACTS                                                          
         MVC   UNITS,NBESTUN                                                    
         MVC   GRPS,NDESTDEM+2                                                  
         MVC   IMPS,NDESTDEM+4                                                  
         MVC   HOMES,NBESTHOM+4                                                 
         B     DU2                                                              
         SPACE 1                                                                
USEACTS  MVC   UNITS,NBACTUN       IF ACTUAL DEMOS                              
         MVC   GRPS,NDACTDEM+2                                                  
         MVC   IMPS,NDACTDEM+4                                                  
         MVC   HOMES,NBACTHOM+4                                                 
         SPACE 1                                                                
DU2      LA    R2,MONLIST                                                       
         LA    R3,WGRPS                                                         
         XC    WGRPS,WGRPS                                                      
         L     R5,NUMMONS                                                       
         SPACE 1                                                                
WE14     CLC   DATE,2(R2)          LOCATE WEEK (MONTH)                          
         BNH   WE16                                                             
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,WE14                                                          
         B     XIT                                                              
         SPACE 1                                                                
WE16     LH    R1,GRPS             ADD GRPS TO THIS SLOT                        
         CLC   SPLDEM(4),=C'UNIT'  OR, OPTIONALLY, UNITS                        
         BNE   *+8                                                              
         LH    R1,UNITS                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         XC    NETDISP,NETDISP                                                  
         CLC   NBACTNET(3),=C'ABC'                                              
         BE    WE18                                                             
         MVC   NETDISP,=H'80'                                                   
         CLC   NBACTNET(3),=C'CBS'                                              
         BE    WE18                                                             
         MVC   NETDISP,=H'160'                                                  
         CLC   NBACTNET(3),=C'NBC'                                              
         BE    WE18                                                             
         MVC   NETDISP,=H'240'                                                  
         SPACE 1                                                                
WE18     LA    R2,DPLIST                                                        
         SR    R3,R3                                                            
         SPACE 1                                                                
WE20     STH   R3,DPDISP                                                        
         CLC   NBACTDP,0(R2)                                                    
         BE    WE22                                                             
         CLI   0(R2),X'FF'                                                      
         BE    WE22                                                             
         LA    R2,8(R2)                                                         
         LA    R3,400(R3)                                                       
         B     WE20                                                             
         SPACE 1                                                                
WE22     LH    R2,DPDISP           ADD INTO DAYPART/NETWORK                     
         AH    R2,NETDISP                                                       
         BAS   RE,WE24                                                          
         LH    R2,DPDISP           DAYPART/TOTAL                                
         LA    R2,320(R2)                                                       
         BAS   RE,WE24                                                          
         LA    R2,3600             ALL/NETWORK                                  
         AH    R2,NETDISP                                                       
         BAS   RE,WE24                                                          
         LA    R2,3600             ALL/ALL                                      
         LA    R2,320(R2)                                                       
         BAS   RE,WE24                                                          
         XIT1                                                                   
         SPACE 1                                                                
WE24     NTR1                                                                   
         LA    R2,ACCUMS(R2)       ADD TO ACCUM(R2)                             
         LA    R3,GRPS-2                                                        
         XC    0(2,R3),0(R3)                                                    
         LA    R0,20                                                            
         SPACE 1                                                                
WE26     L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,WE26                                                          
         B     XIT                                                              
         EJECT                                                                  
*              REPORT TOTALS                                                    
         SPACE 3                                                                
REPTOTS  LA    R2,ACCUMS                                                        
         LA    R3,DPLIST                                                        
         LA    R0,11               SET UP TO HANDLE 9 DAYPARTS + TOT            
         SPACE 1                                                                
WE32     MVC   P(7),1(R3)                                                       
         BAS   RE,WE34                                                          
         LA    R2,400(R2)                                                       
         LA    R3,8(R3)                                                         
         BCT   R0,WE32                                                          
         B     XIT                                                              
         SPACE 1                                                                
WE34     NTR1                                                                   
         LA    R3,NETLIST                                                       
         LA    R0,5                HANDLE 3 NETWORKS + OTHERS + TOTALS          
         SPACE 1                                                                
WE36     MVC   P+8(3),0(R3)                                                     
         MVI   SPACING,1                                                        
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         BAS   RE,FORMAT                                                        
         LA    R2,80(R2)                                                        
         LA    R3,3(R3)                                                         
         BCT   R0,WE36                                                          
         XIT1                                                                   
         EJECT                                                                  
*              FORMAT A LINE OF ACCUMULATORS                                    
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         OC    0(80,R2),0(R2)                                                   
         BZ    XIT                                                              
         EDIT  (4,0(R2)),(7,P+11),1     TOTAL POINTS                            
         CLI   P+11,C' '                                                        
         BE    FMA                                                              
         L     R1,0(R2)                                                         
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(7,P+11)                                                    
         SPACE 1                                                                
FMA      L     R1,4(R2)            DOLLARS                                      
         LTR   R1,R1                                                            
         BZ    FMB                                                              
         EDIT  (R1),(9,P+18)                                                    
         L     R3,8(R2)            HOME CPM                                     
         LA    R4,P+28                                                          
         BAS   RE,CPM                                                           
         L     R3,12(R2)           TARGET CPM                                   
         LA    R4,6(R4)                                                         
         BAS   RE,CPM                                                           
         SPACE 1                                                                
FMB      LA    R2,16(R2)           WEEKLY POINTS                                
         LA    R3,P+41                                                          
         L     R4,NUMMONS                                                       
         SPACE 1                                                                
FM2      OC    0(4,R2),0(R2)                                                    
         BZ    FM4                                                              
         EDIT  (4,0(R2)),(4,0(R3))                                              
         CLC   SPLDEM(4),=C'UNIT'                                               
         BE    FM4                                                              
         EDIT  (4,0(R2)),(4,0(R3)),1                                            
         CLC   0(4,R2),=F'1000'              ALWAYS HAVE ROOM FOR 99.9          
         BL    FM4                                                              
         L     R1,0(R2)                                                         
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'           GET POINTS ROUNDED                           
         EDIT  (R1),(4,0(R3))      AND SHOW THESE                               
         C     R1,=F'10000'                                                     
         BL    FM4                                                              
         MVC   0(4,R3),SPACES      STILL NOT ROOM                               
         LR    R6,R3                                                            
         SH    R6,=H'4'                                                         
         CLC   0(8,R6),SPACES                BIG NUMBERS MAY FIT                
         BE    *+8                                                              
         LA    R6,132(R6)                    OR GO ON THE LINE BELOW            
         EDIT  (R1),(8,0(R6))                                                   
         SPACE 1                                                                
FM4      LA    R2,4(R2)                                                         
         LA    R3,5(R3)                                                         
         BCT   R4,FM2                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE CPM                                            
         SPACE 3                                                                
CPM      NTR1                                                                   
         LTR   R3,R3               R3 CONTAINS IMPS                             
         BZ    XIT                                                              
         M     R0,=F'2000'         R1 CONTAINS DOLLARS                          
         DR    R0,R3                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(5,0(R4)),2    TRY AND SHOW TO 2 DEC PLACES                 
         CLI   0(R4),C' '                                                       
         BE    XIT                                                              
         LA    R1,50(R1)           NOT ENOUGH ROOM                              
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(5,0(R4))       SO JUST SHOW DOLLARS                        
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+14(20),SPLPRON                                                
         MVC   H6+14(24),SPLESTN                                                
         CLI   NBSELESE,0                                                       
         BE    HOOK1                                                            
         MVC   H6+10(7),SPLEST                                                  
         MVI   H6+17,C' '                                                       
         MVC   H6+18(24),SPLESTN                                                
         OC    H6+10(32),SPACES                                                 
         GOTO1 SQUASHER,DMCB,H6+10,32                                           
         SPACE 2                                                                
HOOK1    LA    R2,H1+44            TITLE                                        
         MVC   H1+37(6),=C'WEEKLY'                                              
         MVC   H2+37(8),=8C'-'                                                  
         CLI   PERTYPE,C'W'                                                     
         BE    HOOK2                                                            
         MVC   H1+37(7),=C'MONTHLY'                                             
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
HOOK2    MVC   0(18,R2),=C'STEWARDSHIP REPORT'                                  
         MVC   132(18,R2),=18C'-'                                               
         MVC   H5+82(8),SPLDPTN                                                 
         MVC   H6+82(5),=C'UNITS'                                               
         CLC   SPLDEM(4),=C'UNIT'                                               
         BE    HOOK4                                                            
         SPACE 1                                                                
*                                  GET NAME OF 1ST DEMO                         
         NETGO NVDEMCON,DMCB,(0,NDDEMBLK),DBLOCK,(7,WORK)                       
         MVC   H6+82(7),WORK                                                    
         CLI   FLAVOR,C'E'                                                      
         BE    HOOK4                                                            
         MVC   H6+90(9),=CL9'ACTUAL'                                            
         SPACE 1                                                                
HOOK4    LA    R2,MONLIST                                                       
         LA    R3,H10+42                                                        
         L     R4,NUMMONS                                                       
         SPACE 1                                                                
HOOK6    CLI   PERTYPE,C'M'                                                     
         BE    HOOK8                                                            
         GOTO1 DATCON,DMCB,(2,0(R2)),(4,WORK)                                   
         MVC   0(3,R3),WORK                                                     
         MVC   133(2,R3),WORK+3                                                 
         B     HOOK10                                                           
         SPACE 1                                                                
HOOK8    GOTO1 DATCON,DMCB,(2,2(R2)),(6,WORK)                                   
         MVC   0(3,R3),WORK                                                     
         MVC   133(2,R3),WORK+4                                                 
         SPACE 1                                                                
HOOK10   LA    R2,4(R2)                                                         
         LA    R3,5(R3)                                                         
         BCT   R4,HOOK6                                                         
         B     XIT                                                              
         EJECT                                                                  
*              LTORG AND CONSTANTS                                              
         SPACE 3                                                                
DPLIST   DC    CL8'DDAYTIME'                                                    
         DC    CL8'FFRINGE'                                                     
         DC    CL8'PPRIME'                                                      
         DC    CL8'KKIDS'                                                       
         DC    CL8'YYOUTH'                                                      
         DC    CL8'SSPORTS'                                                     
         DC    CL8'NNEWS'                                                       
         DC    CL8'LLATE'                                                       
         DC    CL8'EEARLY'                                                      
         DC    X'FF',CL7'OTHERS'                                                
         DC    X'FF',CL7'*TOTAL*'                                               
         SPACE 1                                                                
NETLIST  DC    C'ABCCBSNBCOTHTOT'                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR WEEKLY STEWARDSHIP                                     
         SPACE 3                                                                
WEEKD    DSECT                        COMMON WITH EDIT                          
         PRINT OFF                                                              
****   ++INCLUDE NETDEMOD                                                       
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
DPFILT   DS    CL1                                                              
FLAVOR   DS    CL1                                                              
DATOPT   DS    CL1                                                              
         SPACE 1                                                                
PERTYPE  DS    CL3                 PERIOD TYPE, CONTROLS                        
MAXMONTS EQU   16                                                               
NUMMONS  DS    F                                                                
MONLIST  DS    CL(4*MAXMONTS)                                                   
DATE     DS    CL2                                                              
NETDISP  DS    H                                                                
DPDISP   DS    H                                                                
UNITS    DS    H                                                                
         DS    0F                                                               
FILLER   DS    H                                                                
GRPS     DS    H                                                                
DOLLARS  DS    F                                                                
HOMES    DS    F                                                                
IMPS     DS    F                                                                
WGRPS    DS    CL64                                                             
ACCUMS   DS    50CL80                                                           
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF1D                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014NEMED21   07/22/02'                                      
         END                                                                    
