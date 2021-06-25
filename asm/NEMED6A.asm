*          DATA SET NEMED6A    AT LEVEL 019 AS OF 05/01/02                      
*PHASE T31E6AA                                                                  
         TITLE 'T31E6A - BRAND ALLOCATION'                                      
**          CLIENT RECORD PASSED FROM EDIT IN W/S AREA 1                        
*                                                                               
T31E6A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NTBR**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2                                                       
         USING PEPRD,R7                                                         
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         EJECT                                                                  
*                                                                               
*   INITIALIZE NETBLOCK                                                         
*                                                                               
         MVI   NBACTOPT,C'Y'       GET ACTUAL DEMOS                             
         MVI   NBDATA,C'U'         GET UNITS                                    
         MVI   NBSELUOP,C'A'       USE ACTUAL SCHEDULE                          
*                                                                               
         XC    PERTYPE,PERTYPE     USE WEEKS FOR PERIOD                         
         MVI   PERTYPE,C'D'        USE DAYS, THEN WEEKS                         
         MVI   PERTYPE+1,1         SET TO USE MONTHS IF > 16 WKS                
         LA    R4,MAXMONTS         SET NUMMONS TO MAX MONTHS IN LIST            
         ST    R4,NUMMONS                                                       
*                                                                               
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK   PROCESS DATE                             
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GOTDATE                                                          
         B     PROCDAT                                                          
*                                                                               
GOTDATE  NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE     FILL LIST               
*                                                                               
FRSTUN   NETGO NSNETIO,DMCB,NETBLOCK    GET FIRST UNIT RECORD                   
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST     LAST ONE                                     
         BE    LASTONE                                                          
         CLI   NBMODE,NBPROCUN     IF A UNIT                                    
         BE    GOTFIRST                                                         
         B     FRSTUN                                                           
*                                                                               
GOTFIRST MVC   SAVEPROG,NBPROGNM   FIRST UNIT                                   
         MVC   SAVEDAY,NBDAYNAM                                                 
         MVC   SAVETIME,NBTIME                                                  
         MVC   SAVELEN,NBLEN                                                    
         BAS   RE,POST                                                          
*                                                                               
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK    NOW DO UNIT RECORDS                     
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST     LAST ONE                                     
         BE    LASTONE                                                          
         CLI   NBMODE,NBPROCUN     IF A UNIT                                    
         BE    CKPROG                                                           
         B     GETUNIT                                                          
*                                                                               
CKPROG   TM    NBSUBMSK,NBSBMPRG   IF A NEW PROGRAM                             
         BZ    GOTUNIT                                                          
         BAS   RE,BUYTOTS                                                       
         MVC   SAVEPROG,NBPROGNM                                                
         MVC   SAVEDAY,NBDAYNAM                                                 
         MVC   SAVETIME,NBTIME                                                  
         MVC   SAVELEN,NBLEN                                                    
GOTUNIT  BAS   RE,POST                                                          
         B     GETUNIT                                                          
*                                                                               
LASTONE  BAS   RE,BUYTOTS          PRINT PROG TOTALS                            
         BAS   RE,PAKTOTS          NOW PACKAGE TOTALS                           
         XIT1                                                                   
*                                                                               
PROCERR  DC    H'0'                                                             
**************************************************************                  
         EJECT                                                                  
*              ROUTINES TO POST                                                 
         SPACE 2                                                                
POST     NTR1                                                                   
         LA    R6,BUYACCUM                                                      
         USING ACCUMD,R6                                                        
         LH    R1,NBACTUN          UNITS                                        
         A     R1,ACUN                                                          
         ST    R1,ACUN                                                          
         LH    R1,NBACTHUT                                                      
         A     R1,ACHUT                                                         
         ST    R1,ACHUT                                                         
         LH    R1,NBACTSHR         SHARE                                        
         A     R1,ACSHR                                                         
         ST    R1,ACSHR                                                         
         L     R1,NBASSIGN         CASH                                         
         A     R1,ACDOL                                                         
         ST    R1,ACDOL                                                         
*                                                                               
         LA    R2,NBACTHOM                                                      
         LA    R4,ACHOMVPH                                                      
*                                                                               
         LH    R1,0(R2)            HOMES VPH                                    
         A     R1,0(R4)                                                         
         ST    R1,0(R4)                                                         
         LH    R1,2(R2)            HOME POINTS                                  
         A     R1,4(R4)                                                         
         ST    R1,4(R4)                                                         
         L     R1,4(R2)            HOME IMPS                                    
         A     R1,8(R4)                                                         
         ST    R1,8(R4)                                                         
*                                                                               
         LA    R2,NDACTDEM         DO REST OF DEMOS                             
         ZIC   R3,NDNDEMOS                                                      
         LTR   R3,R3                                                            
         BZ    POST3                                                            
         LA    R4,12(R4)                                                        
         SPACE 2                                                                
POST2    LH    R1,0(R2)            VPH                                          
         A     R1,0(R4)                                                         
         ST    R1,0(R4)                                                         
         LH    R1,2(R2)            POINTS                                       
         A     R1,4(R4)                                                         
         ST    R1,4(R4)                                                         
         L     R1,4(R2)            IMPS                                         
         A     R1,8(R4)                                                         
         ST    R1,8(R4)                                                         
         LA    R2,8(R2)                                                         
         LA    R4,12(R4)                                                        
         BCT   R3,POST2                                                         
*                                                                               
POST3    SR    R1,R1                                                            
         LA    R2,MONLIST          GET WEEK NUMBER                              
PLOOP    LA    R1,1(R1)                                                         
         CLC   NBACTDAT(2),2(R2)                                                
         BNH   POST4                                                            
         LA    R2,4(R2)                                                         
         B     PLOOP                                                            
*                                                                               
POST4    STC   R1,SAVEWEEK         SAVE IT FOR LATER                            
         BCTR  R1,0                                                             
         MH    R1,=H'20'                                                        
         LA    R6,BUYWKACC(R1)                                                  
         USING WEEKD,R6                                                         
         LM    R1,R5,ACWEKHOM                                                   
         AH    R1,NBACTHOM+2       HOMES GRP                                    
         AH    R2,NDACTDEM+2       DEMO  GRP                                    
         AH    R3,NBACTUN                                                       
         AH    R4,NBACTHUT                                                      
         A     R5,NBACTUAL                                                      
         STM   R1,R5,ACWEKHOM                                                   
         EJECT                                                                  
*              ROUTINE TO POST BRAND INTO GRIDS                                 
         SPACE 3                                                                
         BAS   RE,PRLUP                                                         
         B     POST8                                                            
         SPACE 2                                                                
PRLUP    NTR1                                                                   
         MVC   WORK,SPACES                                                      
         LA    R4,NBPRD                                                         
         LA    R5,WORK                                                          
         BAS   RE,PRLUP2                                                        
         LA    R4,NBPRD2                                                        
         LA    R5,WORK+3                                                        
         BAS   RE,PRLUP2                                                        
         B     XIT                                                              
         SPACE 2                                                                
PRLUP2   L     R2,ANETWS1          A(CLIENT RECORD)                             
         USING CLTHDR,R2                                                        
         LA    R2,CLIST            A(PRODUCT LIST)                              
         DROP  R2                                                               
*                                                                               
         LA    R3,220                                                           
         CLI   0(R4),0                                                          
         BER   RE                                                               
         CLI   0(R4),X'FF'                                                      
         BER   RE                                                               
         SPACE 2                                                                
PRLUP4   CLC   0(1,R4),3(R2)                                                    
         BE    PRLUP6                                                           
         LA    R2,4(R2)                                                         
         BCT   R3,PRLUP4                                                        
         BR    RE                                                               
         SPACE 2                                                                
PRLUP6   MVC   0(3,R5),0(R2)                                                    
         BR    RE                                                               
         SPACE 2                                                                
POST8    ZIC   R2,SAVEWEEK         POSITION TO COLUMN IN GRIDS                  
         BCTR  R2,0                                                             
         SLL   R2,2                                                             
         LA    R2,GRIDS(R2)                                                     
         LA    R3,29                                                            
         LA    R4,1                                                             
         SPACE 2                                                                
POST10   OC    0(4,R2),0(R2)                                                    
         BZ    POST12                                                           
         LA    R2,64(R2)                                                        
         LA    R4,1(R4)                                                         
         BCT   R3,POST10                                                        
         B     XIT                                                              
         SPACE 2                                                                
POST12   MVC   0(3,R2),=C'UNA'                                                  
         CLC   WORK(3),SPACES                                                   
         BE    POST14                                                           
         MVC   0(3,R2),WORK                                                     
         CLC   WORK+3(3),SPACES                                                 
         BE    POST14                                                           
         MVC   64(3,R2),WORK+3                                                  
         LA    R4,1(R4)                                                         
         SPACE 2                                                                
POST14   ZIC   R3,ALLOWLIN                                                      
         CR    R4,R3                                                            
         BL    XIT                                                              
         STC   R4,ALLOWLIN                                                      
         B     XIT                                                              
         EJECT                                                                  
*              BUY TOTALS                                                       
         SPACE 3                                                                
BUYTOTS  NTR1                                                                   
         LA    R6,BUYACCUM                                                      
         USING ACCUMD,R6                                                        
         OC    ACUN,ACUN           DONT PRINT IF NO UNITS                       
         BZ    XITBUYT                                                          
         MVC   P+4(16),SAVEPROG                                                 
         EDIT  (1,SAVELEN),(3,P+20)                                             
         L     R1,ACSHR            SHARE                                        
         SR    R0,R0                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'5'                                                         
         EDIT  (R1),(3,P+25)                                                    
         MVI   P+27,C' '                                                        
         L     R0,ACHOMRTG         AVE RTG                                      
         SRDA  R0,31                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(4,P+29),1                                                  
         MVC   P+34(3),SAVEDAY                                                  
         GOTO1 UNTIME,DMCB,SAVETIME,P+38                                        
         EDIT  (4,ACUN),(3,P+49)                                                
         LA    R2,GRIDS                                                         
         ZIC   R3,ALLOWLIN                                                      
         CLI   SPLSPAC,C'Y'                                                     
         BNE   BUYTOTS2                                                         
         ZIC   R1,ALLOWLIN                                                      
         SLL   R1,1                                                             
         CH    R1,=H'40'                                                        
         BL    *+8                                                              
         LA    R1,40                                                            
         STC   R1,ALLOWLIN                                                      
         SPACE 2                                                                
BUYTOTS2 MVC   P+55(63),0(R2)                                                   
         OC    P,SPACES                                                         
         CLI   SPLSPAC,C'2'                                                     
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         CLI   SPLSPAC,C'3'                                                     
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPLSPAC,C'4'                                                     
         BNE   *+8                                                              
         MVI   SPACING,4                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    0(64,R2),0(R2)                                                   
         LA    R2,64(R2)                                                        
         BCT   R3,BUYTOTS2                                                      
         CLI   SPLSKIP,C'Y'                                                     
         BNE   *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     BUYTOTS4                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 2                                                                
BUYTOTS4 LA    R4,130                                                           
         BAS   RE,ROLL                                                          
XITBUYT  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD AND CLEAR ACCUMULATORS                            
         SPACE 3                                                                
ROLL     NTR1                                                                   
         LA    R2,BUYACCUM                                                      
         LA    R3,PAKACCUM                                                      
         SR    R0,R0                                                            
         SPACE 2                                                                
ROLL2    L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R0,0(R2)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,ROLL2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              PACKAGE TOTALS                                                   
         SPACE 3                                                                
PAKTOTS  NTR1                                                                   
         MVC   P+4(14),=C'PACKAGE TOTALS'                                       
         LA    R6,PAKACCUM                                                      
         USING ACCUMD,R6                                                        
         EDIT  (4,ACUN),(4,P+48)                                                
         LA    R6,PAKWKACC                                                      
         USING WEEKD,R6                                                         
         LA    R2,P+53                                                          
         L     R3,NUMMONS                                                       
         SPACE 2                                                                
PAKTOTS2 EDIT  (4,ACWEKUN),(4,0(R2)),ZERO=BLANK                                 
         LA    R6,20(R6)                                                        
         LA    R2,4(R2)                                                         
         BCT   R3,PAKTOTS2                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
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
         MVC   H5+75(09),=C'NETWORK -'                                          
         MVC   H5+85(4),SPLNET                                                  
         MVC   H6+75(33),SPLPAKN                                                
         LA    R2,MONLIST                                                       
         L     R3,NUMMONS                                                       
         LA    R4,H10+55                                                        
*                                                                               
HOOK2    GOTO1 DATCON,DMCB,(2,(R2)),(8,WORK)                                    
         MVC   0(3,R4),WORK                                                     
         MVC   132(2,R4),WORK+3                                                 
         LA    R2,4(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,HOOK2                                                         
*                                                                               
XIT      XIT1                                                                   
*        SPACE 2                                                                
         EJECT                                                                  
*              WORK SPACE ETC.                                                  
*        ******** COMMON WITH EDIT                                              
PEPRD    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
         EJECT                                                                  
*                                                                               
**** LOCAL W/S                                                                  
*                                                                               
PERTYPE  DS    CL3                 FIRST BYTE IS PERIOD TYPE                    
MAXMONTS EQU   16                                                               
MONLIST  DS    CL(4*MAXMONTS)      MONTH (WEEK) LIST                            
NUMMONS  DS    F                   NUMBER OF DATE-SETS IN LIST                  
SAVEWEEK DS    CL1                                                              
SAVEPROG DS    CL16                                                             
SAVEDAY  DS    CL3                                                              
SAVETIME DS    CL4                                                              
SAVELEN  DS    CL1                                                              
*                                                                               
BUYACCUM DS    50F                                                              
BUYWKACC DS    80F                                                              
PAKACCUM DS    50F                                                              
PAKWKACC DS    80F                                                              
GRIDS    DS    30CL64                                                           
         SPACE 3                                                                
*              DSECTS TO COVER ACCUMULATORS                                     
         SPACE 3                                                                
ACCUMD   DSECT                                                                  
ACUN     DS    F                                                                
ACHUT    DS    F                                                                
ACSHR    DS    F                                                                
ACDOL    DS    F                                                                
ACHOMVPH DS    F                                                                
ACHOMRTG DS    F                                                                
ACHOMIMP DS    F                                                                
ACDEMVPH DS    F                                                                
ACDEMRTG DS    F                                                                
ACDEMIMP DS    F                                                                
WEEKD    DSECT                                                                  
ACWEKHOM DS    F                                                                
ACWEKDEM DS    F                                                                
ACWEKUN  DS    F                                                                
ACWEKHUT DS    F                                                                
ACWEKDOL DS    F                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDEAD                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019NEMED6A   05/01/02'                                      
         END                                                                    
