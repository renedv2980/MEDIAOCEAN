*          DATA SET DDTESTSMF  AT LEVEL 005 AS OF 07/16/10                      
*PHASE TESTSMFA                                                                 
*INCLUDE SMFOUT                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*                                                                               
TESTSMF  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,TEST,WORK=A(WORK)                                              
         SAM24                                                                  
*                                                                               
LOOP     GOTO1 =V(CARDS),PLIST,(0,PDATA),(0,=C'RE00')                           
         CLC   PDATA(2),=C'/*'                                                  
         BE    MAIN                                                             
         CLC   PDATA(4),=C'SMF='                                                
         BNE   ERROR               ONLY CARD FOR NOW                            
         CLI   PDATA+4,C'Y'        IF Y THEN OUTPUT TO SMF                      
         BNE   PRNT                                                             
         MVI   SMFIND,0            OUTPUT TO SMF, DEFAULT IS TO PRINT           
         B     PRNT                                                             
*                                                                               
ERROR    MVC   PDATA+60(11),=CL11'** ERROR **'                                  
*                                                                               
PRNT     GOTO1 =V(PRINT),DMCB,(0,PLINE),(0,=C'BL01')                            
         B     LOOP                                                             
*                                                                               
SMFEXIT  XBASE                                                                  
***********************************************************************         
* PROCESS EACH SMF RECORD TYPE IN SMFTAB                              *         
* SMF=Y IF YOU WANT TO ACTUALLY WRITE OUT THE RECORDS TO SMF          *         
* ELSE IT WILL JUST PRINT THE OUTPUT                                  *         
***********************************************************************         
         USING SMFTD,R7                                                         
MAIN     DS    0H                  SET X'80' BIT ON IN P1 FOR TEST ONLY         
         LA    R0,SMFTABQ          NUMBER OF ENTRIES TO TEST                    
         LA    R7,SMFTAB           A(TABLE)                                     
SMF10    XR    R3,R3                                                            
         ICM   R3,1,SMFTYPE        SMF TYPE                                     
         BZ    SMFEXIT                                                          
         XR    R4,R4                                                            
         ICM   R4,7,SMFDATA        SMF DATA TO SEND OUT                         
         GOTO1 =V(SMFOUT),SMFPL,(SMFIND,(R3)),(R4)                              
         BCTR  R3,0                INDEX INTO MESSAGE                           
         MHI   R3,L'SMFMSG                                                      
         LA    R8,SMFMSG(R3)       R8 - POINT TO TEXT                           
         BAS   R9,PRTIT                                                         
         AHI   R7,SMFLNQ           NEXT ENTRY                                   
         BCT   R0,SMF10                                                         
*                                                                               
         B     SMFEXIT                                                          
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* DIG OUT THE BUILT RECORD AND PRINT IT                                         
***********************************************************************         
PRTIT    STM   RE,RD,SVREGS                                                     
         L     R5,72(RD)           GET A(SMFREC) IN SMFOUT'S W/S                
         L     R0,=A(SMFREC)                                                    
         LH    R1,0(R5)            GET L'SMFREC                                 
         CHI   R1,4096                                                          
         BNH   *+8                                                              
         LHI   R1,4096                                                          
         LR    RE,R5                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY TO MY W/S AREA                          
         L     R5,=A(SMFREC)                                                    
         LH    R6,0(R5)            GET L'SMFREC                                 
         CHI   R6,4096                                                          
         BNH   *+8                                                              
         LHI   R6,4096                                                          
         GOTO1 =V(HEXOUT),PLIST,SMFPL+3,29(R8),1,=C'SEP'                        
         GOTO1 =V(PRNTBL),PLIST,(32,(R8)),(R5),C'DUMP',(R6),=C'2D'              
         LM    RE,RD,SVREGS                                                     
         BR    R9                                                               
         EJECT ,                                                                
***********************************************************************         
* VARIABLES AND CONSTANTS                                                       
***********************************************************************         
         LTORG                                                                  
*                                                                               
SPACES   DC    CL140' '                                                         
*                                                                               
         DS    0D                                                               
         DC    CL8'*SMFPL**'                                                    
SMFPL    DS    D                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
SVREGS   DS    16A                 SAVE REGS                                    
*                                                                               
PLINE    DC    XL1'00'                                                          
PDATA    DC    CL132' '                                                         
         DC    CL20' '                                                          
*                                                                               
SMFIND   DC    X'80'               PRINT ONLY                                   
         EJECT ,                                                                
***********************************************************************         
* SMF RECORDS TO OUTPUT                                                         
***********************************************************************         
REQCARD  DC    CL80'REQUEST CARD',C'//'                                         
QDQ      DC    AL2(036),CL34'QDQ DATA',C'//'                                    
SOONS    DC    AL2(010),CL10'SOON START',C'//'                                  
SOONE    DC    AL2(004),XL04'00',C'//'                                          
DARE     DC    AL2(020),CL20'HE WHO DARES WINS   ',C'//'                        
FILOLD   DC    AL2(090),CL88'BALANCE THE OLD     ',C'//'                        
UTILITY  DC    AL2(032),CL30'UTILITY RECORD      ',C'//'                        
RUNNER   DC    AL2(074),CL72'RUNNER STATISTICS   ',C'//'                        
FILBAL   DC    AL2(090),CL88'BALANCE THE BOOKS   ',C'//'                        
*                                                                               
NEWACC   DC    A(NASTATS),A(NACARDS),A(NAREMOTE),A(NAARC)                       
NASTATS  DC    AL2(047),CL45'NEWACC STATS        ',C'//'                        
NACARDS  DC    AL2(2+(11*80))                                                   
         DC    CL80'REQUEST CARD#1               '                              
         DC    CL80'REQUEST CARD#2               '                              
         DC    CL80'REQUEST CARD#3               '                              
         DC    CL80'REQUEST CARD#4               '                              
         DC    CL80'REQUEST CARD#5               '                              
         DC    CL80'REQUEST CARD#6               '                              
         DC    CL80'REQUEST CARD#7               '                              
         DC    CL80'REQUEST CARD#8               '                              
         DC    CL80'REQUEST CARD#9               '                              
         DC    CL80'REQUEST CARD#10              '                              
         DC    CL80'REQUEST CARD#11              ',C'//'                        
NAREMOTE DC    CL80'DIRECT=CARD                  ',C'//'                        
NAARC    DC    CL72'ARC=CARD                     ',C'//'                        
         EJECT ,                                                                
***********************************************************************         
* SMF TYPES TO CALL                                                             
***********************************************************************         
SMFTAB   DS    0F                                                               
         DC    AL1(01),AL3(MASTC)    TYPE 01 SVC 247-6                          
         DC    AL1(02),AL3(REQCARD)  TYPE 02 SVC 247-10                         
         DC    AL1(03),AL3(QDQ)      TYPE 03 SVC 247-13                         
         DC    AL1(04),AL3(SOONS)    TYPE 04 SVC 247-14                         
         DC    AL1(05),AL3(SOONE)    TYPE 05 SVC 247-15                         
         DC    AL1(06),AL3(DARE)     TYPE 06 SVC 247-22                         
         DC    AL1(07),AL3(FILOLD)   TYPE 07 SVC 247-23                         
         DC    AL1(08),AL3(UTILITY)  TYPE 08 SVC 247-20                         
         DC    AL1(09),AL3(NEWACC)   TYPE 09 SVC 247-16                         
         DC    AL1(10),AL3(RUNNER)   TYPE 10 SVC 247-31                         
         DC    AL1(11),AL3(FILBAL)   TYPE 11                                    
         DC    AL1(12),AL3(FILBAL)   TYPE 12                                    
SMFTABQ  EQU   (*-SMFTAB)/SMFLNQ                                                
         DC    AL1(0)                                                           
*                                                                               
SMFMSG   DS    0CL32                                                            
         DC    CL32'  T#01  REQ ACCOUNTING    RC=00'                            
         DC    CL32'  T#02  REQ CARD          RC=00'                            
         DC    CL32'  T#03  QDQ EVENT         RC=00'                            
         DC    CL32'  T#04  SOON START        RC=00'                            
         DC    CL32'  T#05  SOON END          RC=00'                            
         DC    CL32'  T#06  DARE EVENT        RC=00'                            
         DC    CL32'  T#07  OLD FILE BALANCE  RC=00'                            
         DC    CL32'  T#08  UTILITY/UNIVERSAL RC=00'                            
         DC    CL32'  T#09  NEW REQ ACC COMBO RC=00'                            
         DC    CL32'  T#10  RUNNER STATISTICS RC=00'                            
         DC    CL32'  T#11  FILE BALANCING 1  RC=00'                            
         DC    CL32'  T#12  FILE BALANCING 2  RC=00'                            
         EJECT ,                                                                
***********************************************************************         
* AREA TO BUILD SMF RECORD                                                      
***********************************************************************         
         DS    0D                                                               
         DC    CL8'*SMFREC*'                                                    
SMFREC   DC    4096C' '                                                         
         DS    0D                                                               
*                                                                               
         DC    C'WKWKWKWK'                                                      
WORK     DC    8000D'0'                                                         
         DC    C'WKWKWKWK'                                                      
*                                                                               
MASTC    CSECT                                                                  
       ++INCLUDE DDMASTC                                                        
         EJECT ,                                                                
***********************************************************************         
* SMF DSECT TO COVER SMF TYPE TABLE                                             
***********************************************************************         
SMFTD    DSECT                                                                  
SMFTYPE  DS    AL1                 SMF TYPE NUMBER                              
SMFDATA  DS    AL3                 A(RECORD TO OUTPUT)                          
SMFLNQ   EQU   *-SMFTD                                                          
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DDTESTSMF 07/16/10'                                      
         END                                                                    
