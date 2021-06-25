*          DATA SET APGHFIHK6  AT LEVEL 011 AS OF 05/08/02                      
*PHASE ACHFHK6A                                                                 
         TITLE 'P&&L WITH 14,15 AND 16 OFFICE REPLACEMENT'                      
ACHFIHK6 CSECT                                                                  
         PRINT NOGEN                                                            
         USING MAND,RA                                                          
         USING ACWORKD,RC                                                       
         NMOD1 0,**ACHK**,R8,RR=R5                                              
         L     RA,0(,R1)                                                        
         L     RC,HOOKAWRK                                                      
         ST    R5,HKRELO                                                        
         L     RF,=A(HOOKIO)                                                    
         AR    RF,R5                                                            
         ST    RF,AHOOKIO                                                       
                                                                                
*---------------------------------------------------------------------*         
*        REPLACE OFFICE IN ROW WITH 14,15 OR 16 OFFICE AND            *         
*        GET THE CORRISPONDING OFFICE NAME                            *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING HOOK1D,R7                                                        
HOOKSORT DS    0H                                                               
         L     R7,HOOKAREC         A(SORT RECORD)                               
         LA    R5,H1CODE1          REPLACE ROW  1                               
         LA    R6,H1NAME1                                                       
         CLI   HOOKNUM,1           MODE=PUTHOOK 1                               
         BE    HK010                                                            
                                                                                
         LA    R5,H1CODE2          REPLACE ROW  2                               
         LA    R6,H1NAME2                                                       
         CLI   HOOKNUM,2           MODE=PUTHOOK 2                               
         BE    HK010                                                            
                                                                                
         LA    R5,H1CODE3          REPLACE ROW  3                               
         LA    R6,H1NAME3                                                       
         CLI   HOOKNUM,3           MODE=PUTHOOK 3                               
         BE    HK010                                                            
                                                                                
         LA    R5,H1CODE4          REPLACE ROW  4                               
         LA    R6,H1NAME4                                                       
         CLI   HOOKNUM,4           MODE=PUTHOOK 4                               
         BNE   HK200                                                            
                                                                                
HK010    CLI   ONEXONLY,C'Y'                                                    
         BE    HK100                                                            
         MVI   ONEXONLY,C'Y'                                                    
                                                                                
         USING ACMD,R1                                                          
         L     R1,AMONACC          GET ADDRESS OF OFFICE NAMES                  
         MVC   AOFFTAB,ACMAOFNB    START  OF TABLE                              
         MVC   OFFICE#,ACMNOFNB    NUMBER OF ENTRIES                            
         DROP  R1                                                               
                                                                                
HK100    CLI   QOPT6,C'1'             DUMP RECORD BEFORE (MODE 1)               
         BNE   HK110                                                            
         LA    R0,H1LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC1B'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
                                                                                
HK110    DS    0H                                                               
         CLC   CURRCON+1(2),=C'13'                                              
         BE    HK190                                                            
         MVC   0(2,R5),CURRCON+4        REPLACE                                 
         CLC   CURRCON+1(2),=C'16'                                              
         BNE   *+10                                                             
         MVC   0(2,R5),CURRCON+3        REPLACE                                 
                                                                                
         USING OFFD,R3                                                          
HK170    DS    0H                                                               
         L     R3,AOFFTAB          START OF TABLE                               
         L     R0,OFFICE#          NUMBER OF OFFICES IN TABLE                   
                                                                                
HK172    CLC   OFFCODE,0(R5)       MATCH OFFICE                                 
         BE    HK175                                                            
         LA    R3,OFFQ(,R3)                                                     
         BCT   R0,HK172                                                         
         MVC   0(36,R6),=CL36'*** UNKNOWN OFFICE NAME ***'                      
         B     HK190                                                            
                                                                                
HK175    MVC   0(36,R6),OFFNAME                                                 
         DROP  R3                                                               
                                                                                
HK190    CLI   QOPT7,C'1'             DUMP RECORD AFTER (MODE 1)                
         BNE   XIT                                                              
         LA    R0,H1LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC1A'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PROCESS SORT RECORD                                          *         
*---------------------------------------------------------------------*         
HK200    DS    0H                                                               
         B     XIT                                                              
                                                                                
XIT      SR    RC,RC                    KEEP RECORD                             
XITNO    LTR   RC,RC                    DROP RECORD                             
         XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        DEFINE CONSTANTS                                                       
*--------------------------------------------------------------------*          
         DS    0F                                                               
AHOOKIO  DC    A(HOOKIO)                                                        
AOFFTAB  DC    A(0)                                                             
HKRELO   DS    F                                                                
HOOKSW   DC    C'N'                     BEEN HERE BEFORE?                       
ONEXONLY DC    C'N'                                                             
OFFICE#  DC    F'0'                                                             
IOKEY    DS    CL49                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
         DC    C'**AIO1**'                                                      
         DS    0F                                                               
HOOKIO   DS    CL2000                                                           
                                                                                
         DC    C'**OFTB**'                                                      
         DS    0F                                                               
OFFTAB   DS    300CL(OFFQ)                                                      
         EJECT                                                                  
OFFD     DSECT                                                                  
OFFCODE  DS    CL2                                                              
OFFNAME  DS    CL36                                                             
OFFQ     EQU   *-OFFD                                                           
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        MODE PUTHOOK RECORD (1)                                                
*-------------------------------------------------------------------*           
HOOK1D   DSECT                                                                  
H1HEADER DS    0XL18                                                            
H1REPNO  DS    XL1                                                              
H1REPCPY DS    XL1                                                              
         DS    XL16                                                             
H1ROW1   DS    XL2                                                              
H1CODE1  DS    XL14                                                             
H1ROW2   DS    XL2                                                              
H1CODE2  DS    XL14                                                             
H1ROW3   DS    XL2                                                              
H1CODE3  DS    XL14                                                             
H1ROW4   DS    XL2                                                              
H1CODE4  DS    XL14                                                             
*                                                                               
HZEROS   DS    CL96                                                             
*                                                                               
H1COL1   DS    PL8                                                              
H1COL2   DS    PL8                                                              
H1COL3   DS    PL8                                                              
H1COL4   DS    PL8                                                              
H1COL5   DS    PL8                                                              
H1COL6   DS    PL8                                                              
H1COL7   DS    PL8                                                              
H1COL8   DS    PL8                                                              
H1COL9   DS    PL8                                                              
H1COL10  DS    PL8                                                              
H1COL11  DS    PL8                                                              
H1COL12  DS    PL8                                                              
H1COL13  DS    PL8                                                              
H1COL14  DS    PL8                                                              
*                                                                               
HZEROS2  DS    XL144                                                            
*                                                                               
H1NAME1  DS    CL36                                                             
H1NAME2  DS    CL36                                                             
H1NAME3  DS    CL36                                                             
H1NAME4  DS    CL36                                                             
*                                                                               
H1LEN    EQU   *-HOOK1D                                                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        MODE SORTHOOK RECORD (2)                                               
*-------------------------------------------------------------------*           
HOOK2D   DSECT                                                                  
H2ROW1   DS    XL2                                                              
H2CODE1  DS    XL14                                                             
H2ROW2   DS    XL2                                                              
H2CODE2  DS    XL14                                                             
H2ROW3   DS    XL2                                                              
H2CODE3  DS    XL14                                                             
H2ROW4   DS    XL2                                                              
H2CODE4  DS    XL14                                                             
H2REPNO  DS    XL1                                                              
H2REPCP  DS    XL1                                                              
H2TYPE   DS    XL2                                                              
*                                                                               
H2NAME1  DS    CL36                                                             
H2NAME2  DS    CL36                                                             
H2NAME3  DS    CL36                                                             
H2NAME4  DS    CL36                                                             
*                                                                               
H2COL1   DS    PL8                                                              
H2COL2   DS    PL8                                                              
H2COL3   DS    PL8                                                              
H2COL4   DS    PL8                                                              
H2COL5   DS    PL8                                                              
H2COL6   DS    PL8                                                              
H2COL7   DS    PL8                                                              
H2COL8   DS    PL8                                                              
H2COL9   DS    PL8                                                              
H2COL10  DS    PL8                                                              
H2COL11  DS    PL8                                                              
H2COL12  DS    PL8                                                              
H2COL13  DS    PL8                                                              
H2COL14  DS    PL8                                                              
H2LEN    EQU   *-HOOK2D                                                         
         EJECT                                                                  
                                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011APGHFIHK6 05/08/02'                                      
         END                                                                    
