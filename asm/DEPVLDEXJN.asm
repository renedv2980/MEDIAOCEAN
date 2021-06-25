*          DATA SET DEPVLDEXJN AT LEVEL 105 AS OF 06/06/02                      
*PHASE DEPVLEJA PVLDEXJN                                                        
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'PVLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
**********************************************************************          
* PVLDEXTJ - DELETES J-PTRS FROM CABLE BOOK WHICH ENABLES A RE-LOAD             
*   OF THE BOOK TO OCCUR. (J-PTRS CORRELATE THE DDS PRG# WITH THE FILE          
*   NUMBERS FOR THE PARTICULAR BOOK.                                            
*                                                                               
*    1. ENTER THE BOOK TO BE DELETED IN THE PJBOOK FIELD                        
*       (EXTERN WILL DELETE ALL JCN KEYS WITH THAT BOOK IN IT)                  
*    2. DUMP DIRECTORY WITHOUT EXTERN.                                          
*    3. LOAD DIRECTORY WITH EXTERN.                                             
*                                                                               
**********************************************************************          
* GENERAL EXTERN PARAMETER LIST                                                 
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
**********************************************************************          
         PRINT NOGEN                                                            
PVLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*PVLDEXT,RR=R5                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
PVXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    PVINIT              INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    PVXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    PVXEOF              END-OF-FILE                                  
         B     PVXIT                                                            
*                                                                               
PVXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     PVXIT                                                            
*                                                                               
PVXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         L     R3,AREC                                                          
         USING PJKEY,R3                                                         
         MVI   0(R1),X'FF'                                                      
         SR    R9,R9                                                            
         ICM   R9,3,PJINTNUM                                                    
         EDIT  (R9),(5,P+11)                                                    
         MVC   DUB+2(5),PJEXTNUM                                                
         MVI   DUB+7,X'0F'                                                      
         UNPK  P(10),DUB+3(5)                                                   
         MVI   P+9,C' '                                                         
         MVC   WORK,SPACES         KEEP RECORD                                  
         MVC   WORK+1(23),0(R3)                                                 
         MVI   WORK,23                                                          
         GOTO1 =V(HEXOUT),DMCB,WORK+11,P+19,23                                  
         GOTO1 VPRINTER                                                         
         DROP  R3                                                               
         BAS   RE,DUMPREC                                                       
         B     PVXIT                                                            
*                                                                               
PVINIT   LA    RE,COUNTS                                                        
         L     RF,=F'15000'                                                     
         XCEF                                                                   
         B     PVXIT                                                            
         SPACE 2                                                                
PVXIT    XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PJKEY,R3                                                         
         CLI   0(R3),X'FF'         ALWAYS PURGE THE EOF RECORDS                 
         BE    PVXPURGE                                                         
         CLC   0(3,R3),=C'JNN'                                                  
         BNE   PVXKEEP                                                          
         CLI   PJSTAT+4,C'N'                                                    
         BNE   PVXKEEP                                                          
         LA    R5,NTINUMT                                                       
PVXRECJ  CLI   0(R5),X'FF'                                                      
         BE    PVXKEEP                                                          
         CLC   PJINTNUM(2),0(R5)                                                
         BNE   *+14                                                             
         CLC   PJEXTNUM(5),2(R5)                                                
         BE    PVXPURGE                                                         
         LA    R5,9(R5)                                                         
         B     PVXRECJ                                                          
*                                                                               
         OC    PJBOOK,PJBOOK       CAUTION: NEVER DELETE BOOK=0 JPTRS!          
         BZ    PVXKEEP                                                          
*                                                                               
         CLC   PJBOOK,=X'6423'     <-- BOOK TO BE DELETED                       
         BE    PVXPURGE                                                         
*                                                                               
         B     PVXKEEP                                                          
**********************************************************************          
PVXEOF   MVC   P(21),=CL21'***RECORDS SAVED***'                                 
         GOTO1 VPRINTER                                                         
         LA    R2,COUNTS                                                        
PVXEOF1  OC    0(5,R2),0(R2)                                                    
         BZ    PVXIT                                                            
         SR    R9,R9                                                            
         ICM   R9,15,5(R2)                                                      
         MVC   P(3),0(R2)                                                       
         EDIT  (R9),(8,P+10)                                                    
         ZIC   R9,3(R2)                                                         
         EDIT  (R9),(2,P+4)                                                     
         ZIC   R9,4(R2)                                                         
         EDIT  (R9),(2,P+6)                                                     
*        GOTO1 VPRINTER                                                         
         LA    R2,9(R2)                                                         
         B     PVXEOF1                                                          
         EJECT                                                                  
* DUMPREC EXPECTS HEADER MESSAGE LENGTH IN WORK AND TEXT AT WORK+1              
*                                                                               
DUMPREC  NTR1                                                                   
         MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
         LH    R5,HALF                                                          
         CLI   HALF,X'FF'          TEST FOR PASSIVE RECORD                      
         BNE   *+8                                                              
         LA    R5,23               LENGTH OF PASSIVE RECORD                     
         ZIC   R2,WORK             HEADER MESSAGE LENGTH                        
         GOTO1 VPRNTBL,DMCB,((R2),WORK+1),(R3),C'DUMP',(R5),=C'2H'              
         XIT1                                                                   
         EJECT                                                                  
*          DATA SET DENTIFIX   AT LEVEL 086 AS OF 05/28/02                      
*                 CURR#      NTILONG#       NEW#                                
NTINUMT  DC    AL2(10769),X'0000127728',AL2(10900)                              
         DC    AL2(10769),X'0000127785',AL2(10901)                              
         DC    AL2(10769),X'0000127806',AL2(10902)                              
         DC    AL2(10769),X'0000127828',AL2(10903)                              
         DC    AL2(10769),X'0000127847',AL2(10904)                              
         DC    AL2(10770),X'0000127729',AL2(10905)                              
         DC    AL2(10770),X'0000127827',AL2(10906)                              
         DC    AL2(10770),X'0000127862',AL2(10907)                              
         DC    AL2(10771),X'0000127808',AL2(10908)                              
         DC    AL2(10771),X'0000127890',AL2(10909)                              
         DC    AL2(10772),X'0000127829',AL2(10910)                              
         DC    AL2(10772),X'0000127841',AL2(10911)                              
         DC    AL2(10772),X'0000127842',AL2(10912)                              
         DC    AL2(10772),X'0000127844',AL2(10913)                              
         DC    AL2(10772),X'0000127911',AL2(10914)                              
         DC    AL2(10773),X'0000127843',AL2(10915)                              
         DC    AL2(10773),X'0000127917',AL2(10916)                              
         DC    AL2(10774),X'0000127872',AL2(10917)                              
         DC    AL2(10774),X'0000127967',AL2(10918)                              
         DC    AL2(10775),X'0000124363',AL2(10919)                              
         DC    AL2(10775),X'0000127819',AL2(10920)                              
         DC    AL2(10775),X'0000127912',AL2(10921)                              
         DC    AL2(10775),X'0000127915',AL2(10922)                              
         DC    AL2(10775),X'0000127918',AL2(10923)                              
         DC    AL2(10775),X'0000127968',AL2(10924)                              
         DC    AL2(10775),X'0000127991',AL2(10925)                              
         DC    AL2(10775),X'0000128141',AL2(10926)                              
         DC    AL2(10776),X'0000127820',AL2(10927)                              
         DC    AL2(10776),X'0000127913',AL2(10928)                              
         DC    AL2(10776),X'0000128083',AL2(10929)                              
         DC    AL2(10776),X'0000128121',AL2(10930)                              
         DC    AL2(10776),X'0000128150',AL2(10931)                              
         DC    AL2(10776),X'0000128205',AL2(10932)                              
         DC    AL2(10778),X'0000127616',AL2(10933)                              
         DC    AL2(10778),X'0000128078',AL2(10934)                              
         DC    AL2(10778),X'0000128079',AL2(10935)                              
         DC    AL2(10778),X'0000128196',AL2(10936)                              
         DC    AL2(10778),X'0000128206',AL2(10937)                              
         DC    AL2(10779),X'0000127907',AL2(10938)                              
         DC    AL2(10779),X'0000127904',AL2(10939)                              
         DC    AL2(10779),X'0000128180',AL2(10940)                              
         DC    AL2(10779),X'0000128197',AL2(10941)                              
         DC    AL2(10780),X'0000127901',AL2(10942)                              
         DC    AL2(10780),X'0000128204',AL2(10943)                              
         DC    AL2(10781),X'0000128143',AL2(10944)                              
         DC    AL2(10781),X'0000128151',AL2(10945)                              
         DC    AL2(10782),X'0000128152',AL2(10946)                              
         DC    AL2(10782),X'0000128291',AL2(10947)                              
         DC    AL2(10783),X'0000128155',AL2(10948)                              
         DC    X'FFFF'                                                          
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
LOGIO    DC    V(LOGIO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
KEEPS    DC    F'0'                                                             
SVKEY    DS    CL18                                                             
HEAD     DC    C'**RECORD**'                                                    
COUNTS   DS    15000C                                                           
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
VPRNTBL  DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105DEPVLDEXJN06/06/02'                                      
         END                                                                    
