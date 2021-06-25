*          DATA SET DDSTATLINE AT LEVEL 010 AS OF 05/01/02                      
*PHASE STATLINE,*,NOAUTO                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE FATAB                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFFZZWZ                                                               
         TITLE 'STATLINE - PRINT ADRFILE STATISTICS, LINE ORDER'                
STATLINE CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**STATLI,=V(REGSAVE)                                           
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*&&DO*&& OPEN  ADRIN                                                            
*&&OS*&& OPEN  (ADRIN,INPUT)                                                    
*                                                                               
         MVC   MID1(84),INHEAD                                                  
         MVC   TITLE(29),=C'STATPLOT - INPUT TAPE LISTING'                      
         B     INPUT0                                                           
*                                                                               
INHEAD   DC    C'  LINE TRM  SYSTEM   PROGRAM  TASK    SIN     START   X        
                 END     DURTN  CPU TM    I/O'                                  
*                                                                               
*  USE THESE COLUMN HEADINGS WHEN ALL COLUMNS ARE SHOWN. (LENGTH OF             
*  INHEAD WILL BE 125 INSTEAD OF 84)                                            
*                                                                               
*                END     DURTN  CPU TM    I/O  STTM(TU)  NDTM(TU)  STTMX        
*              STTM  NDTMSTTM '                                                 
         EJECT                                                                  
**** SORT ON LINE ID AND START TIME ****                                        
*                                                                               
INPUT0   GOTO1 =V(SORTER),P1,SORTCARD,RECCARD                                   
INPUT    GET   ADRIN,REC                                                        
*                                   ONLY TAKE RECORD IF IT IS                   
         CLC   REC+34(2),=H'512'    MORE THAN 512 I/O'S                         
         BNL   INPUTA                                                           
         CLC   REC+24(4),=F'600'    OR 2 SEC CPU TM                             
         BL    INPUT                                                            
INPUTA   GOTO1 =V(SORTER),P1,=C'PUT',REC                                        
         B     INPUT                                                            
         SPACE 3                                                                
INPUT1   GOTO1 =V(SORTER),P1,=C'GET'                                            
         L     R3,P2                                                            
         LTR   R3,R3                                                            
         BZ    ENDIN1                                                           
*                                                                               
         USING ADRRECD,R3                                                       
*                                                                               
         OC    ADRREC(38),ADRREC   IGNORE NULL RECORDS                          
         BZ    INPUT1                                                           
         EJECT                                                                  
* ROUTINES TO LIST INPUT                                                        
*                                                                               
INPRT    DS    0H                                                               
         MVC   PLN,ADRLINE                                                      
         MVC   PTRM,ADRADDR                                                     
*                                                                               
         L     R5,=V(SELIST)                                                    
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
*                                                                               
         CLC   SESYS,ADRSYSNO                                                   
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
         MVC   PSYS,SENAME                                                      
*                                                                               
         L     R5,SEPGMS                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
*                                                                               
         CLC   PGMNUM,ADRPRGNO                                                  
         BE    *+12                                                             
         BXLE R5,R6,*-10                                                        
         B     *+10                                                             
         MVC   PPRG,PGMNAME                                                     
*                                                                               
         MVC   PTSK,ADRTASK                                                     
*                                                                               
         L     R0,ADRSIN                                                        
         EDIT  (R0),(6,PSIN)                                                    
*                                                                               
         L     R0,ADRSTTM                                                       
         BAS   RE,CVTIME                                                        
         MVC   PSTTM,WORK+2                                                     
*                                                                               
         L     R0,ADRNDTM                                                       
         BAS   RE,CVTIME                                                        
         MVC   PNDTM,WORK+2                                                     
*                                                                               
         L     R0,ADRNDTM                                                       
         S     R0,ADRSTTM                                                       
         BAS   RE,CVDUR                                                         
         MVC   PDUR,WORK+4                                                      
*                                                                               
         L     R0,ADRCPUTM                                                      
         BAS   RE,CVDUR                                                         
         MVC   PCPU,WORK+5                                                      
*                                                                               
         LH    R0,ADRIOCNT                                                      
         EDIT  (R0),(5,PIO)                                                     
*                                                                               
*     DON'T NEED THESE LAST FOUR COLUMNS FOR NOW                                
*                                                                               
*        GOTO1 =V(HEXOUT),P1,ADRSTTM,PSTTU,4,=C'MIX'                            
*                                                                               
*        GOTO1 (RF),(R1),ADRNDTM,PNDTU,4                                        
*                                                                               
*        CLC   LINSTOR,ADRLINE                                                  
*        BE    INPRT1                                                           
*        LA    R1,0                                                             
*        LA    R2,0                                                             
*        B     INPRT2                                                           
*NPRT1   L     R1,ADRSTTM          (INPRT1)                                     
*        S     R1,TIMSTOR1                                                      
*        L     R2,ADRSTTM                                                       
*        S     R2,TIMSTOR2                                                      
*NPRT2   EDIT  (R1),(8,PDIFF1)     (INPRT2)                                     
*        EDIT  (R2),(8,PDIFF2)                                                  
*        MVC   TIMSTOR1,ADRSTTM                                                 
*        MVC   TIMSTOR2,ADRNDTM                                                 
*        MVC   LINSTOR,ADRLINE                                                  
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     INPUT2                                                           
         EJECT                                                                  
* CONVERT DURATION IN HEX TU TO DEC MS                                          
*                                                                               
CVDUR    LA    R1,1000             X 1000 = MS                                  
         MR    R0,R0                                                            
*&&DO*&& D     R0,=F'300'          GIVES MILLI-SECONDS                          
*&&OS*&& D     R0,=F'100'                                                       
         CVD   R1,DUB                                                           
         MVC   WORK(10),=X'40202020202020202120'                                
         ED    WORK(10),DUB+3                                                   
         BR    RE                                                               
         SPACE 2                                                                
* CONVERT TIME IN HEX TU TO HH.MM.SS                                            
CVTIME   SRDA  R0,32                                                            
*&&DO*&& D     R0,=F'300'          GET SECONDS                                  
*&&OS*&& D     R0,=F'100'                                                       
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         ST    R0,DUB              SAVE SECONDS                                 
         SR    R0,R0                                                            
         D     R0,=F'60'           GIVES MIN IN R0,HRS IN R1                    
         MH    R1,=H'10000'        SHIFT LEFT                                   
         MH    R0,=H'100'                                                       
         AR    R1,R0                                                            
         A     R1,DUB                                                           
         CVD   R1,DUB                                                           
         MVC   WORK(10),=X'402120204B20204B2020'                                
         ED    WORK(10),DUB+4                                                   
         BR    RE                                                               
         EJECT                                                                  
INPUT2   AP    INCNT,=P'1'                                                      
         B     INPUT1                                                           
         SPACE 2                                                                
*&&DO                                                                           
ENDIN    CLOSE ADRIN                                                            
*&&                                                                             
*&&OS                                                                           
ENDIN    CLOSE (ADRIN)                                                          
*&&                                                                             
         XC    TIMSTOR1,TIMSTOR1                                                
         XC    TIMSTOR2,TIMSTOR2                                                
         MVI   LINSTOR,C' '                                                     
         MVC   LINSTOR+1(L'LINSTOR-1),LINSTOR                                   
         B     INPUT1                                                           
*                                                                               
ENDIN1   GOTO1 =V(SORTER),P1,=C'END'                                            
*                                                                               
         MVC   P+1(20),INMSG                                                    
         OI    INCNT+3,X'0F'                                                    
         UNPK  P+1(6),INCNT                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE                                                                  
*                                                                               
INCNT    DC    PL4'0'                                                           
INMSG    DC    C'999999 INPUT RECORDS'                                          
         EJECT                                                                  
DUB      DS    D                                                                
TABADDR  DS    A                                                                
SAVER1   DS    A                                                                
TIME     DC    F'0'                                                             
REC      DS    CL38                                                             
WORK     DS    CL24                                                             
TASKBIT  DC    X'00'                                                            
UPSI     DC    X'00'                                                            
*                                                                               
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
P4       DS    A                                                                
P5       DS    A                                                                
P6       DS    A                                                                
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,8,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=38'                                    
TIMSTOR1 DC    F'0'                START TIME STORE                             
TIMSTOR2 DC    F'0'                END TIME STORE                               
LINSTOR  DC    CL8' '              LINE ID / TERM ADDR STORE                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
*&&DO                                                                           
ADRIN    DTFMT BLKSIZE=1900,DEVADDR=SYS012,FILABL=STD,IOAREA1=INBUFF,  X        
               RECFORM=FIXBLK,RECSIZE=38,REWIND=UNLOAD,TYPEFLE=INPUT,  X        
               EOFADDR=ENDIN,WORKA=YES                                          
INBUFF   DS    1900C                                                            
*&&                                                                             
*&&OS                                                                           
ADRIN    DCB   DDNAME=ADRIN,DSORG=PS,MACRF=(GM),EODAD=ENDIN,           X        
               RECFM=FB,BLKSIZE=1900,LRECL=38                                   
*&&                                                                             
         EJECT                                                                  
* DDSADRREC                                                                     
       ++INCLUDE FAADRREC                                                       
         EJECT                                                                  
* FAPGMLST                                                                      
       ++INCLUDE FAPGMLST                                                       
         SPACE 2                                                                
* FASELIST                                                                      
       ++INCLUDE FASELIST                                                       
         EJECT                                                                  
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
* DSECT FOR INPUT PRINT LINE                                                    
         ORG   P                                                                
*                                                                               
         DS    2C                                                               
PLN      DS    CL4                                                              
         DS    C                                                                
PTRM     DS    CL4                                                              
         DS    C                                                                
PSYS     DS    CL7                                                              
         DS    2C                                                               
PPRG     DS    CL7                                                              
         DS    4C                                                               
PTSK     DS    C                                                                
         DS    3C                                                               
PSIN     DS    CL6                                                              
         DS    2C                                                               
PSTTM    DS    CL8                                                              
         DS    2C                                                               
PNDTM    DS    CL8                                                              
         DS    1C                                                               
PDUR     DS    CL6                                                              
         DS    3C                                                               
PCPU     DS    CL5                                                              
         DS    2C                                                               
PIO      DS    CL5                                                              
         DS    2C                                                               
PSTTU    DS    CL8                                                              
         DS    2C                                                               
PNDTU    DS    CL8                                                              
         DS    2C                                                               
PDIFF1   DS    CL8                                                              
         DS    2C                                                               
PDIFF2   DS    CL8                                                              
         EJECT                                                                  
* DSECT FOR OUTPUT PRINT LINE                                                   
*                                                                               
         ORG   P                                                                
         DS    C                                                                
PTIME    DS    CL10                                                             
         DS    2C                                                               
PTASK1   DS    C                                                                
         DS    2C                                                               
PTASK2   DS    C                                                                
         DS    2C                                                               
PTASK3   DS    C                                                                
         DS    2C                                                               
PTIMETU  DS    CL8                                                              
 END                                                                            
