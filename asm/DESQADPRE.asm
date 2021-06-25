*          DATA SET DESQADPRE  AT LEVEL 002 AS OF 03/21/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DESQPREA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'SORT/COMBINE SQAD'                                              
***********************************************************************         
* THIS IS THE PRE-PROCESSOR FOR SQAD HISPANIC AND RADIO                         
* PLEASE USE DESQSRT FOR SQAD TV, IT INCLUDES FQV SUPPORT                       
***********************************************************************         
SQADSORT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SQADSORT,VREGSAVE                                              
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT                                                             
*                                                                               
STXTAB   DS    0F                                                               
         DC    A(SQADSORT)                                                      
         DC    V(PDUMPER)                                                       
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT     OPEN  (FILIN1,(INPUT))                                                 
         MVI   INFILE,1            FIRST INPUT FILE                             
         GOTO1 =V(SORTER),DMCB,SORTCRD,RECCRD                                   
         XC    INCNT,INCNT                                                      
         XC    OUTCNT,OUTCNT                                                    
         LA    RE,IOAREA                                                        
         LH    R1,=Y(RECLENQ)                                                   
         AR    RE,R1                                                            
         ST    RE,AREC             A(SAVED RECORD)                              
*                                                                               
SORT2    GET   FILIN1,IOAREA+SORTKDQ                                            
*                                                                               
         L     RE,INCNT            COUNT THE INPUT RECORDS                      
         LA    RE,1(RE)                                                         
         ST    RE,INCNT                                                         
         LA    R2,IOAREA+SORTKDQ                                                
         LA    R3,IOAREA                                                        
         USING SORTKD,R3                                                        
*                                                                               
         CLC   4(12,R2),=C'"IssueMonth"'                                        
         BNE   SORT3                                                            
         XC    0(SORTKDQ,R3),0(R3)                                              
         B     SORT4                                                            
*                                                                               
SORT3    DS    0H                                                               
         AHI   R2,4                                                             
         XC    0(SORTKDQ,R3),0(R3)                                              
         LA    R4,SORTKTAB                                                      
         ZIC   R0,0(R4)                                                         
SORT3A   BAS   RE,PARSER                                                        
         ZIC   R0,0(R4)                                                         
         ZIC   R1,1(R4)                                                         
         ZIC   RE,WORK                                                          
         CR    R1,RE               MAKE SURE LENGTH OF FLD NOT EXCEED           
         BNL   *+6                                                              
         DC    H'0'                                                             
*        BCTR  RE,0                NOT GOING TO SUBTRACT ONE                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
         AR    R3,R1                                                            
         AHI   R3,1                                                             
         AHI   R4,2                                                             
         CLI   0(R4),X'FF'                                                      
         BE    SORT4                                                            
         ZIC   R0,0(R4)                                                         
         B     SORT3A                                                           
*                                                                               
SORT4    GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
         B     SORT2                                                            
*                                                                               
SORT10   CLOSE (FILIN1,)                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
SORT14   DS    0H                                                               
         OPEN  (FILOUT,(OUTPUT))                                                
*                                                                               
SORT15   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    DONE                                                             
         L     R2,DMCB+4           ADDRESS OF RECD FROM SORT                    
         AHI   R2,SORTKDQ                                                       
         PUT   FILOUT,(R2)                                                      
         B     SORT15                                                           
*                                                                               
DONE     L     RE,OUTCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OUTCNT                                                        
         GOTO1 =V(PRINTER)         BLANK LINE                                   
         MVC   P(23),=C'TOTAL RECORDS OUTPUT = '                                
         EDIT  OUTCNT,(8,P+25)                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
DONE10   CLOSE FILOUT                                                           
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*PARSER  - PARSING FIELDS                                                       
* ON ENTRY R2-POINTS TO BEGINNING OF WHERE TO START PARSING                     
*          R0-CONTAINS WHICH FIELD WE ARE LOOKING FOR                           
* ON EXIT  WORK  HAS THE LENGTH OF FIELD                                        
*          WORK+1  HAS THE PARSED FIELD                                         
***********************************************************************         
*                                                                               
PARSER   NTR1                                                                   
         LA    RE,WORK+1                                                        
         XC    WORK,WORK                                                        
         LA    R5,1                STARTING WITH FIRST FIELD                    
         LA    R7,0                FIELD LENGTH                                 
*                                                                               
PAR05    CR    R5,R0               THIS THE FIELD WE LOOKING FOR?               
         BL    PAR10                                                            
PAR06    LA    R2,1(R2)            MOVE PAST FIRST "                            
*                                                                               
PAR07    MVC   0(1,RE),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         LA    R7,1(R7)                                                         
         STC   R7,WORK             KEEP TRACK OF LENGTH                         
*                                                                               
PAR08    CLI   0(R2),C'"'          END OF FIELD YET?                            
         BNE   PAR07                                                            
         B     PARXX                                                            
*                                                                               
PAR10    CLI   0(R2),C','          START OF NEXT FIELD?                         
         BE    PAR15                                                            
PAR12    LA    R2,1(R2)            BUMP THROUGH TILL NEXT FIELD                 
         CLI   0(R2),C','                                                       
         BNE   PAR12                                                            
         LA    R5,1(R5)                                                         
         LA    R2,1(R2)                                                         
         B     PAR05                                                            
*                                                                               
PAR15    LA    R2,1(R2)            POINT TO NEXT STARTING "                     
         LA    R5,1(R5)            BUMP COUNTER                                 
         B     PAR05                                                            
PARXX    XIT1                                                                   
*                                                                               
* ****************************************************************              
*                                                                               
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(1,32,A),FORMAT=BI'                             
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=401'                                   
         EJECT                                                                  
FILIN1   DCB   DDNAME=FILIN1,DSORG=PS,RECFM=VB,MACRF=(GM),             X        
               EODAD=SORT10,LRECL=1200                                          
*                                                                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,MACRF=(PM),             X        
               LRECL=1200,BLKSIZE=3600                                          
*                                                                               
SORTKTAB DS    0H                                                               
         DC    AL1(6),AL1(3)                                                    
         DC    AL1(1),AL1(2)                                                    
         DC    AL1(2),AL1(4)                                                    
         DC    AL1(4),AL1(4)                                                    
         DC    AL1(3),AL1(1)                                                    
         DC    AL1(8),AL1(2)                                                    
         DC    AL1(7),AL1(2)                                                    
         DC    AL1(5),AL1(1)                                                    
         DC    AL1(9),AL1(6)                                                    
         DC    X'FF'                                                            
*                                                                               
INCNT    DC    F'0'                                                             
OUTCNT   DC    F'0'                                                             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
TMP      DS    CL5                                                              
INFILE   DS    X                                                                
REL      DS    X                                                                
CHAR     DS    C                                                                
AREC     DS    F                                                                
         DS    F                                                                
         SPACE 2                                                                
         LTORG                                                                  
RECLENQ  EQU   5000                                                             
IOAREA   DS    CL(RECLENQ)                                                      
LASTREC  DS    CL(RECLENQ)                                                      
         EJECT                                                                  
*                                                                               
SORTKD   DSECT                                                                  
SORTKY   DS    CL1                                                              
SORTMON  DS    CL2                 MONTH                                        
         DS    CL1                                                              
SORTYR   DS    CL4                 YEAR                                         
         DS    CL1                                                              
SORTQTR  DS    CL1                 QTR                                          
         DS    CL1                                                              
SORTDYR  DS    CL4                 DATA YEAR                                    
         DS    CL1                                                              
SORTLVL  DS    CL1                 CPP LEVEL                                    
         DS    CL1                                                              
SORTMID  DS    CL3                 MARKET ID                                    
         DS    CL1                                                              
SORTTID  DS    CL2                 TARGET ID                                    
         DS    CL1                                                              
SORTDID  DS    CL2                 DAYPART ID                                   
         DS    CL1                                                              
SORTCPP  DS    CL6                 CPP VALUE                                    
SORTKDQ  EQU   *-SORTKY                                                         
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DESQADPRE 03/21/14'                                      
         END                                                                    
