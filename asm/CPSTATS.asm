*          DATA SET CPSTATS    AT LEVEL 017 AS OF 05/01/02                      
*PHASE CPSTATSA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'CPSTAT - COST PER POINT FILE STATISTICS'                        
CPSTATS  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**CPSTAT,=V(REGSAVE)                                           
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         MVC   MID1(79),INHEAD                                                  
         MVC   MID2(79),UNDRLN                                                  
         MVC   TITLE(17),=C'CPP FILE ANALYSIS'                                  
         B     INPUT                                                            
         SPACE 2                                                                
INHEAD   DC    C'            TYPE          AGENCY          RECORDS     X        
                  AVE BYTES         TIME'                                       
UNDRLN   DC    C'            ----          ------          -------     X        
                  /RECORD           ----'                                       
         EJECT                                                                  
         XC    TYPESTR,TYPESTR                                                  
INPUT    L     R4,=A(OUTDATA)                                                   
         LA    R4,4(R4)                                                         
         USING CPKEYD,R4                                                        
         OPEN  (CPTIN,(INPUT))     OPEN THE FILE                                
         SPACE 2                                                                
INPUT1   L     R2,=A(OUTDATA)                                                   
         GET   CPTIN,(R2)          GET A RECORD                                 
         SPACE 2                                                                
         CLC   TYPESTR,CPKTYPE                                                  
         BE    CPA20                                                            
         CLI   TYPESTR,0                                                        
         BE    *+8                                                              
         BAS   RE,NEWTYPE          PRINT TOTALS FROM PREVIOUS TYPE              
         CLI   CPKTYPE,X'FF'       LAST RECORD                                  
         BE    INPUT1                                                           
         MVC   TYPESTR,CPKTYPE                                                  
         ZAP   TOTREC,=P'0'                                                     
         XC    TOTBYTE,TOTBYTE                                                  
         XC    AGYSTR,AGYSTR                                                    
         SPACE 1                                                                
CPA20    CLI   TYPESTR,X'06'       FOR BANK DATA,THERE'S NO AGENCY              
         BE    CPA23               BUT WE FUDGE IT AFTER THE FIRST TIME         
         CLC   AGYSTR,CPKAGY                                                    
         BE    CPA40                                                            
         B     CPA25                                                            
         SPACE 1                                                                
CPA23    CLC   AGYSTR,CPKAGY                                                    
         BNE   CPA40                                                            
         SPACE 1                                                                
CPA25    CLI   AGYSTR,0                                                         
         BE    *+8                                                              
         BAS   RE,NEWAGY           PRINT TOTALS FROM PREVIOUS AGENCY            
         MVC   AGYSTR,CPKAGY                                                    
         LA    R3,RECTYPE          FIND CORRECT RECORD TYPE NAME                
CPA27    CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),TYPESTR                                                  
         BE    CPA35                                                            
         LA    R3,L'RECTYPE(R3)                                                 
         B     CPA27                                                            
         SPACE 1                                                                
CPA35    MVC   PTYPE(L'RECTYPE-1),1(R3)                                         
         MVC   PAGY,NOAGY                                                       
         CLI   CPKTYPE,X'06'                                                    
         BNE   *+14                                                             
         MVC   AGYSTR,NOAGY        FOR BANK DATA, FUDGE AGENCY NAME             
         B     *+10                                                             
         MVC   PAGY,CPKAGY                                                      
         ZAP   RECCT,=P'0'                                                      
         XC    BYTECT,BYTECT                                                    
         SPACE 2                                                                
CPA40    AP    RECCT,=P'1'                                                      
         L     R5,BYTECT                                                        
         AH    R5,CPLENGTH                                                      
         ST    R5,BYTECT                                                        
         B     INPUT1                                                           
         SPACE 3                                                                
CPTEOF   CLOSE (CPTIN)                                                          
         XBASE                                                                  
         EJECT                                                                  
*ROUTINE FOR A NEW RECORD TYPE                                                  
         SPACE 3                                                                
NEWTYPE  NTR1                                                                   
         BAS   RE,NEWAGY        FIRST PRINT LAST AGENCY FOR LAST TYPE           
         SPACE 1                                                                
         LA    R3,RECTYPE                                                       
REC1     CLI   0(R3),X'FF'         FIND CORRECT RECORD TYPE NAME                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),TYPESTR                                                  
         BE    REC2                                                             
         LA    R3,L'RECTYPE(R3)                                                 
         B     REC1                                                             
         SPACE 1                                                                
REC2     MVC   PTYPE(L'RECTYPE-1),1(R3)                                         
         MVC   PAGY(7),=C'*TOTAL*'                                              
         EDIT  (P6,TOTREC),(10,PREC),COMMAS=YES                                 
         SPACE 2                                                                
         L     R7,TOTBYTE          PREPARE FOR AVERAGE ROUTINE                  
         ZAP   DUB,TOTREC                                                       
         CVB   R8,DUB                                                           
         BAS   RE,AVERAGE                                                       
         EDIT  (R7),(10,PBYTE),1                                                
         EDIT  (TIME,NOW),(8,PTIME)                                             
         GOTO1 =V(PRINTER)                                                      
         ZAP   LINE,=P'75'         NEW PAGE FOR EACH TYPE                       
XIT      XIT1                                                                   
         EJECT                                                                  
*        ROUTINE FOR A NEW AGENCY                                               
         SPACE 2                                                                
NEWAGY   NTR1                                                                   
         EDIT  (P6,RECCT),(10,PREC),COMMAS=YES                                  
         AP    TOTREC,RECCT                                                     
         L     R7,BYTECT                                                        
         A     R7,TOTBYTE                                                       
         ST    R7,TOTBYTE                                                       
         SPACE 2                                                                
         L     R7,BYTECT           PREPARE FOR AVERAGE ROUTINE                  
         ZAP   DUB,RECCT                                                        
         CVB   R8,DUB                                                           
         BAS   RE,AVERAGE                                                       
         EDIT  (R7),(10,PBYTE),1                                                
         EDIT  (TIME,NOW),(8,PTIME)                                             
         SPACE 2                                                                
         GOTO1 =V(PRINTER)                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                  R7=DIVIDEND                                  
*                                  R8=DIVISOR                                   
AVERAGE  DS    0H                                                               
         XR    R6,R6                                                            
         M     R6,=F'10'           NEED ONE DECIMAL PLACE                       
         SLDA  R6,1                MULTIPLY BY 2                                
         DR    R6,R8               (ACCUM X 10)/TOTAL                           
         LTR   R7,R7                                                            
         BM    *+8                                                              
         A     R7,=F'1'                                                         
         SRA   R7,1           DIVIDE BY TWO - R7 HAS ROUNDED ANSWER             
         BR    RE                                                               
         EJECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL24                                                             
TYPESTR  DS    CL1                                                              
AGYSTR   DS    CL2                                                              
RECCT    DS    PL6                                                              
BYTECT   DS    F                                                                
BYTEREC  DS    F                                                                
TOTREC   DS    PL6                                                              
TOTBYTE  DS    F                                                                
TOTBYREC DS    F                                                                
NOAGY    DC    CL2'**'                                                          
*                                                                               
*                                                                               
*                                                                               
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
P4       DS    A                                                                
P5       DS    A                                                                
P6       DS    A                                                                
         EJECT                                                                  
*              RECORD TYPE TABLE                                                
         SPACE 2                                                                
RECTYPE  DS    0CL12                                                            
         DC    X'02',CL11'CLIENT DATA'                                          
         DC    X'04',CL11'AGENCY DATA'                                          
         DC    X'06',CL11'BANK DATA  '                                          
         DC    X'12',CL11'CLIENT NAME'                                          
         DC    X'14',CL11'AGENCY NAME'                                          
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
CPTIN    DCB   DDNAME=CPTIN,                                           X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=08196,                                            X        
               BLKSIZE=08200,                                          X        
               MACRF=GM,                                               X        
               EODAD=CPTEOF                                                     
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         DC    C'* OUTPUT DATA **'                                              
OUTDATA  CSECT                                                                  
         DC    1040X'00'                                                        
         EJECT                                                                  
       ++INCLUDE CPGENFILE                                                      
         EJECT                                                                  
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
       EJECT                                                                    
*              DSECT FOR PRINT LINE                                             
         ORG   P                                                                
         DS    9C                                                               
PTYPE    DS    CL11                                                             
         DS    8C                                                               
PAGY     DS    CL2                                                              
         DS    9C                                                               
PREC     DS    PL10                                                             
         DS    5C                                                               
PBYTE    DS    PL10                                                             
         DS    10C                                                              
PTIME    DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017CPSTATS   05/01/02'                                      
         END                                                                    
