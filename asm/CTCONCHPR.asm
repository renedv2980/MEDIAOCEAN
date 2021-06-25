*          DATA SET CTCONCHPR  AT LEVEL 003 AS OF 05/01/02                      
*PHASE CTCHPRA                                                                  
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
***********************************************************************         
*  PURPOSE:  COPY B7'S FROM B5'S THAT ARE WI AND ALL,ALL                        
***********************************************************************         
CTCHPR   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*CTCHPR*,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         EJECT                                                                  
MAIN     DS    0H                                                               
         MVC   NEXT,=A(TABLE)                                                   
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',FLIST,AIO                
         MVI   DATADISP+1,28                                                    
         OPEN  (TOUT,OUTPUT)       QSAM MACRO                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
                                                                                
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, BAD READ                                
                                                                                
         L     R6,AIO              POINT AT REC READ                            
         USING CTUREC,R6                                                        
         B     M20                                                              
                                                                                
M10      GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,AIO               
         CLI   DMCB+8,X'80'        NO MORE RECORDS?                             
         BE    M50                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, BAD READ                                
                                                                                
M20      L     R1,TOTREC                                                        
         LA    R1,1(R1)                                                         
         ST    R1,TOTREC                                                        
         CLI   0(R6),CTUKTYPQ      RECORD TYPE 'U'                              
         BNE   M40                                                              
         CLC   CTUKAGY,=C'WI'      AGENCY                                       
         BNE   M40                                                              
         CLI   CTUKMED,0           ALL                                          
         BNE   M40                                                              
         CLC   =C'B5',CTUKPROG+1   PROGRAM B5                                   
         BE    M30                                                              
         CLC   =C'B7',CTUKPROG+1   PROGRAM B7                                   
         BNE   M40                                                              
         BAS   RE,CHKTAB           CHECK IF NEW RECORD REPLACING                
         CLI   FOUND,C'Y'                                                       
         BE    M10                 RECORD REPLACED, DON'T PUT TO SORTER         
         B     M40                                                              
                                                                                
M30      BAS   RE,SPUT             PUT B5 RECORD TO SORTER                      
         BAS   RE,ADDRECD          ADD B7 RECORD TO TABLE                       
         BAS   RE,SPUT             PUT B7 RECORD TO SORTER                      
         GOTO1 =V(HEXOUT),DMCB,(R6),P,25,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         L     R1,COPIED                                                        
         LA    R1,1(R1)                                                         
         ST    R1,COPIED                                                        
         B     M10                                                              
                                                                                
M40      BAS   RE,SPUT             PUT RECORD TO SORTER                         
         B     M10                                                              
                                                                                
M50      GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R6,15,4(R1)                                                      
         BZ    MX                                                               
         CLC   KEYSAVE,4(R6)                                                    
         BNE   M60                                                              
         BAS   RE,DUPKEY                                                        
         B     M50                                                              
                                                                                
M60      PUT   TOUT,(R6)                                                        
         MVC   KEYSAVE,4(R6)                                                    
         B     M50                                                              
                                                                                
MX       GOTO1 =V(SORTER),DMCB,=C'END'                                          
         MVC   P(14),=C'TOTAL REC READ'                                         
         EDIT  (4,TOTREC),(8,P+20)                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(16),=C'TOTAL REC COPIED'                                       
         EDIT  (4,COPIED),(8,P+20)                                              
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
* ADDRECD: PUT RECORD IN TABLE                                        *         
***********************************************************************         
         SPACE                                                                  
ADDRECD  NTR1                                                                   
         L     R2,NEXT                                                          
                                                                                
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   CTUKPROG+2,C'7'     COPY B5'S AS A B7                            
         MVC   0(L'CTUKEY,R2),0(R6)    SAVE COPY OF KEY                         
         LA    R2,L'CTUKEY(R2)     POINT TO ARE FOR NEXT KEY                    
         ST    R2,NEXT                                                          
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHKTAB: CHECK THAT KEY DOESN'T ALREADY EXIST                        *         
***********************************************************************         
         SPACE                                                                  
CHKTAB   NTR1                                                                   
         MVI   FOUND,C'N'          KEY WAS NOT FOUND                            
         LA    R2,TABLE                                                         
                                                                                
CHKTAB10 CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    CHKTABX                                                          
                                                                                
         C     R2,NEXT             NO MORE KEYS IN TABLE?                       
         BE    CHKTABX                                                          
                                                                                
         CLC   0(L'CTUKEY,R2),0(R6)   SAME KEY                                  
         BE    *+12                                                             
         LA    R2,L'CTUKEY(R2)                                                  
         B     CHKTAB10                                                         
                                                                                
         MVI   FOUND,C'Y'          KEY ALREADY EXISTS                           
         MVI   P+1,C'*'                                                         
         MVC   P+3(25),0(R2)                                                    
         GOTO1 =V(PRINTER)                                                      
                                                                                
         DROP  R6                                                               
CHKTABX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SPUT: SORT PUT, SET LEN, 2 BYTES OF NULLS, AND VERIFY VALID LENGTH  *         
***********************************************************************         
         SPACE                                                                  
SPUT     NTR1                                                                   
         L     R7,AIO                                                           
         L     R2,ARECLEN                                                       
                                                                                
* IS THE RECORD LENGTH ALWAYS AT 25                                             
SP10     MVC   0(2,R2),25(R7)      GET RECORD LEN FOR PUT                       
         LA    R1,4                ADD 4 TO COVER 2 BYTES LEN + ...             
         AH    R1,0(R2)            2 BYTES OF NULLS                             
         STH   R1,0(R2)                                                         
         CH    R1,=H'32'           25B KEY + 4B ABOVE + 3B(LEN+STAT)            
         BNL   *+6                                                              
         DC    H'0'                DIE, BAD LENGTH                              
         GOTO1 =V(SORTER),DMCB,=C'PUT',ARECLEN                                  
                                                                                
SPX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DUPKEY: PRINT DUPLICATE KEY ERROR                                   *         
***********************************************************************         
         SPACE                                                                  
DUPKEY   NTR1                                                                   
         MVC   P(22),=C'ERROR, DUPLICATE KEY: '                                 
         LA    R6,4(R6)                                                         
         GOTO1 =V(HEXOUT),DMCB,(R6),P+25,25,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(CTCHPR,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG   *                                                                
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,BLKSIZE=8200,LRECL=2048,BUFNO=2                         
*                                                                               
SSB      DC    F'0'                                                             
UTL      DC    F'0',X'0A'                                                       
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2048'                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
ARECLEN  DC    A(RECLEN)                                                        
AIO      DC    A(IO)                                                            
*                                                                               
NEXT     DS    F                                                                
TOTREC   DS    F                                                                
COPIED   DS    F                                                                
DUB      DS    D                                                                
DATADISP DS    H                                                                
FOUND    DS    X                                                                
WORK     DS    CL17                                                             
ELCODE   DS    CL1                                                              
KEY      DS    CL25                                                             
KEYSAVE  DS    CL25                                                             
*                                                                               
*                                                                               
DMCB     DS    6F                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
RECLEN   DS    H                   REC LEN FOR QSAM PUT                         
         DC    H'0'                                                             
IO       DS    XL2000              IO AREA                                      
*                                                                               
TABLE    DS    1000CL25                                                         
         DC    X'FF'                                                            
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTCONCHPR 05/01/02'                                      
         END                                                                    
