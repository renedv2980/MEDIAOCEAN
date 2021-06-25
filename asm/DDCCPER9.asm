*          DATA SET DDCCPER9   AT LEVEL 001 AS OF 01/17/07                      
*PHASE CCPER9A                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE DATCON                                                                 
         TITLE 'CONVERT PANCC LEVELS TO PER RECORDS'                            
         SPACE 1                                                                
CCPER9   CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CCPE**,=A(WORKAREA),RA,R9,R8,R7                    
         USING WORKD,RC                                                         
*                                                                               
         OPEN  (PANIN,INPUT)       OPEN ALL FILES                               
         OPEN  (PEROUT,OUTPUT)                                                  
*                                                                               
MAIN010  GET   PANIN               GET PAN DATA                                 
         LA    R4,4(R1)                                                         
         USING PANRECD,R4                                                       
         LA    R5,IO               SET UP PER RECORD                            
         USING PEPANLD,R5                                                       
*                                                                               
         MVI   PELPANID,PELPANIQ                                                
         MVC   PELBOOK,PANBK                                                    
*                                                                               
         PACK  DUB,PANLEV                                                       
         CVB   R1,DUB                                                           
         STC   R1,PELLEV                                                        
*                                                                               
         GOTO1 =V(DATCON),DMCB,(0,PANDAT1),(2,PELDAT)                           
         XC    PELDAT,=X'FFFF'                                                  
*                                                                               
         MVC   PELTYP,PANLANG      SET A OR O                                   
*                                                                               
         CLI   PELTYP,C'A'                                                      
         BNE   *+10                                                             
         CLC   PANTEST(18),SPACES                                               
         BNE   *+8                                                              
         MVI   PELTYP,C'B'         SOURCE BOOK WITH CATALP                      
*                                                                               
         CLI   PELTYP,C'O'         OBJ                                          
         BNE   *+8                                                              
         MVI   PELTYP,C'C'         FLAG AS CATALP                               
*                                                                               
         CLC   PANLIB+2(3),=C'PGM'                                              
         BNE   *+8                                                              
         MVI   PELTYP,C'P'         SET PHASE FROM DSPACE                        
*                                                                               
         CLC   PANLIB+2(3),=C'PRG'                                              
         BNE   *+8                                                              
         MVI   PELTYP,C'P'         SET PHASE FROM DSPACE                        
*                                                                               
         CLC   PANLIB+2(3),=C'TST'                                              
         BNE   *+8                                                              
         MVI   PELTYP,C'P'         SET PHASE FROM DSPACE                        
*                                                                               
         CLC   PANLIB+2(4),=C'LOAD'                                             
         BE    *+10                                                             
         CLC   PANLIB+2(4),=C'TEST'                                             
         BNE   *+8                                                              
         MVI   PELTYP,C'L'         SET LOAD LIBRARY MODULE                      
*                                                                               
         MVC   PELCTRY,PANLIB+1    K FROM UK OR S FROM US                       
*                                                                               
         MVC   PELLIB,PANLIB+2     APPL RMOR NYAP ETC                           
*                                                                               
         MVC   PELTEST,PANTEST     SET PHASE INFORMATION                        
         MVC   PELPROD,PANLIVE                                                  
         CLC   PELTEST(16),SPACES                                               
         BNE   *+16                                                             
         MVC   PELTEST,PANRMTST    OR RM IF BLANK                               
         MVC   PELPROD,PANRMLIV                                                 
*                                                                               
         LA    R1,PELDATA          ELEMENTS                                     
         USING PELELEM,R1                                                       
*                                                                               
         LA    R2,PANVAR           VARIABLE DATA CSECTS AND INCLUDES            
MAIN040  CLI   0(R2),C'I'                                                       
         BE    MAIN045                                                          
         CLI   0(R2),C'C'                                                       
         BE    MAIN045                                                          
         BNE   MAIN050                                                          
*                                                                               
MAIN045  MVC   PELELCOD,0(R2)                                                   
         MVI   PELELLEN,10                                                      
         MVC   PELNAME,1(R2)                                                    
*                                                                               
         LA    R2,10(R2)                                                        
         LA    R1,10(R1)                                                        
         B     MAIN040                                                          
*                                                                               
MAIN050  MVI   0(R1),X'00'                                                      
*                                                                               
         LA    R1,1(R1)            LENGTH CALCULATIONS                          
         SR    R1,R5                                                            
         STCM  R1,3,PELLEN                                                      
         LA    R1,4(R1)                                                         
         STCM  R1,3,IOL                                                         
*                                                                               
         PUT   PEROUT,IOL          PUT RECORD                                   
*                                                                               
MAIN051  CLC   PANRMTST,SPACES                                                  
         BE    MAIN052                                                          
         XC    IO(80),IO                                                        
         USING PEPANLPD,R5                                                      
         XC    PEPANLPK,PEPANLPK                                                
         MVI   PELPANLD,PELPANLQ                                                
         MVC   PELRM,PANRMTST                                                   
         MVC   PELSRC,PANBK                                                     
         USING PEPANLD,R5                                                       
         MVC   PELLEN,=X'002D'                                                  
         MVC   IOL,=X'0031'                                                     
         PUT   PEROUT,IOL          PUT PASSIVE                                  
*                                                                               
MAIN052  CLC   PANRMTST,SPACES                                                  
         BE    MAIN053                                                          
         XC    IO(80),IO                                                        
         USING PEPANLPD,R5                                                      
         XC    PEPANLPK,PEPANLPK                                                
         MVI   PELPANLD,PELPANLQ                                                
         MVC   PELRM,PANRMLIV                                                   
         MVC   PELSRC,PANBK                                                     
         USING PEPANLD,R5                                                       
         MVC   PELLEN,=X'002D'                                                  
         MVC   IOL,=X'0031'                                                     
         PUT   PEROUT,IOL          PUT PASSIVE                                  
*                                                                               
MAIN053  B     MAIN010                                                          
*                                                                               
PANINX   EQU   *                                                                
*                                                                               
MAIN990  CLOSE PANIN                                                            
         CLOSE PEROUT                                                           
*                                                                               
XBASE    XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LTORG                                          *         
***********************************************************************         
         SPACE 1                                                                
PANIN    DCB   DDNAME=PANIN,DSORG=PS,MACRF=(GL),RECFM=VB,              X        
               BLKSIZE=8200,LRECL=4096,BUFNO=2,EODAD=PANINX                     
PEROUT   DCB   DDNAME=PEROUT,DSORG=PS,MACRF=(PM),RECFM=VB,             X        
               BLKSIZE=8200,LRECL=4096,BUFNO=2                                  
*                                                                               
SPACES   DC    100C' '                                                          
         LTORG                                                                  
         EJECT                                                                  
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      DC    H'0'                                                             
         DC    X'FF'                                                            
         DC    X'02'               SUPPRESS RECOVERY                            
         DC    1020X'00'                                                        
         EJECT                                                                  
***********************************************************************         
*        WORK AREA                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'WORKAREAWORKAREA'                                           
WORKAREA DC    100000X'00'                                                      
*                                                                               
***********************************************************************         
*        WORKING STORAGE                                              *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
DMOFFS   DS    A                                                                
ALET     DS    A                                                                
*                                                                               
WORK     DS    CL132                                                            
*                                                                               
IOL      DS    XL4                                                              
IO       DS    CL4096                                                           
WORKX    EQU   *                                                                
*                                                                               
************************************************************                    
*        PAN DSECT                                         *                    
************************************************************                    
         SPACE 1                                                                
PANRECD  DSECT                                                                  
PANBK    DS    CL10                BOOK NAME DDBIGBOOK                          
         DS    CL1                                                              
PANLEV   DS    CL3                 LEVEL 001                                    
         DS    CL1                                                              
PANDAT1  DS    CL7                 DAT1 YYMMDD                                  
         DS    CL1                                                              
PANDATE  DS    CL8                 DATE                                         
         DS    CL1                                                              
PANLANG  DS    CL5                 LANGUAGE ASMB                                
         DS    CL1                                                              
PANLIB   DS    CL8                 LIBRARY UKRMOR UKAPPL UKNYAPPL               
         DS    CL1                                                              
PANTEST  DS    CL8                 TEST PHASE T11000A                           
         DS    CL1                                                              
PANLIVE  DS    CL8                 LIVE PHASE T11000                            
         DS    CL1                                                              
PANRMTST DS    CL8                 RM BOOK TEST FAMNTRA                         
         DS    CL1                                                              
PANRMLIV DS    CL8                 RM BOOK LIVE FAMNTR                          
         DS    CL1                                                              
PANVAR   DS    0CL1                CSECT AND INCLUDE INFO                       
*                                                                               
* FASSBOFF                                                                      
         PRINT ON                                                               
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE FAPROGSPCD                                                     
       ++INCLUDE DDARREDITD                                                     
       ++INCLUDE DMSPACED                                                       
       ++INCLUDE PEGENPAN                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDCCPER9  01/17/07'                                      
         END                                                                    
