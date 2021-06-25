*          DATA SET PPAFM00    AT LEVEL 036 AS OF 11/06/18                      
         TITLE 'T40700  CHANGE LOG'                                             
*                                                                               
* KWAN  06/20/18 NEW MEDIA D (DIGITAL AUDIO)                                    
*                                                                               
* KWAN  06/26/15 NEW MEDIA CODES B/W/V                                          
*                                                                               
* KWAN  02/20/14 NEW MEDIA L (SOCIAL)                                           
*                                                                               
* KWAN  03/00    ADD NEW FIELD RFP ID TO SCREEN AND STORE IN PAGYREC            
*                                                                               
* ROSA  5/31/88  MODIFY PGM NOT TO CLEAR OUT BOTTOM HALF OF SCREEN  L01         
*                WHEN ERROR OCCURS IN HEADER                                    
*                                                                               
         TITLE 'T40700  PRINTPAK SPECIAL FILE MAINTENANCE'                      
*************\                                                                  
*PHASE T40700A                                                                  
*************/                                                                  
T40700   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 500,T40700,RR=R9                                                 
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         BAS   RE,INITL                                                         
         USING T407FFD,RA                                                       
         LA    R9,IOAREA                                                        
         EJECT                                                                  
*                                                                               
*    SAVE FIRST 3 BYTES OF OVERLAY SCREEN AND SET HDRLAST TO CLEAR              
*  AFTER SO LOWER SCREEN WILL BE CLEARED IF AN EROR OCCURS IN                   
*  HEADER FIELDS                                                                
*                                                                               
         MVC   SAVESCRN(3),HDRLAST                                              
******** MVC **HDRLAST(3),=X'000001'         REMOVE LINE       L01              
         XC    HDRMSG(60),HDRMSG                                                
         TM    HDRRECH+4,X'20'                                                  
         BO    *+8                                                              
         B     NOTVAL                                                           
         TM    HDRACTH+4,X'20'                                                  
         BO    CKVALC           GO CHECK REC/ACT COMBINATION                    
         CLC   HDRACT(3),=C'CHA'      IF ACT=CHA AND LAST WAS DISP              
         BNE   NOTVAL              AND OTHER FIELDS VALIDATED                   
*                                  THEN DO EDIT                                 
         CLI   BACT,3                                                           
         BNE   NOTVAL                                                           
         TM    HDRMEDH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         TM    HDRAGYH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         OI    HDRACTH+4,X'20'     VALIDATE CHA AFTER DISP                      
         MVI   BACT,2                                                           
         B     CKVALC                                                           
*                                                                               
NOTVAL   NI    HDRRECH+4,X'DF'                                                  
         NI    HDRACTH+4,X'DF'                                                  
         NI    HDRMEDH+4,X'DF'                                                  
         NI    HDRAGYH+4,X'DF'                                                  
*                                                                               
*    VALIDATE RECORD                                                            
         LA    R2,HDRRECH                                                       
         LA    R3,RECERR                                                        
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         LA    R5,SFRECS                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,RELO                                                          
         LA    R5,6(R5)                                                         
         IC    R4,5(R2)                                                         
         BCTR  R4,R0                                                            
CKREC    EX    R4,COMP                                                          
         BE    CKRECA                                                           
         BXLE  R5,R6,CKREC                                                      
         B     ERROR                                                            
CKRECA   MVC   BREC(1),8(R5)                                                    
         OI    HDRRECH+4,X'20'          VALIDATE                                
         FOUT  HDRRECH                                                          
*   VALIDATE ACTION                                                             
*                                                                               
         LA    R2,HDRACTH                                                       
         LA    R3,ACTERR                                                        
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         LA    R5,SFACTS                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,RELO                                                          
         LA    R5,6(R5)                                                         
         IC    R4,5(R2)                                                         
         BCTR  R4,R0                                                            
CKACT    EX    R4,COMP                                                          
         BE    CKACTA                                                           
         BXLE  R5,R6,CKACT                                                      
         B     ERROR                                                            
CKACTA   MVC   BACT(1),8(R5)                                                    
         OI    HDRACTH+4,X'20'     VALIDATE                                     
         FOUT  HDRACTH                                                          
         B     CKVALC                                                           
*                                                                               
COMP     CLC   8(0,R2),0(R5)                                                    
*    VALIDATE REC/ACT COMBINATION                                               
CKVALC   LA    R5,COMBOS                                                        
         LA    R6,3                                                             
         LA    R7,COMBOSX-1                                                     
CKCOMB   CLC   BREC(2),0(R5)                                                    
         BE    BLDKEY                                                           
         BXLE  R5,R6,CKCOMB                                                     
         NI    HDRRECH+4,X'DF'                                                  
         NI    HDRACTH+4,X'DF'                                                  
         LA    R3,COMBERR                                                       
         B     ERROR                                                            
*                                                                               
*    REC/ACT OK - SO EDIT KEY                                                   
*                                                                               
*                                                                               
BLDKEY   MVC   OLNUM(1),2(R5)                                                   
AGYKEY   LA    R2,HDRMEDH                                                       
         LA    R3,MEDERR                                                        
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         XC    KEY,KEY                                                          
         CLI   HDRMED,C'B'         MOBILE?         ADDED ON JUN26/2015          
         BE    MVMED                                                            
         CLI   HDRMED,C'D'         DIGITAL AUDIO?  ADDED ON JUN20/2018          
         BE    MVMED                                                            
         CLI   HDRMED,C'I'         INTERACTIVE?    ADDED ON 10/5/98             
         BE    MVMED                                                            
         CLI   HDRMED,C'L'         SOCIAL?         ADDED ON FEB20/2014          
         BE    MVMED                                                            
         CLI   HDRMED,C'M'         MAGAZINE?                                    
         BE    MVMED                                                            
         CLI   HDRMED,C'N'         NEWSPAPER?                                   
         BE    MVMED                                                            
         CLI   HDRMED,C'O'         OUTDOOR                                      
         BE    MVMED                                                            
         CLI   HDRMED,C'S'         SUPPLEMENT/SEARCH?                           
         BE    MVMED                                                            
         CLI   HDRMED,C'T'         TRADE MAGS?                                  
         BE    MVMED                                                            
         CLI   HDRMED,C'V'         NATIONAL VIDEO? ADDED ON JUN26/2015          
         BE    MVMED                                                            
         CLI   HDRMED,C'W'         LOCAL VIDEO?    ADDED ON JUN26/2015          
         BE    MVMED                                                            
         B     ERROR                                                            
MVMED    MVC   KMED,HDRMED                                                      
         TM    4(R2),X'20'                                                      
         BO    CKAGY                                                            
         FOUT  HDRMEDNH,SPACES,10                                               
         FOUT  HDRAGYNH,SPACES,33                                               
         NI    HDRAGYH+4,X'DF'                                                  
         OI    4(R2),X'20'                                                      
         MVI   BYTE2,1             SET ACTION = FORMAT                          
         B     CKAGY                                                            
         EJECT                                                                  
*                                                                               
*   VALIDATE AGENCY                                                             
*                                                                               
CKAGY    LA    R2,HDRAGYH                                                       
         LA    R3,AGYERR                                                        
         MVC   KAGY,HDRAGY                                                      
         MVI   KRCD,X'01'                                                       
         TM    4(R2),X'20'                                                      
         BO    CKRPT                                                            
         FOUT  HDRMEDNH,SPACES,10                                               
         FOUT  HDRAGYNH,SPACES,33                                               
         CLI   5(R2),2                                                          
         BNE   ERROR                                                            
         CLC   BREC(2),=X'0101'                                                 
         BE    CKADD                                                            
         BAS   RE,READ                                                          
         MVC   AGYADDR(4),KEY+27                                                
         BAS   RE,GETREC                                                        
         USING PAGYREC,R9                                                       
         FOUT  HDRAGYNH,PAGYNAME,33                                             
         FOUT  HDRMEDNH,PAGYMED,10                                              
CKAGYX   DS    0H                                                               
         OI    4(R2),X'20'                                                      
         MVI   BYTE2,1             SET ACTION = FORMAT                          
         B     CKRPT                                                            
*                                                                               
         EJECT                                                                  
CKRPT    CLI   BREC,1                                                           
         BE    GETOVLY                                                          
         DC    H'0'                                                             
*                                                                               
CKADD    LA    R4,=C'DMREAD'                                                    
         LA    R5,=C'PRTDIR'                                                    
         LA    R6,KEY                                                           
         LA    R7,KEYSAVE                                                       
         LA    R1,DMCB                                                          
         STM   R4,R7,0(R1)                                                      
         MVI   0(R1),X'08'         SET TO PASS DELETED RECS                     
         MVC   16(1,R1),TERMNAL                                                 
         L     RF,VDATAMGR                                                      
         BASR  RE,RF                                                            
         LA    R3,51                                                            
         TM    8(R1),X'40'         DISK ERROR                                   
         BNZ   ERROR                                                            
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ADDOK                                                            
         LA    R3,56                                                            
         TM    KEYSAVE+25,X'80'    DELETED                                      
         BNZ   ERROR                                                            
         LA    R3,52               DUP KEY ON ADD                               
         B     ERROR                                                            
ADDOK    MVI   BYTE2,1             SET ACTION = FORMAT                          
         OI    4(R2),X'20'         VALIDATE                                     
         B     GETOVLY                                                          
         EJECT                                                                  
         CNOP  2,4                                                              
SFRECS   DC    H'9'                                                             
         DC    A(SFRECSX-1)                                                     
         DC    CL8'AGENCY'                                                      
         DC    X'01'                                                            
         DC    CL8'AGYHDR'                                                      
         DC    X'01'                                                            
SFRECSX  EQU   *                                                                
         EJECT                                                                  
         CNOP  2,4                                                              
SFACTS   DC    H'9'                                                             
         DC    A(SFACTSX-1)                                                     
         DC    CL8'ADD'                                                         
         DC    X'01'                                                            
         DC    CL8'CHANGE'                                                      
         DC    X'02'                                                            
         DC    CL8'DISPLAY'                                                     
         DC    X'03'                                                            
SFACTSX  EQU   *                                                                
*                                                                               
*                                                                               
COMBOS   DC    X'010101'           REC/ACT/OLNUM                                
         DC    X'010201'                                                        
         DC    X'010301'                                                        
COMBOSX  EQU   *                                                                
*                                                                               
SAVESCRN DS    CL3                                                              
SPACES   DC    CL40' '                                                          
RECERR   EQU   11                                                               
ACTERR   EQU   12                                                               
MEDERR   EQU   13                                                               
AGYERR   EQU   35                                                               
COMBERR  EQU   10                                                               
         EJECT                                                                  
*                                                                               
* RESTORE FIRST 3 BYTES OF OVERLAY SCREEN - NO ERROR IN HEADER FIELDS           
*                                                                               
GETOVLY  MVC   HDRLAST(3),SAVESCRN                                              
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),OLNUM                                                    
         ST    RA,DMCB+4                                                        
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
         BASR  RE,RF                                                            
         TM    4(R1),X'FF'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RC,DMCB                                                          
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         CLI   BYTE3,1             SEE IF ACTION COMPLETED                      
         BNE   EXXMOD                                                           
         MVC   HDRMSG(60),=CL60'*** ACTION COMPLETED ***'                       
         LA    R2,HDRRECH                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGENEROL                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE GENOLD                                                         
*                                                                               
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDFLDIND                                                       
         ORG   KEY                                                              
KAGY     DS    CL2                                                              
KMED     DS    CL1                                                              
KRCD     DS    CL1                                                              
KSYS     DS    CL1                                                              
KMSG     DS    CL1                                                              
         DS    CL26                                                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPAFMFFD                                                       
         ORG   T407FFD                                                          
         DS    CL16                                                             
BREC     DS    CL1                                                              
BACT     DS    CL1                                                              
OLNUM    DS    CL1                                                              
AGYADDR  DS    F                                                                
SCRNUM   DS    CL1                                                              
*                                                                               
PUBIO    DS    10C                 ** NOT USED - TAG HERE TO PREVENT            
*                                  ASSEMBLY ERR (PGENEROL USES PUBIO)           
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036PPAFM00   11/06/18'                                      
         END                                                                    
