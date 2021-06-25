*          DATA SET ACLFM08    AT LEVEL 051 AS OF 05/01/02                      
*PHASE T60308A,+0                                                               
*INCLUDE SCANNER                                                                
*INCLUDE UNSCAN                                                                 
         TITLE 'MODULE TO HANDLE CLIENT RECORD'                                 
T60308   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM8**                                                       
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T60308+4096,R7                                                   
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         LA    R8,SAVECOMP                                                      
         USING ACCOMPD,R8                                                       
         LA    R9,SAVEHEIR                                                      
         USING ACHEIRD,R9                                                       
         EJECT                                                                  
**********************************************************************          
* BUILD KEY FOR CLIENT                                               *          
**********************************************************************          
         SPACE 1                                                                
         CLI   MODE,BUILDKEY                                                    
         BNE   CL100                                                            
         BAS   RE,ANYCOMP                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPJOB                                                 
         LA    R2,LOGCLIH                                                       
         GOTO1 ANY                                                              
         CLI   8(R2),C' '          DOES CLIENT START WITH A BLANK ?             
         BNE   CL10                NO                                           
         MVI   ERROR,INVALID       YES, ERROR                                   
         B     XIT                                                              
         SPACE 2                                                                
CL10     CLC   8(3,R2),=C'ALL'     IS THE CLIENT "ALL" ?                        
         BNE   CL20                NO                                           
         MVI   ERROR,14            YES, ERROR                                   
         B     XIT                                                              
         SPACE 2                                                                
CL20     CLC   5(1,R2),ACHRLEVA                                                 
         BNH   CL30                                                             
         MVI   ERROR,ACTOOLNG                                                   
         B     XIT                                                              
*                                                                               
CL30     CLI   LOGACT,C'N'         ARE THEY TRYING TO ADD?                      
         BNE   CL60                NO - SKIP THIS CODE                          
         OC    ACMPCFIL,ACMPCFIL   IS THERE ANY AGENCY ID?                      
         BZ    CL60                NO - SKIP THIS CODE                          
         USING ZENRECD,R5                                                       
         LA    R5,IO2                                                           
         XC    ZENRKEY,ZENRKEY     CLEAR KEY                                    
         MVI   ZENKCODE,ZENKCODQ   X'05' - ZENITH RECORD                        
         MVI   ZENKTYP,ZENCLTQ     X'09' - CLIENT RECORD                        
         MVC   ZENKAGY,ACMPCFIL    COMPANY CODE                                 
         MVC   ZENKCLT,LOGCLI      CLIENT CODE                                  
         OC    ZENKCLT,SPACES      IF TWO BYTE CODE FIL W/SPACE                 
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',IO2,IO2,0                     
         CLI   DMCB+8,0                                                         
         BNE   CL40                GOTO ERROR ROUTINE                           
*                                                                               
         LA    R5,ZENFIRST         FIRST ELEMENT                                
         USING ZENELEM,R5                                                       
         CLI   ZENELCD,ZENELCDQ    X'01' - ZENITH ELEMENT                       
         BE    CL50                                                             
*                                                                               
CL40     LA    R2,LOGCLIH          ADDRESS CLIENT HEADER                        
         MVI   ERROR,INVCLI        INVALID CLIENT CODE                          
         B     XIT                                                              
*                                                                               
CL50     LA    R1,L'ZENCNAME       FIND ACTUAL LENGTH                           
         LA    R3,ZENCNAME         R3 = A(ZENITH CLIENT NAME)                   
         LA    R3,L'ZENCNAME-1(R3) R3 = LAST POSITION IN ZENCNAME               
         CLI   0(R3),C' '          COMPARE IF CURRENT POS IS SPACE              
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   R1,*-10                                                          
         STC   R1,LOGCNAMH+5       STICK LENGTH INTO HEADER                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   LOGCNAM(0),ZENCNAME                                              
         OI    LOGCNAMH+6,X'80'    TRANSMIT                                     
         DROP  R5                                                               
*                                                                               
CL60     GOTO1 MOVE                                                             
         MVC   KEY+3(12),WORK                                                   
         TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         MVI   ANYKEY,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DISPLAY CLIENT RECORD                                             *           
**********************************************************************          
         SPACE 1                                                                
CL100    CLI   MODE,DSPLYREC                                                    
         BNE   CL200                                                            
         OI    LOGCLIH+4,X'20'                                                  
         LA    R2,LOGNMDSH                                                      
         GOTO1 NAMOUT                                                           
         LA    R2,LOGCNAMH                                                      
         BAS   RE,HANDLPRF                                                      
         BAS   RE,HANDLEX                                                       
         OI    4(R2),X'20'                                                      
         FOUT  LOGNUMBH,SPACES,50                                               
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
         LA    R6,LOGNUMB                                                       
         SPACE 2                                                                
CL110    CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),X'11'                                                      
         BE    CL140                                                            
         CLI   0(R4),X'21'         LOOK FOR NUMBER ELEMENTS                     
         BNE   CL130                                                            
         USING ACNUMD,R4                                                        
         LA    R1,LOGNUMB          IF THIS IS NOT THE FIRST ...                 
         CR    R1,R6                                                            
         BE    CL120                                                            
         MVI   0(R6),C','          PUT A COMMA BETWEEN FIELDS                   
         LA    R6,1(R6)                                                         
         SPACE 2                                                                
CL120    MVC   0(6,R6),ACNUMAFT    PUT OUT THE NUMBER                           
         LA    R6,6(R6)                                                         
         CLI   ACNUMLEN,14         THIS MAY BE PRECEDED BY MEDIA CODE           
         BE    CL130                                                            
         SH    R6,=H'6'                                                         
         MVC   0(1,R6),ACNUMTYP                                                 
         MVI   1(R6),C'='                                                       
         MVC   2(6,R6),ACNUMAFT                                                 
         LA    R6,8(R6)                                                         
         SPACE 2                                                                
CL130    IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     CL110                                                            
         SPACE 1                                                                
         USING ACMEDIAD,R4                                                      
CL140    MVC   0(6,R6),ACMDLBIL    LAST NUMBER                                  
         MVI   6(R6),C','                                                       
         MVC   7(4,R6),ACMDRSET    AND RESET                                    
         B     CL130                                                            
         EJECT                                                                  
**********************************************************************          
* BUILD CLIENT RECORD                                                *          
**********************************************************************          
         SPACE 1                                                                
CL200    LA    R2,LOGCNAMH                                                      
         GOTO1 ANY                                                              
         CLI   LOGACT,C'A'         FOR ACTION AMEND, SAVE NAME                  
         BNE   CL210                                                            
         GOTO1 CHKNAM,DMCB,(C'B',IO),NAMESAVE                                   
         LA    RF,NAMESAVE-LOCALS  SAVE DISP TO SAVED NAME                      
         STCM  RF,3,DSAVNAM        FOR SEARCH                                   
*                                                                               
CL210    GOTO1 NAMIN                                                            
         BAS   RE,HANDLPRF                                                      
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         BAS   RE,HANDLEX                                                       
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
**T                                                                             
         CLI   ACCEMU,C'Y'         IS IT THE NEW FILE                           
         BNE   CL220                                                            
         CLI   LOGACT,C'A'         MUST BE AMEND                                
         BNE   CL220                                                            
         TM    4(R2),X'20'         HAS NAME CHANGED                             
         BO    CL220                                                            
         GOTO1 CHKNAM,DMCB,(C'A',IO2),NAMESAVE                                  
CL220    DS    0H                                                               
**T                                                                             
*&&US*&& GOTO1 REMANEL,DMCB,(X'11',0)     FOR NUMBERS BY CLIENT                 
         GOTO1 REMANEL,DMCB,(X'21',0)                                           
         LA    R2,LOGNUMBH                                                      
         CLI   5(R2),0                                                          
         BE    CL260                                                            
         GOTO1 =V(SCANNER),DMCB,(R2),(10,BLOCK),0,RR=RB                         
         MVI   ERROR,INVALID                                                    
         LA    R3,BLOCK                                                         
         SR    R5,R5                                                            
         IC    R5,4(R1)                                                         
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         LA    R4,ELEMENT                                                       
*&&US                                                                           
         CH    R5,=H'2'            ALLOW NUMBER AND RESET                       
         BNE   CL240                        NNNNNN,NNNN                         
         CLC   0(2,R3),=X'0600'                                                 
         BNE   CL240                                                            
         OC    4(4,R3),4(R3)                                                    
         BZ    XIT                 NOT NUMBERIC                                 
         XC    ELEMENT,ELEMENT                                                  
         USING ACMEDIAD,R4                                                      
         MVC   ACMDEL(2),=X'1146'                                               
         MVC   ACMDFBIL,12(R3)                                                  
         MVC   ACMDLBIL,12(R3)                                                  
         LA    R3,32(R3)                                                        
         CLC   0(2,R3),=X'0400'                                                 
         BNE   XIT                 NO RESET                                     
         OC    4(4,R3),4(R3)                                                    
         MVC   ACMDRSET,12(R3)                                                  
         GOTO1 ADDANEL                                                          
         MVI   ERROR,X'FF'                                                      
         B     CL260                                                            
*&&                                                                             
         USING ACNUMD,R4                                                        
         SPACE 2                                                                
CL230    MVI   ACNUMEL,X'21'                                                    
         MVI   ACNUMLEN,14                                                      
         MVC   ACNUMBEF,12(R3)                                                  
         MVC   ACNUMAFT,12(R3)                                                  
         CLC   0(2,R3),=X'0600'    WE ALLOW NNNNNN  UK ONLY                     
         BE    CL250                                                            
CL240    MVI   ACNUMEL,X'21'                                                    
         MVI   ACNUMLEN,15                                                      
         MVC   ACNUMBEF,22(R3)                                                  
         MVC   ACNUMAFT,22(R3)                                                  
         MVC   ACNUMTYP,12(R3)                                                  
         CLC   0(2,R3),=X'0106'    OR M=NNNNNN                                  
         BE    CL250                                                            
         MVC   ACNUMBEF,=6X'F0'                                                 
         MVC   ACNUMAFT,=6X'F0'                                                 
         CLC   0(2,R3),=X'0100'    OR M                                         
         BNE   XIT                                                              
         SPACE 2                                                                
CL250    MVC   WORK(6),=6X'F0'                                                  
         MVZ   WORK(6),ACNUMBEF                                                 
         CLC   WORK(6),=6X'F0'                                                  
         BNZ   XIT                                                              
         GOTO1 ADDANEL                                                          
         LA    R3,32(R3)                                                        
*&&UK*&& BCT   R5,CL230                                                         
*&&US*&& BCT   R5,CL240                                                         
         MVI   ERROR,X'FF'                                                      
         SPACE 2                                                                
CL260    GOTO1 STATIN                                                           
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
         DROP  R8                                                               
       ++INCLUDE ACLFM089A                                                      
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMF7D                                                       
SAVECOMP DS    CL(ACMPLNQ)         SAVED AREA FOR COMPANY ELEMENT               
SAVEHEIR DS    CL(ACHRLENQ)        SAVED ARE FOR HIERARCHY ELEMENT              
THISPROF DS    CL255                                                            
THISADD  DS    CL105                                                            
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
SAVEADDR DS    CL107                                                            
SAVEPROF DS    CL255                                                            
SAVELEM  DS    CL255                                                            
         EJECT                                                                  
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENZEN                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051ACLFM08   05/01/02'                                      
         END                                                                    
