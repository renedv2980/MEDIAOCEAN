*          DATA SET CTLFM1F    AT LEVEL 032 AS OF 02/01/88                      
*PHASE TA021FA,+0                                                               
         TITLE 'CTLFM1F - CONTROL FILE MAINT - STATION TWX RECS'                
*                                                                               
*  LEV 23-24 JUN23/87 ALLOW FOR GRAPHNET PRINTER NUMBER (6 DIGITS)              
*  LEV 25-27 SEP04/87 ADD TELEPHONE NUMBER AND ACTIVITY DATE                    
*  LEV 28    JAN22/88 FIX DDS CODE - WAS SPACES, MADE BI ZERO                   
*  LEV 29-32 JAN27/88 ADD COMML TYPE TO RECORD                                  
*                                                                               
TA021F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,CTLFM1F*                                                     
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CSTREC,R4           R4=A(STATION TWX RECORD)                     
         EJECT                                                                  
KEYVAL   XC    CSTKEY,CSTKEY       INITIALISE KEY                               
         MVI   CSTKEY,C'6'                                                      
         SPACE 2                                                                
KEYV10   LA    R1,TWXSTAH          VALIDATE STATION                             
         GOTO1 AFVAL                                                            
         BZ    EMIF                MISSING STATION                              
         CLI   FLDH+5,5                                                         
         BE    KEYV20              MUST BE 5 OR                                 
         CLI   FLDH+5,6                                                         
         BNE   STAERR              6 CHARS                                      
         CLI   FLD+4,C'-'                                                       
         BNE   STAERR                                                           
         MVC   CSTKSTA(4),FLD                                                   
         MVC   CSTKSTA+4(1),FLD+5                                               
         B     KEYV30                                                           
         SPACE 2                                                                
KEYV20   CLI   FLD+3,C'-'                                                       
         BNE   STAERR                                                           
         MVC   CSTKSTA(3),FLD                                                   
         MVI   CSTKSTA+3,C' '                                                   
         MVC   CSTKSTA+4(1),FLD+4                                               
KEYV30   CLI   CSTKSTA+4,C'T'                                                   
         BE    KEYV40                                                           
         CLI   CSTKSTA+4,C'F'                                                   
         BE    KEYV40                                                           
         CLI   CSTKSTA+4,C'A'                                                   
         BNE   MEDERR                                                           
         SPACE 2                                                                
KEYV40   MVC   KEY,CSTKEY          CHECK KEY COMPATIBLE WITH ACTION             
         MVC   KEYNEXT,KEY                                                      
         LA    R1,TWXSTAH                                                       
         ST    R1,FADR                                                          
         CLI   ACTN,2              CANT CHANGE KEY ON CHANGE ACTION             
         BNE   KEYV44                                                           
         CLC   KEY,LKEY                                                         
         BE    KEYV44                                                           
         MVI   ACTN,3              SET ACTN TO DISPLAY ON KEY CHANGE            
*                                                                               
KEYV44   DS    0H                                                               
         CLI   ACTN,3                                                           
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD               READ STATION TWX RECORD                      
         BZ    EIIO                DISK ERROR                                   
         TM    DMCB+8,X'10'        TEST FOR NOT FOUND                           
         BZ    KEYV50                                                           
         CLI   ACTN,1              ONLY VALID FOR ADD                           
         BE    DATAVAL                                                          
         B     ERNF                                                             
KEYV50   CLI   ACTN,1              CANT EXIST FOR ADD                           
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,3              A DELETED RECORD CAN ONLY BE DISP            
         BNE   ERNF                                                             
         CLI   ACTN,2                                                           
         BE    DATAVAL                                                          
         EJECT                                                                  
DISPLAY  LA    R5,CSTDATA          DISPLAY RECORD                               
         SR    R1,R1                                                            
         XC    TWXAD1,TWXAD1                                                    
         OI    TWXAD1H+6,X'80'                                                  
         XC    TWXAD2,TWXAD2                                                    
         OI    TWXAD2H+6,X'80'                                                  
         XC    TWXAD3,TWXAD3                                                    
         OI    TWXAD3H+6,X'80'                                                  
         XC    TWXAD4,TWXAD4                                                    
         OI    TWXAD4H+6,X'80'                                                  
         XC    TWXTEL,TWXTEL                                                    
         OI    TWXTELH+6,X'80'                                                  
         XC    TWXTYP,TWXTYP                                                    
         OI    TWXTYPH+6,X'80'                                                  
         XC    TWXDAT,TWXDAT                                                    
         OI    TWXDATH+6,X'80'                                                  
         SPACE 2                                                                
DISP10   CLI   0(R5),0             FIND LINE LIST ELEMENT                       
         BNE   DISP14                                                           
DISP12   LA    R5,CSTDATA                                                       
         SR    R1,R1                                                            
         B     DISPX                                                            
DISP14   IC    R1,1(R5)                                                         
         CLI   0(R5),X'10'         TWX NUMBER DATA IS X'10' EL                  
         BNE   DISP20                                                           
         XC    TWXTWX,TWXTWX                                                    
         MVC   TWXTWX(20),2(R5)                                                 
         OI    TWXTWXH+6,X'80'                                                  
         XC    TWXTWXA,TWXTWXA                                                  
         MVC   TWXTWXA(20),22(R5)                                               
         OI    TWXTWXAH+6,X'80'                                                 
         XC    TWXDDST,TWXDDST                                                  
         MVC   TWXDDST(4),42(R5)                                                
         OI    TWXDDSTH+6,X'80'                                                 
         AR    R5,R1                                                            
         B     DISP10                                                           
*                                                                               
DISP20   CLI   0(R5),X'21'         STATION ADDRESS LINES 1                      
         BNE   DISP22                                                           
         MVC   TWXAD1(24),2(R5)                                                 
         AR    R5,R1                                                            
         B     DISP10                                                           
DISP22   CLI   0(R5),X'22'         STATION ADDRESS LINES 2                      
         BNE   DISP23                                                           
         MVC   TWXAD2(24),2(R5)                                                 
         AR    R5,R1                                                            
         B     DISP10                                                           
DISP23   CLI   0(R5),X'23'         STATION ADDRESS LINES 3                      
         BNE   DISP24                                                           
         MVC   TWXAD3(24),2(R5)                                                 
         AR    R5,R1                                                            
         B     DISP10                                                           
DISP24   CLI   0(R5),X'24'         STATION ADDRESS LINES 4                      
         BNE   DISP30                                                           
         MVC   TWXAD4(24),2(R5)                                                 
         AR    R5,R1                                                            
         B     DISP10                                                           
DISP30   CLI   0(R5),X'30'         TELEPHONE NUMBER                             
         BNE   DISP40                                                           
         ZIC   RF,1(R5)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,DISP30M                                                       
         AR    R5,R1                                                            
         B     DISP10                                                           
DISP30M  MVC   TWXTEL(0),2(R5)                                                  
         SPACE                                                                  
DISP40   CLI   0(R5),X'40'         COMMERCIAL TYPE                              
         BNE   DISPF1                                                           
         MVC   TWXTYP(3),2(R5)                                                  
         AR    R5,R1                                                            
         B     DISP10                                                           
DISPF1   CLI   0(R5),X'F1'         STATION ADDRESS LINES 4                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATCON,DMCB,(3,2(R5)),(5,TWXDAT)                                
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP10                                                           
         SPACE 2                                                                
DISPX    TM    CSTSTAT,X'80'       TEST DELETED RECORD                          
         BO    DISPX1                                                           
         MVI   NACTN,X'03'         SET OK TO CHANGE/DELETE                      
         LA    R1,TWXTWXH                                                       
         ST    R1,FADR                                                          
         B     DISPXX                                                           
DISPX1   MVI   NACTN,X'04'         SET OK TO RESTORE                            
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
DISPXX   B     EXIT                                                             
         EJECT                                                                  
DATAVAL  XC    TEMP,TEMP           INITIALIZE RECORD                            
         GOTO1 ABLDREC                                                          
         SPACE 2                                                                
VALD02   DS   0H                                                                
         LA    R1,TWXTWXH                                                       
         ST    R1,FADR                                                          
         CLI   TWXTWXH+5,0         VALIDATE TWX NUMBER                          
         BE    EMIF                                                             
         MVC   WORK(20),=20C'0'                                                 
         MVN   WORK(20),TWXTWX                                                  
         ZIC   R1,TWXTWXH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),TWXTWX                                                   
         BNE   TWXERRN                                                          
         CLI   TWXTWX,C'5'         MUST HAVE TWX ID                             
         BE    VALD04                                                           
         CLI   TWXTWXH+5,7         TELEX NUMBER                                 
         BNE   *+14                                                             
         CLC   =C'3724300',TWXTWX  DDS GRAPHNET PRINTER ID                      
         BE    VALD06                                                           
         CLI   TWXTWX,C'0'         MUST HAVE TELEX ID                           
         BNE   TWXERRID                                                         
         CLI   TWXTWXH+5,9         TELEX NUMBER                                 
         BH    TWXERRTL                                                         
         CLI   TWXTWXH+5,7         TELEX NUMBER                                 
         BL    TWXERRTL                                                         
         B     VALD06                                                           
VALD04   CLI   TWXTWXH+5,11        VALIDATE TWX NUMBER                          
         BNE   TWXERRTW                                                         
VALD06   MVI   TEMP,X'10'                                                       
         MVI   TEMP+1,46                                                        
         MVC   TEMP+2(20),TWXTWX                                                
         SPACE 2                                                                
VALD10   LA    R1,TWXTWXAH         VALIDATE TWX ANSWERBACK                      
         GOTO1 AFVAL                                                            
         BZ    EMIF                MISSING TWX ANSWERBACK                       
         MVC   TEMP+2+20(20),FLD                                                
         SPACE 2                                                                
VALDA    LA    R1,TWXDDSTH         VALIDATE DDS CODE                            
         GOTO1 AFVAL                                                            
         CLI   TWXDDSTH+5,0        WAS DDS CODE ENTERED                         
         BNE   DDSCERR                                                          
         MVI   TEMP,X'10'                                                       
         MVI   TEMP+1,46                                                        
         MVC   TEMP+2+20+20(4),TWXDDST                                          
         GOTO1 ABLDREC             ADD TWX NUMBER ELEMENT                       
         SPACE 2                                                                
VALDB    LA    R1,TWXAD1H          VALIDATE ADDRESS LINE 1                      
         GOTO1 AFVAL                                                            
         BZ    EMIF                                                             
         MVI   TEMP,X'21'                                                       
         MVI   TEMP+1,26                                                        
         MVC   TEMP+2(24),FLD                                                   
         GOTO1 ABLDREC             ADD ADDRESS LINE 1 ELEMENT                   
         SPACE 2                                                                
VALDC    LA    R1,TWXAD2H          VALIDATE ADDRESS LINE 2                      
         GOTO1 AFVAL                                                            
         BZ    EMIF                                                             
         MVI   TEMP,X'22'                                                       
         MVI   TEMP+1,26                                                        
         MVC   TEMP+2(24),FLD                                                   
         GOTO1 ABLDREC             ADD ADDRESS LINE 2 ELEMENT                   
         SPACE 2                                                                
VALDD    LA    R1,TWXAD3H          VALIDATE ADDRESS LINE 3                      
         GOTO1 AFVAL                                                            
         BZ    EMIF                                                             
         MVI   TEMP,X'23'                                                       
         MVI   TEMP+1,26                                                        
         MVC   TEMP+2(24),FLD                                                   
         GOTO1 ABLDREC             ADD ADDRESS LINE 3 ELEMENT                   
         SPACE 2                                                                
VALDE    LA    R1,TWXAD4H          VALIDATE ADDRESS LINE 4                      
         GOTO1 AFVAL                                                            
         BZ    VALDF                                                            
         MVI   TEMP,X'24'                                                       
         MVI   TEMP+1,26                                                        
         MVC   TEMP+2(24),FLD                                                   
         GOTO1 ABLDREC             ADD ADDRESS LINE 4 ELEMENT                   
         EJECT                                                                  
VALDF    LA    R1,TWXTELH          VALIDATE TELEPHONE                           
         GOTO1 AFVAL                                                            
         BZ    VALDG                                                            
         CLI   TWXTELH+5,13        TEL MUST BE (123)123-1234                    
         BL    TELERR                                                           
         CLI   FLD,C'('                                                         
         BNE   TELERR                                                           
         CLI   FLD+4,C')'                                                       
         BNE   TELERR                                                           
         CLI   FLD+8,C'-'                                                       
         BNE   TELERR                                                           
         MVC   WORK(4),=C'0000'                                                 
         MVN   WORK(3),FLD+1                                                    
         CLC   WORK(3),FLD+1                                                    
         BNE   TELERR                                                           
         MVC   WORK(4),=C'0000'                                                 
         MVN   WORK(3),FLD+5                                                    
         CLC   WORK(3),FLD+5                                                    
         BNE   TELERR                                                           
         MVC   WORK(4),=C'0000'                                                 
         MVN   WORK(3),FLD+9                                                    
         CLC   WORK(3),FLD+9                                                    
         BNE   TELERR                                                           
         MVI   TEMP,X'30'                                                       
         MVI   TEMP+1,L'TWXTEL+2                                                
         MVC   TEMP+2(18),FLD                                                   
         GOTO1 ABLDREC             ADD TELEPHONE ELEMENT                        
         SPACE                                                                  
VALDG    LA    R1,TWXTYPH          VALIDATE COMMERCIAL TYPE                     
         GOTO1 AFVAL                                                            
         BZ    VALDH                                                            
         LA    R0,CTYPTBCT                                                      
         LA    R1,CTYPTABL                                                      
         CLC   FLD(3),0(R1)                                                     
         BE    *+16                                                             
         LA    R1,3(,R1)                                                        
         BCT   R0,*-14                                                          
         B     TYPERR                                                           
         MVI   TEMP,X'40'                                                       
         MVI   TEMP+1,5                                                         
         MVC   TEMP+2(3),FLD                                                    
         GOTO1 ABLDREC             ADD COMMERCIAL TYPE ELEMENT                  
         SPACE                                                                  
VALDH    MVI   TEMP,X'F1'                                                       
         MVI   TEMP+1,5                                                         
         GOTO1 VDATCON,DMCB,(5,0),(3,TEMP+2)                                    
         GOTO1 ABLDREC             ADD ACTIVITY ELEMENT                         
         SPACE                                                                  
         LA    R1,TWXTWXH          POSN TO 1ST KEY FLD & SET OK                 
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         CLI   ACTN,1                                                           
         BNE   UPD1                                                             
         GOTO1 AADD                                                             
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     UPDX                                                             
         SPACE 2                                                                
UPD1     GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
UPDX     MVI   NACTN,X'03'         SET OK TO CHANGE/DELETE                      
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
TWXERRN  MVC   BASHDR,=CL60'* ERROR * TWX MUST BE NUMERIC *'                    
         LA    R1,TWXTWXH                                                       
         B     COMERR                                                           
TELERR   MVC   BASHDR,=CL60'* ERROR * TEL MUST BE (999)999-999 *'               
         LA    R1,TWXTELH                                                       
         B     COMERR                                                           
TYPERR   MVC   BASHDR,=CL60'* ERROR * INVALID COMMERCIAL TYPE *'                
         LA    R1,TWXTYPH                                                       
         B     COMERR                                                           
TWXERRID MVC   BASHDR,=CL60'* ERROR * TWX MUST BE PRECEDED BY 5, TELEX C        
               BY 0 *'                                                          
         LA    R1,TWXTWXH                                                       
         B     COMERR                                                           
TWXERRTW MVC   BASHDR,=CL60'* ERROR * TWX MUST BE 10 DIGITS PRECEDED BYC        
                5 *'                                                            
         LA    R1,TWXTWXH                                                       
         B     COMERR                                                           
TWXERRTL MVC   BASHDR,=CL60'* ERROR * TELEX MUST BE 6-8 DIGITS PRECEDEDC        
                BY 0 *'                                                         
         LA    R1,TWXTWXH                                                       
COMERR   OI    BASHDRH+6,X'80'                                                  
         MVI   FERN,X'FE'                                                       
         ST    R1,FADR                                                          
         B     EXIT                                                             
STAERR   MVI   FERN,X'FE'                                                       
         MVC   BASHDR,=CL60'* ERROR * STATION MUST BE ENTERED XXX-M OR C        
               XXXX-M *'                                                        
         LA    R1,TWXSTAH                                                       
         B     COMERR                                                           
MEDERR   MVI   FERN,X'FE'                                                       
         MVC   BASHDR,=CL60'* ERROR * MEDIA MUST BE T, A, OR F *'               
         LA    R1,TWXSTAH                                                       
         B     COMERR                                                           
DDSCERR  MVI   FERN,X'FE'                                                       
         MVC   BASHDR,=CL60'* ERROR * DDS CODE IS FOR FUTURE USE *'             
         LA    R1,TWXDDSTH                                                      
         B     COMERR                                                           
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRCMLTYP                                                     
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMFE0                                                                      
       ++INCLUDE CTLFME0D                                                       
         SPACE                                                                  
* FASYSLSTD                                                                     
*        PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032CTLFM1F   02/01/88'                                      
         END                                                                    
