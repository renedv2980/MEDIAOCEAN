*          DATA SET CTLFM03    AT LEVEL 002 AS OF 08/22/00                      
*PHASE TA0203A                                                                  
         TITLE 'CTLFM03 - CONTROL FILE MAINT - ERROR MESSAGES'                  
CTLFM03  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,**LFM3**                                                     
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTEREC,R4           R4=A(ERROR RECORD)                           
         EJECT                                                                  
KEYVAL   XC    CTEKEY,CTEKEY                                                    
         MVI   CTEKEY,C'E'                                                      
         SPACE 2                                                                
         LA    R1,ERRSYSAH         VALIDATE SYSTEM NAME                         
         GOTO1 AFVAL                                                            
         BZ    EXIT                MISSING SYSTEM NAME                          
         TM    FLDH+4,X'08'        IF INPUT NUMERIC MOVE TO KEY                 
         BZ    KEYV0                                                            
         OC    FLDH(3),FLDH                                                     
         BNZ   EFTB                                                             
         MVC   CTEKSYS,FLDH+3                                                   
         B     KEYV3                                                            
KEYV0    DS    0H                                                               
         CLI   FLDH+5,3                                                         
         BL    EFTS                                                             
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=C'SYSTEM'   SPECIAL SYSTEM                               
         BNE   *+12                                                             
         MVI   CTEKSYS,0           SET SYS NUM TO ZERO                          
         B     KEYV3                                                            
         L     RE,ASYSTBL                                                       
         USING SYSLSTD,RE                                                       
KEYV1    CLI   SYSLNUM,0           SEARCH SYSTEM TABLE                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),SYSLNAME                                                  
         BE    KEYV2                                                            
         LA    RE,SYSLLEN(RE)                                                   
         B     KEYV1                                                            
KEYV2    MVC   CTEKSYS,SYSLEQU     SYS NUM TO KEY                               
         DROP  RE                                                               
         SPACE 2                                                                
KEYV3    LA    R1,ERRNUMH          VALIDATE ERROR NUMBER                        
         GOTO1 AFVAL                                                            
         BZ    EXIT                MISSING ERROR NUMBER                         
         TM    FLDH+4,X'08'                                                     
         BZ    EFNN                NOT NUMERIC                                  
         CLC   FLDH(4),=F'255'                                                  
         BH    EIIF                                                             
         MVC   CTEKNUM,FLDH+3      ERR NUM TO KEY                               
         SPACE 2                                                                
KEYV4    MVC   KEY,CTEKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,ERRSYSAH                                                      
         ST    R1,FADR                                                          
         CLI   ACTN,2              CANT CHANGE KEY ON CHANGE ACTION             
         CLI   ACTN,2                                                           
         BNE   KEYV4A                                                           
         CLC   KEY,LKEY                                                         
         BE    KEYV4A                                                           
         MVI   ACTN,3              SET ACTN TO DISPLAY ON KEY CHANGE            
*                                                                               
KEYV4A   DS    0H                                                               
         CLI   ACTN,3                                                           
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD               READ ERROR RECORD                            
         BZ    EIIO                DISK ERROR                                   
         TM    DMCB+8,X'10'        TEST FOR NOT FOUND                           
         BZ    KEYV5                                                            
         CLI   ACTN,1              ONLY VALID FOR ADD                           
         BE    DATAVAL                                                          
         B     ERNF                                                             
KEYV5    CLI   ACTN,1              CANT EXIST FOR ADD                           
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,3              A DELETED RECORD CAN ONLY BE DISP            
         BNE   ERNF                                                             
         CLI   ACTN,2                                                           
         BE    DATAVAL                                                          
         EJECT                                                                  
DISPLAY  LA    R5,CTEDATA          DISPLAY RECORD                               
         SR    R1,R1                                                            
DISP1    CLI   0(R5),0             FIND DESCRIPTION ELEMENT                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'02'                                                      
         BE    DISP2                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP1                                                            
         SPACE 2                                                                
DISP2    IC    R1,1(R5)            R5=A(DESC EL)                                
         SH    R1,=H'3'                                                         
         XC    ERRMSG,ERRMSG                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ERRMSG(0),2(R5)                                                  
         LA    R1,1(R1)                                                         
         OI    ERRMSGH+6,X'80'                                                  
         STC   R1,ERRMSGH+5                                                     
         STC   R1,ERRMSGH+7                                                     
DISPX    TM    CTESTAT,X'80'                                                    
         BO    DISPX1                                                           
         MVI   NACTN,X'03'         SET OK TO CHANGE/DELETE                      
         LA    R1,ERRMSGH                                                       
         ST    R1,FADR                                                          
         B     DISPXX                                                           
DISPX1   MVI   NACTN,X'04'         SET OK TO RESTORE                            
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
DISPXX   B     EXIT                                                             
         EJECT                                                                  
DATAVAL  MVI   TEMP,0              INITIALIZE RECORD                            
         GOTO1 ABLDREC                                                          
         SPACE 2                                                                
         LA    R1,ERRMSGH          VALIDATE ERROR MESSAGE                       
         GOTO1 AFVAL                                                            
         BZ    EXIT                MISSING ERROR MESSAGE                        
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         LA    R1,2(R1)                                                         
         MVI   TEMP,X'02'                                                       
         STC   R1,TEMP+1                                                        
         MVC   TEMP+2(60),FLD                                                   
         GOTO1 ABLDREC             ADD ERROR MESSAGE ELEMENT                    
         EJECT                                                                  
         LA    R1,ERRSYSAH         POSN TO 1ST KEY FLD & SET OK                 
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
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMFCD                                                                      
       ++INCLUDE CTLFMFCD                                                       
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTLFM03   08/22/00'                                      
         END                                                                    
