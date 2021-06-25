*          DATA SET CTLFM0A    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TA020AA                                                                  
         TITLE 'CTLFM0A - CONTROL FILE MAINT - IDATTN RECORD'                   
CTLFM0A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 252,**LFMA**                                                     
         USING WRKD,RC             RC=A(TEMP W/S)                               
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTIREC,R4           R4=A(IOAREA)                                 
         EJECT                                                                  
*              KEY VALIDATION                                                   
*                                                                               
KEYVAL   XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         LA    R1,IDAIDH           VALIDATE ID                                  
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         CLI   FLDH+5,3                                                         
         BL    EFTS                                                             
         MVC   CTIKID,FLD          MOVE ID TO KEY                               
         MVC   KEY,CTIKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         CLI   ACTN,CHANGE                                                      
         BNE   KEYV2                                                            
         CLC   KEY,LKEY            IF KEY CHANGED SET ACTION TO DISP            
         BE    KEYV2                                                            
         MVI   ACTN,DISPLAY                                                     
*                                                                               
KEYV2    CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'                                                     
         BO    ERNF                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY                                                     
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    CHAREC                                                           
         CLI   ACTN,DISPLAY                                                     
         BE    DISREC                                                           
         B     EIAS                                                             
         EJECT                                                                  
*              DISPLAY ID ATTENTION DETAILS                                     
*                                                                               
DISREC   LA    R1,IDAATTAH                                                      
         BAS   RE,CLEAR            CLEAR UNPROTS IN TWA                         
         LA    R5,IDAATTAH                                                      
         USING LINED,R5            R5=A(TWA LINE)                               
         LA    R6,CTIDATA          R6=A(FIRST ELEMENT)                          
*                                                                               
DISR2    CLI   0(R6),0             END OF RECORD                                
         BE    DISEND                                                           
         CLI   0(R6),X'31'         ATTENTION DETAIL                             
         BE    DISR6                                                            
*                                                                               
DISR4    SR    R1,R1               BUMP TO NEXT ELEMENT                         
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISR2                                                            
*                                                                               
DISR6    DS    0H                  DISPLAY ATTN DETAIL ELEMENT                  
         USING CTATTND,R6                                                       
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         SH    R1,=H'6'            R1=L'NAME                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINAME(0),CTATTDET                                               
         MVC   LITYPE,CTATTTYP                                                  
         LA    R5,LINEXT(R5)                                                    
         B     DISR4                                                            
         DROP  R6                                                               
*                                                                               
DISEND   TM    CTISTAT,X'80'       SET NEXT ACTIONS ETC.                        
         BO    DISE2                                                            
         LA    R1,IDAATTAH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKDEL+OKCHA                                                
         B     DISEX                                                            
*                                                                               
DISE2    MVI   NACTN,OKRES                                                      
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
*                                                                               
DISEX    MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*              CHANGE ID ATTENTION DETAILS                                      
*                                                                               
CHAREC   MVI   TEMP,X'01'          STRIP RECORD OF AMENDABLE ELEMENTS           
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'31'                                                       
         GOTO1 ADELEL                                                           
         LA    R6,CTIDATA                                                       
         SR    R1,R1               LOCATE PASSIVE ID# ELEMENT                   
*                                                                               
CHAR2    CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R6),X'02'                                                      
         BE    *+14                                                             
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CHAR2                                                            
         MVC   IDNUM,2(R6)         SAVE ACTIVE/PASSIVE ID POINTERS              
         MVC   IDALP,FLD                                                        
*                                                                               
         LA    R5,IDAATTAH         R5=A(FIRST TWA INPUT FIELD)                  
         USING LINED,R5                                                         
*                                                                               
CHAR4    CLI   0(R5),10            END OF TWA                                   
         BL    CHAREND                                                          
         LA    R1,LITYPEH                                                       
         GOTO1 AFVAL                                                            
         BNZ   *+16                                                             
         CLI   LINAMEH+5,0                                                      
         BNE   EMIF                                                             
         B     CHARA                                                            
         MVC   WORK(3),FLD                                                      
         LA    R1,LINAMEH                                                       
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         LA    R1,LITYPEH                                                       
         ST    R1,FADR                                                          
         XC    TEMP,TEMP           BUILD ATTENTION DETAIL ELEMENT               
         MVC   TEMP(2),=X'3105'                                                 
         LA    R7,TEMP                                                          
         USING CTATTND,R7                                                       
         MVC   CTATTTYP,WORK                                                    
         MVC   CTATTDET,FLD                                                     
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         LA    R1,5(R1)                                                         
         STC   R1,CTATTLEN                                                      
*                                                                               
         LA    R6,CTIDATA          SEE IF DUPLICATE ATTN TYPE ON REC            
         SR    R1,R1                                                            
*                                                                               
CHAR6    CLI   0(R6),0                                                          
         BE    CHAR8                                                            
         CLI   0(R6),X'31'                                                      
         BNE   *+14                                                             
         CLC   2(3,R6),CTATTTYP                                                 
         BE    EDIF                DUPLICATE INPUT FIELD (TYPE)                 
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CHAR6                                                            
         DROP  R7                                                               
*                                                                               
CHAR8    GOTO1 APUTEL              ADD ELEMENT                                  
         BZ    EXIT                                                             
*                                                                               
CHARA    LA    R5,LINEXT(R5)       BUMP TO NEXT TWA LINE                        
         B     CHAR4                                                            
         DROP  R5                                                               
*                                                                               
CHAREND  LA    R1,IDAIDH           POSITION CURSOR TO KEY FIELD                 
         ST    R1,FADR                                                          
         GOTO1 ABLDACT             BUILD & ADD ACTIVITY ELEMENT                 
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         GOTO1 AWRITE              WRITE UPDATED ID RECORD                      
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   TEMP,X'02'          DELETE POINTER ELEMENT                       
         GOTO1 ADELEL                                                           
         MVC   TEMP(2),=X'020C'    AND BUILD ALPHA POINTER & ADD IT             
         MVC   TEMP+2(10),IDALP                                                 
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         XC    CTIKEY,CTIKEY       BUILD POINTER KEY                            
         MVI   CTIKEY,C'I'                                                      
         MVC   CTILEN-2(2),IDNUM                                                
         MVC   KEY,CTIKEY                                                       
         LA    R5,REC              DO RFU ON PASSIVE ID RECORD                  
         ST    R5,AREC                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         ST    R4,AREC                                                          
         GOTO1 AWRITE              AND WRITE UPDATED POINTER REC                
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   FERN,X'FF'                                                       
         MVI   NACTN,OKCHA+OKDEL   SET NEXT ACTION & RETURN                     
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMCODE                                                                     
       ++INCLUDE CTLFMCODE                                                      
         EJECT                                                                  
         LTORG                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WRKD     DSECT                                                                  
IDALP    DS    CL10                                                             
IDNUM    DS    CL2                                                              
REC      DS    1000C                                                            
*                                                                               
*              DSECT TO COVER TWA LINE                                          
*                                                                               
LINED    DSECT                                                                  
LITYPEH  DS    CL8                                                              
LITYPE   DS    CL3                                                              
LINAMEH  DS    CL8                                                              
LINAME   DS    CL33                                                             
LINEXT   EQU   *-LINED                                                          
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMF5D                                                                      
       ++INCLUDE CTLFMF5D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTLFM0A   05/01/02'                                      
         END                                                                    
