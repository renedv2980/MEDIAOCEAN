*          DATA SET CTREPSAVE  AT LEVEL 067 AS OF 08/17/00                      
*          DATA SET CTREP9902 AT LEVEL 065 AS OF 10/26/89                       
*PHASE CT9902A                                                                  
         TITLE 'ID REPORT'                                                      
CT9902   CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8000X                                                            
         ORG   *-8000                                                           
*                                                                               
         NMOD1 0,**SALLY*                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
*                                                                               
         MVC   MID1(110),=CL110'AGENCY  SYSTEM  PROGRAM  PHASE  NAME   X        
                      DESCRIPTION                   FILTER    DATE'             
         MVC   MID2(110),=CL110'------  ------  -------  -----  ----   X        
                      -----------                   ------    ----'             
         LA    R4,P                                                             
         USING PRNTD,R4                                                         
*                                                                               
         XC    KEY,KEY             CLEAR OUT KEY                                
         LA    R3,KEY                                                           
         USING CT01RECD,R3         COVER KEY W/ KEY DSECT                       
         MVI   CT01TYPE,CT01TYPQ                                                
         MVC   CT01AGID,=CL2'SJ'                                                
         MVI   CT01SYS,X'05'                                                    
         MVI   CT01PRG,X'03'                                                    
         MVI   CT01PHAS,X'02'                                                   
         MVC   KEYSAVE(25),KEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,KEY,IO1                               
         CLI   8(R1),0             CHECK RETURN CODE                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R3,IO1              NOW COVER BUFFER W/ KEY DSECT                
*                                                                               
* PROCESS A RECORD                                                              
*                                                                               
NXTREC   CLC   IO1(17),KEYSAVE     FINISHED W/ DESIRED KEY?                     
         BNE   XIT                                                              
         MVC   PAGID,CT01AGID      MOVE IN KEY FIELDS TO BE PRINTED             
         EDIT  (B1,CT01SYS),(2,PSYSTM)                                          
         EDIT  (B1,CT01PRG),(2,PPROG)                                           
         EDIT  (B1,CT01PHAS),(2,PPHASE)                                         
         MVC   PNAME,CT01NAME                                                   
*                                                                               
* PROCESS ELEMENT OF RECORD                                                     
         LA    R6,IO1              R6 WILL BUMP PAST ELEMENTS                   
* DESCRIPTION ELEMENT                                                           
         LA    R6,28(R6)           BUMP PAST KEY,REC LENGTH & STATUS            
         USING CT01DSCD,R6         COVER W/ DESCRIPTION DSECT                   
         ZIC   R9,CT01DLEN         LENGTH OF DESCRIPTION ELEMENT                
         SH    R9,=H'2'            ADJUST FOR CODE AND LENGTH                   
         BCTR  R9,0                ADJUST LENGHT FOR EX INSTR.                  
         EX    R9,*+8              MOVE DESCRIPTION INTO PRINT DSECT            
         B     *+10                                                             
         MVC   PDESC(0),CT01DESC                                                
         LR    R2,R6               SAVE PTR TO 1ST 02 ELEMENT                   
         ZIC   R5,CT01DLEN                                                      
         AR    R2,R5                                                            
* CHECK FOR FILTER ELEMENT                                                      
NXTEL    ZIC   R5,1(R6)                                                         
         AR    R6,R5               BUMP TO NEXT ELEMENT                         
         CLI   0(R6),CT01LCDQ                                                   
         BH    NOFILT                                                           
         BL    NXTEL                                                            
         USING CT01FLTD,R6         COVER W/ FILTER DSECT                        
         MVC   PFILT,CT01FILT      MOVE IN FILTER                               
         ZIC   R5,CT01LLEN                                                      
         AR    R6,R5               BUMP TO NEXT ELEMENT                         
         B     *+10                                                             
NOFILT   MVC   PFILT,=CL4'----'                                                 
*DATE ELEMENT                                                                   
         USING ACTVD,R6                                                         
NXTEL2   CLI   0(R6),X'F1'                                                      
         BNL   DATE                                                             
         ZIC   R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     NXTEL2                                                           
DATE     GOTO1 DATCON,DMCB,(3,ACTVCHDT),(8,PDATE)                               
         GOTO1 REPORT                                                           
* FIELD I.D. & TEXT ELEMENT                                                     
         USING CT01FLDD,R2         COVER W/ FIELD DSECT                         
         LA    R7,P                                                             
         USING PRNTD2,R7                                                        
MORE02   ZIC   R9,CT01FLEN         LENGTH OF FIELD TEXT                         
         SH    R9,=H'4'            ADJUST FOR CODE,LENGTH,ID,SEQUENCE           
         BCTR  R9,0                ADJUST FOR EX INSTR.                         
         EDIT  (B1,CT01ID),(3,PFID)                                             
         EX    R9,*+8              MOVE IN FIELD TEXT                           
         B     *+10                                                             
         MVC   PFTXT(0),CT01TEXT                                                
         GOTO1 REPORT                                                           
         ZIC   R5,CT01FLEN         BUMP TO NEXT ELEMENT                         
         AR    R2,R5                                                            
         CLI   0(R2),CT01FCDQ      CHECK IF 02 ELEMENT                          
         BE    MORE02                                                           
         GOTO1 DATAMGR,DMCB,DMRSEQ,CTFILE,KEY,IO1                               
         CLI   8(R1),0            CHECK RETURN CODE                             
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     NXTREC                                                           
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'***BUFFER***'                                                  
IO1      DS    2000C               READ CTFILE RECORDS HERE                     
         SPACE 3                                                                
PRNTD    DSECT                                                                  
PAGID    DS    CL2                                                              
         DS    CL6                                                              
PSYSTM   DS    CL2                                                              
         DS    CL6                                                              
PPROG    DS    CL2                                                              
         DS    CL7                                                              
PPHASE   DS    CL2                                                              
         DS    CL5                                                              
PNAME    DS    CL8                                                              
         DS    CL6                                                              
PDESC    DS    CL28                                                             
         DS    CL2                                                              
PFILT    DS    CL4                                                              
         DS    CL6                                                              
PDATE    DS    CL8                                                              
PRNTD2   DSECT                                                                  
         DS    CL5                                                              
PFID     DS    CL3                                                              
         DS    CL2                                                              
PFTXT    DS    CL4                                                              
         EJECT                                                                  
       ++INCLUDE CTGENPGREC                                                     
         EJECT                                                                  
       ++INCLUDE CTREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE CTREPWORKD                                                     
         SPACE 3                                                                
       ++INCLUDE DDACTIVD                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067CTREPSAVE 08/17/00'                                      
         END                                                                    
