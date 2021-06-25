*          DATA SET NEPUP05    AT LEVEL 004 AS OF 05/01/02                      
*          DATA SET NEPUP05    AT LEVEL 009 AS OF 07/02/87                      
*PHASE T32205A,*                                                                
         TITLE 'T32205 - PLAN COPY'                                             
T32205   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32205**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   VAL2                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
VAL2     CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FOR COPY                                            
         SPACE 3                                                                
VKEY     NTR1                                                                   
         SPACE 1                                                                
         LA    R2,PUPCLIH          CLIENT (REQUIRED)                            
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE PLANS                                                   
         SPACE 3                                                                
VREC     NTR1                                                                   
         LA    R2,PUPFROMH         FROM PLAN                                    
         GOTO1 VVALPLAN                                                         
         MVC   FROMPLAN,PLANCODE                                                
         OI    PUPFRNMH+6,X'80'                                                 
         MVC   PUPFRNM,PLANNAME    SHOW FROM NAME                               
         LA    R4,KEY                                                           
         SPACE 1                                                                
         LA    R2,PUPTOH           TO PLAN                                      
         USING NPLKEY,R4                                                        
         GOTO1 ANY                                                              
         MVC   TOPLAN,WORK                                                      
         MVC   NPLKPLAN,TOPLAN                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    BADTO               NOT GOOD IF PLAN THERE ALREADY               
         MVC   KEY,KEYSAVE                                                      
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVC   NPLKEY,KEY                                                       
         LA    R2,PUPTONMH         OPTIONAL 'TO' NAME                           
         MVC   NPLNNAME,SPACES                                                  
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   NPLNNAME,PUPTONM                                                 
         OI    DMINBTS,X'08'       SEE IF PLAN IS THERE BUT DELETED             
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    PLAN2                                                            
         MVC   KEY(20),KEYSAVE                                                  
         GOTO1 ADDREC              NO - SO ADD A NEW PLAN                       
         B     COPY1                                                            
         SPACE 1                                                                
PLAN2    LA    R1,KEY              YES                                          
         AH    R1,LKEY                                                          
         NI    0(R1),X'7F'         RESTORE PLAN KEY                             
         GOTO1 WRITE                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              AND WRITE BACK NEW PLAN                      
         EJECT                                                                  
*              NOW DEAL WITH COPYING OVER THE PROGRAMS                          
         SPACE 3                                                                
COPY1    NI    DMINBTS,X'F7'                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPUKEY,R4                                                        
         MVI   NPUKTYPE,X'22'      FILL PROGRAM KEY                             
         MVC   NPUKAM,BINAGYMD                                                  
         MVC   NPUKCLT,CLTCOMP                                                  
         MVC   NPUKNET,NETWORK                                                  
         MVC   NPUKDPT,DPTCODE                                                  
         MVC   NPUKPLAN,FROMPLAN   (THIS IS STILL THE FROM PLAN)                
         GOTO1 HIGH                                                             
         SR    R5,R5               COUNT N'PROGRAMS IN R5                       
         B     COPY6                                                            
         SPACE 1                                                                
COPY4    LA    R4,KEY                                                           
         MVC   KEY,FROMKEY                                                      
         NI    DMINBTS,X'F7'                                                    
         GOTO1 HIGH                (RE-ESTABLISH SEQUENCE)                      
         GOTO1 SEQ                                                              
         SPACE 1                                                                
COPY6    CLC   KEY(13),KEYSAVE     MUST HAVE MATCH ON PLAN                      
         BNE   COPY20                                                           
         MVC   FROMKEY,KEY                                                      
         GOTO1 GETREC                                                           
         LA    R5,1(R5)                                                         
         LA    R4,KEY                                                           
         MVC   NPUKPLAN,TOPLAN     CHANGE TO 'TO' PLAN                          
         L     R4,AIO                                                           
         MVC   NPUKPLAN,TOPLAN                                                  
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE     SEE IF THIS RECORD EXISTS                    
         BE    COPY8                                                            
         GOTO1 ADDREC              NO - SO ADD A NEW ONE                        
         B     COPY4                                                            
         SPACE 1                                                                
COPY8    LA    R1,KEY              IS IT DELETED                                
         AH    R1,LKEY                                                          
         TM    0(R1),X'80'                                                      
         BNO   COPY10                                                           
         NI    0(R1),X'7F'         YES - SO RESTORE KEY                         
         GOTO1 WRITE                                                            
         SPACE 1                                                                
COPY10   MVC   AIO,AIO2            RECORD EXISTS                                
         GOTO1 GETREC              GET OLD RECORD INTO IO2                      
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              AND REWRITE NEW FROM IO1                     
         B     COPY4                                                            
         DROP  R4                                                               
         SPACE 1                                                                
*                                                                               
*              NOW DEAL WITH COPYING OVER THE QPROGS                            
*                                                                               
COPY20   NI    DMINBTS,X'F7'                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPCRECD,R4                                                       
         MVI   NPCKTYPE,X'28'      FILL QPROG KEY                               
         MVC   NPCKAM,BINAGYMD                                                  
         MVC   NPCKCLT,CLTCOMP                                                  
         MVC   NPCKNET,NETWORK                                                  
         MVC   NPCKDPT,DPTCODE                                                  
         MVC   NPCKPLAN,FROMPLAN   (THIS IS STILL THE FROM PLAN)                
         GOTO1 HIGH                                                             
         SPACE 1                                                                
COPY26   CLC   KEY(13),KEYSAVE     MUST HAVE MATCH ON PLAN                      
         BNE   COPYEND                                                          
         MVC   FROMKEY,KEY                                                      
         GOTO1 GETREC                                                           
         LA    R4,KEY                                                           
         MVC   NPCKPLAN,TOPLAN     CHANGE TO 'TO' PLAN                          
         L     R4,AIO                                                           
         MVC   NPCKPLAN,TOPLAN                                                  
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE     SEE IF THIS RECORD EXISTS                    
         BE    COPY28                                                           
         GOTO1 ADDREC              NO - SO ADD A NEW ONE                        
         B     COPYEND                                                          
         SPACE 1                                                                
COPY28   LA    R1,KEY              IS IT DELETED                                
         AH    R1,LKEY                                                          
         TM    0(R1),X'80'                                                      
         BNO   COPY30                                                           
         NI    0(R1),X'7F'         YES - SO RESTORE KEY                         
         GOTO1 WRITE                                                            
         SPACE 1                                                                
COPY30   MVC   AIO,AIO2            RECORD EXISTS                                
         GOTO1 GETREC              GET OLD RECORD INTO IO2                      
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              AND REWRITE NEW FROM IO1                     
         B     COPYEND                                                          
         SPACE 3                                                                
COPYEND  MVC   CONHEAD(L'GOODMSG),GOODMSG                                       
         EDIT  (R5),(3,CONHEAD)                                                 
         GOTO1 SQUASHER,DMCB,CONHEAD,60                                         
         LA    R2,PUPCLIH                                                       
         B     MYEND                                                            
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
BADTO    MVC   CONHEAD(L'TOERR),TOERR                                           
         B     MYEND                                                            
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
TOERR    DC    C'** ERROR ** TO PLAN ALREADY EXISTS'                            
GOODMSG  DC    C'999 PROGRAMS ADDED TO NEW PLAN'                                
         SPACE 1                                                                
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPF5D                                                       
         SPACE 1                                                                
         DS    0D                                                               
FROMPLAN DS    CL4                                                              
TOPLAN   DS    CL4                                                              
FROMKEY  DS    CL32                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEPUP05   05/01/02'                                      
         END                                                                    
