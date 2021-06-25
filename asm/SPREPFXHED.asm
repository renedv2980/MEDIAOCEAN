*          DATA SET SPREPFXHED AT LEVEL 001 AS OF 08/12/97                      
*PHASE SPFX02W                                                                  
********************************************************************            
*        R3 --- ALWAYS POINTS TO KEY                                            
*        R4 --- ALWAYS POINTS TO KEY                                            
********************************************************************            
      TITLE 'SPF02  - PRINT OUT ALL INV TRACKS W/ NO HEADERS'                   
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    INVCHK                                                           
*                                                                               
EXIT     XIT1                                                                   
********************************************************************            
RELO     DC    A(0)                                                             
*==================================================================*            
*                START THE CHECK                                   *            
*==================================================================*            
INVCHK   DS    0H                                                               
         LA    R3,KEY              R3 ALWAYS POINTS TO KEY                      
         USING REINVREC,R3                                                      
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         XC    SVKEY,SVKEY                                                      
*                                                                               
         MVI   KEY,X'12'          INV RECORD                                    
         GOTO1 HIGH                                                             
         B     IC10                                                             
*                                                                               
IC05     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
IC10     DS    0H                                                               
         CLI   KEY,X'12'          STILL INV RECORD?                             
         BNE   IC100                                                            
*                                                                               
         CLI   RINVKSRC,0          INVENTORY HEADER?                            
         BNE   *+10                                                             
         MVC   SVKEY,KEY                                                        
*                                                                               
         OC    SVKEY,SVKEY         DID WE GET A HEADER YET?                     
         BZ    IC05                NO (ONLY UNTIL 1ST HEADER FOUND)             
*                                                                               
         CLI   RINVKSRC,0          CURRENT RECORD INV HEADER?                   
         BE    IC05                YES                                          
*                                                                               
         CLC   SVKEY+17(7),RINVKINV    SAME INV# AND DATE AS LAST HEAD?         
         BE    IC05                    YES - OK                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,RINVKEY,P,17,=C'TOG'                                 
         MVC   P+17(4),RINVKINV                                                 
         GOTO1 HEXOUT,DMCB,RINVKSTD,P+21,3,=C'TOG'                              
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
*                                                                               
         B     IC05                                                             
*                                                                               
IC100    EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         MVC   P(18),=C'# OF BAD RECORDS  '                                     
         GOTO1 REPORT                                                           
*                                                                               
ICEND    B     EXIT                                                             
         EJECT                                                                  
*==============================================================*                
COUNT    DS    F                                                                
SVKEY    DS    CL27                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPFXHED08/12/97'                                      
         END                                                                    
