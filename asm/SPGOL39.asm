*          DATA SET SPGOL39    AT LEVEL 070 AS OF 10/11/17                      
*PHASE T20239B                                                                  
         TITLE  'SPGOL39 - DDLINK UPLOAD INTERFACE'                             
T20239   CSECT                                                                  
         LKSVR TYPE=UR,BLOCKS=(LIOBSB1Q,T202FFD,LIOBSB2Q,GENOLD)                
         PRINT NOGEN                                                            
         NMOD1 0,**GL39**                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         L     R3,VTIA                                                          
         USING LIOBD,R3                                                         
*                                                                               
         L     RA,VTWA                                                          
         USING T202FFD,RA                                                       
*                                                                               
         BRAS  RE,LKINIT                                                        
         XC    LINNUM,LINNUM       SET CURRENT LINE = 0                         
*                                                                               
***********************************************************************         
* GET AN UPLOAD RECORD - HEADER FIELDS MOVED DIRECTLY TO TWA                    
* THEN FILL IN FIRST 13 GOAL ACTION FIELDS                                      
* CALL BASE TO PROCESS                                                          
* AND EXIT WHEN ALL RECORDS HAVE BEEN PROCESSED                                 
***********************************************************************         
                                                                                
*                                                                               
NXTFLD   BAS   RE,CLRSCR                                                        
         XC    LINNUM,LINNUM                                                    
         XC    FLDNUM,FLDNUM                                                    
         XC    GOLMSG,GOLMSG                                                    
         MVI   ERRAREA,0           CLEAR PREVIOUS ERROR                         
         NI    GOLMDH+4,X'DF'      INVALIDATE MEDIA/CLT/...                     
*                                                                               
         GOTOR LINKIO,DMCB,('LIOAGET',LIOBD)                                    
         JH    CLOSE               CLOSE AND RETURN TO SPGOL00                  
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,7,SVINDEX+1             GET ARRAY ADDRESS                     
         JZ    *+2                                                              
         CLI   LQ_TYPE-LQ_D(R4),LQ_TLSTQ  TEST VALID ARRAY                      
         JNE   *+2                                                              
         USING LQ_D,R4             MAP TO ARRAY                                 
         SR    R5,R5                                                            
         ICM   R5,3,LQ_VALUE       NUMBER OF ENTRIES IN ARRAY                   
         LA    R6,LQ_VALUE+2       POINT TO FIRST ENTRY                         
*                                                                               
NXTFLD00 LA    R2,GOLACT1H         POINT TO FIRST FLDHDR                        
         SR    R0,R0                                                            
*                                                                               
         USING MYARRAY,R6                                                       
*                                                                               
NXTFLD10 MVC   8(3,R2),MYACTN                                                   
         BAS   RE,SETIN                                                         
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO MARKET                              
         MVC   8(4,R2),MYMKT                                                    
         BAS   RE,SETIN                                                         
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               SKIP MARKET NAME                             
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO DAYPART                             
         MVC   8(1,R2),MYDPT                                                    
         MVC   9(3,R2),MYSLN                                                    
         BAS   RE,SETIN                                                         
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               DOLLARS                                      
         MVC   8(11,R2),MYDOLS                                                  
         BAS   RE,SETIN                                                         
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO POINTS                              
         MVC   8(11,R2),MYPTS                                                   
         BAS   RE,SETIN                                                         
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               PERIOD                                       
         MVC   8(13,R2),MYPERD                                                  
         BAS   RE,SETIN                                                         
*                                                                               
         AHI   R6,MYARRAYX-MYARRAY NEXT ARRAY ENTRY                             
         BCTR  R5,0                DECREMENT ARRAY COUNT                        
         LTR   R5,R5                                                            
         JZ    NXTFLDX                                                          
*                                                                               
         LLC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RE,GOLLAST                                                       
         CR    R2,RE                                                            
         JL    NXTFLD10                                                         
*                                                                               
NXTFLDX  BAS   RE,GOBASE                                                        
         JNE   NXTFLD              AFTER ERROR, GET NEXT RECOD                  
         LTR   R5,R5               TEST ANY MORE ARRAY ENTRIES                  
         JNP   NOMORE              NO                                           
*                                                                               
         BAS   RE,CLRSCR           CLEAR SCREEN AND CONTINUE                    
         J     NXTFLD00                                                         
*                                                                               
NOMORE   DS    0H                                                               
         MVC   GOLMSG,SPACES                                                    
         MVC   GOLMSG+3(4),=C'0000'                                             
         MVC   GOLMSG+8(22),=C'** ACTION COMPLETED **'                          
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',I#SPGOLR)               
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',001),          X        
               ('LD_CHARQ',GOLMSG+3),(4,0)                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',002),          X        
               ('LD_CHARQ',GOLMSG+8),(L'GOLMSG-6,0)                             
         J     NXTFLD              CONTINUE READING TILL EOF                    
*                                                                               
CLOSE    GOTOR LINKIO,DMCB,('LIOACLO',LIOBD)                                    
         J     EXIT                                                             
*                                                                               
SETIN    NTR1                                                                   
         NI    4(R2),X'FF'-X'20'   UNSET PREVIOUSLY VALID                       
         MVI   5(R2),0             CLEAR INPUT FIELD LEN                        
*                                                                               
         LLC   RF,0(R2)            GET LEN OF FIELD IN TWA                      
         AHI   RF,-8               ADJUST FOR FLDHDR                            
         TM    1(R2),X'02'         TEST FOR EXTENDED FLDHDR                     
         JZ    *+8                                                              
         AHI   RF,-8                                                            
*                                                                               
         LA    RE,8(RF,R2)         POINT BEYOND INPUT DATA                      
         BCTR  RE,0                BACK UP TO LAST CHAR                         
*                                                                               
SETIN2   CLI   0(RE),C' '                                                       
         JH    SETIN4                                                           
         BCTR  RE,0                                                             
         JCT   RF,SETIN2                                                        
         J     EXITY               EXIT WITH INPUT LEN=0                        
*                                                                               
SETIN4   STC   RF,5(R2)            SET DATA LEN IN FLDHDR                       
*                                                                               
         LA    R1,8(R2)                                                         
*                                                                               
SETIN10  CLI   0(R1),C'A'                                                       
         JL    SETIN20                                                          
         CLI   0(R1),C'Z'                                                       
         JH    SETIN20                                                          
         LA    R1,1(R1)                                                         
         JCT   RF,SETIN10                                                       
         OI    4(R2),X'04'         SET FIELD IS VALID ALPHA                     
         J     SETIN30                                                          
*                                                                               
SETIN20  LA    R1,8(R2)                                                         
         LLC   RF,5(R2)                                                         
*                                                                               
SETIN22  CLI   0(R1),C'0'                                                       
         JL    SETIN30                                                          
         CLI   0(R1),C'9'                                                       
         JH    SETIN30                                                          
         LA    R1,1(R1)                                                         
         JCT   RF,SETIN22                                                       
         OI    4(R2),X'08'         SET FIELD IS VALID NUMERIC                   
*                                                                               
SETIN30  DS    0H                                                               
         J     EXITY                                                            
*                                                                               
FLDXC    XC    8(0,R2),8(R2)                                                    
*                                                                               
EXITY    CR    RB,RB               SET CC EQ                                    
         J     EXIT                                                             
*                                                                               
EXITN    LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
NO       LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
*                                                                               
YES      CR    RB,RB               SET CC EQ                                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CALL BASE PROGRAM TO PROCESS INPUT SCREEN                           *         
***********************************************************************         
                                                                                
GOBASE   NTR1  ,                                                                
*                                                                               
         GOTOR VCALLBAS,=C'*T20239*' CALL BASE TO PROCESS INPUT                 
*                                                                               
         CLC   =C'ES',GOLMSG       ERROR MESSAGES START WITH THIS               
         JE    *+14                                                             
         CLC   =C'EG',GOLMSG                                                    
         JNE   EXITY                                                            
* SEND REPLY MAP CODE AND THEN REPLY MSGNUM,MSGTEXT,LINNUM,FLDNUM               
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',I#SPGOLR)               
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',001),          X        
               ('LD_CHARQ',GOLMSG+3),(4,0)                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',002),          X        
               ('LD_CHARQ',GOLMSG+8),(L'GOLMSG-8,0)                             
*                                                                               
         OC    LINNUM,LINNUM       TEST ERROR IN HEADLINES                      
         JZ    EXITN               YES- NEXT RECORD                             
* SEND INPUT LINE NUMBER                                                        
         LH    R0,LINNUM                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(4),DUB                                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',003),          X        
               ('LD_CHARQ',FULL),(4,0)                                          
* AND FIELD NUMBER                                                              
         LLC   R0,FLDNUM                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(4),DUB                                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',004),          X        
               ('LD_CHARQ',FULL),(4,0)                                          
         J     EXITN               GO PROCESS NEXT RECORD                       
         EJECT                                                                  
CLRSCR   NTR1                                                                   
         LA    R2,GOLACT1H                                                      
         SR    R0,R0                                                            
*                                                                               
CLRSCR2  NI    4(R2),X'FF'-X'20'   UNSET PREVIOUSLY VALID                       
         MVI   5(R2),0             CLEAR INPUT FIELD LEN                        
*                                                                               
         LLC   RF,0(R2)            GET LEN OF FIELD IN TWA                      
         AHI   RF,-8               ADJUST FOR FLDHDR                            
         TM    1(R2),X'02'         TEST FOR EXTENDED FLDHDR                     
         JZ    *+8                                                              
         AHI   RF,-8                                                            
         BCTR  RF,0                                                             
         EX    RF,FLDXC                                                         
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    RE,GOLLAST                                                       
         CR    R2,RE                                                            
         JL    CLRSCR2                                                          
         J     EXIT                                                             
CLRXC    XC    8(0,R2),8(R2)                                                    
*                                                                               
         EJECT                                                                  
LKINIT   NTR1  BASE=*,LABEL=*      INITIALIZE FOR DDLINK REQUESTS               
*                                                                               
         XC    LIOBD(LIOBVIS-LIOBD),LIOBD                                       
         LA    RF,LIOBD+L'LIOB                                                  
         ST    RF,LIOBAREC                                                      
*                                                                               
         AHI   RF,2048             MAX SIZE FOR LINKIO REC USES                 
         ST    RF,LIOBABUF                                                      
*                                                                               
         LA    RF,T202FFD                                                       
         ST    RF,LIOBASB1         BLOCK FOR SCREEN                             
         ST    RC,LIOBASB2         SET SECOND BLOCK ADDRESS                     
*                                                                               
         LA    RF,LKUSMAP                                                       
         STCM  RF,15,LIOBAMAP      ADDRESS OF MAP CODES AND FIELDS              
*                                                                               
         MVC   LIOBACOM,VCOMFACS                                                
*                                                                               
         MVI   LIOBMSYS,2          SPOT MESSAGE SYSTEM                          
*                                                                               
LKIOI20  OI    LIOBINDS,LIOBIMLT+LIOBINRM                                       
         ICM   RF,15,LIOBACOM                                                   
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LIOAINI',LIOBD)                                      
         JE    *+6                                                              
         DC    H'0'                DUMP IF CANNOT INIT FOR NOW                  
*                                                                               
LKIOI_X  J     EXIT                                                             
*                                                                               
LKUSMAP  DS    0XL(LIORL)                                                       
         DC    AL2(I#SPGOLU,I#SPGOLU,GOLUPL-LKUSMAP)                            
LKUSMAPX DC    AL2(0)                                                           
*                                                                               
       ++INCLUDE SPMAPGOL                                                       
         LTORG                                                                  
*                                                                               
MYARRAY  DSECT                                                                  
MYACTN   DS    CL3                                                              
MYMKT    DS    CL4                                                              
MYDPT    DS    CL1                                                              
MYSLN    DS    CL3                                                              
MYDOLS   DS    CL11                                                             
MYPTS    DS    CL11                                                             
MYPERD   DS    CL13                                                             
MYARRAYX EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPGOLWRK                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
*                                                                               
LIOBD   DSECT                                                                   
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE DDLINKD                                                        
       ++INCLUDE SPMAPEQUS                                                      
       ++INCLUDE SPDDEQUS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070SPGOL39   10/11/17'                                      
         END                                                                    
